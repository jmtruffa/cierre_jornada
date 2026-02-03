suppressPackageStartupMessages({
  library(bizdays)
  library(tidyverse)
  library(functions)
  library(bcra)
  library(finance)
  library(outlier)
  library(methodsPPI)
  library(bdscale)
  library(scales)
  library(ggthemes)
  library(ggrepel)
  library(flextable)
  library(slider) # lo uso en cierre_fx para la volatilidad
  library(jsonlite) # la uso en cierre_boncer_be
  library(zoo)
  library(tidyquant)
  library(purrr)
  require(httr2)
  library(patchwork)
  library(gghighlight)
  library(rofex)
  library(officer)
  library(R.utils)
})

# vamos a llamar cada script con esta función para evitar que si una falle, se caiga el resto
safe_source <- function(file, timeout = 600) {
  tryCatch(
    {
      if (!is.null(run_scripts) && !basename(file) %in% run_scripts) {
        msg_skip <- sprintf("[%s] Skip (no incluido en argumentos): %s",
                            format(Sys.time(), "%F %T"), basename(file))
        message(msg_skip)
        cat(msg_skip, "\n", file = log_file, append = TRUE)
        return(invisible(NULL))
      }
      msg_ini <- sprintf("[%s] Ejecutando: %s", format(Sys.time(), "%F %T"), basename(file))
      message(msg_ini)
      cat(msg_ini, "\n", file = log_file, append = TRUE)
      
      withTimeout(
        expr     = source(file),
        timeout  = timeout,
        onTimeout = "error"
      )
      
      msg_fin <- sprintf("[%s] OK: %s", format(Sys.time(), "%F %T"), basename(file))
      cat(msg_fin, "\n", file = log_file, append = TRUE)
    },
    error = function(e) {
      msg <- sprintf("[%s] ERROR en %s: %s",
                     format(Sys.time(), "%F %T"),
                     basename(file),
                     conditionMessage(e))
      message(msg)
      cat(msg, "\n", file = log_file, append = TRUE)
    }
  )
}

safe_render <- function(...) {
  tryCatch(
    {
      msg_ini <- sprintf("[%s] Renderizando cierre_jornada.qmd...", format(Sys.time(), "%F %T"))
      message(msg_ini)
      cat(msg_ini, "\n", file = log_file, append = TRUE)
      
      rmarkdown::render(...)
      
      msg_fin <- sprintf("[%s] Render OK", format(Sys.time(), "%F %T"))
      cat(msg_fin, "\n", file = log_file, append = TRUE)
    },
    error = function(e) {
      msg <- sprintf("[%s] ERROR en render cierre_jornada.qmd: %s",
                     format(Sys.time(), "%F %T"),
                     conditionMessage(e))
      message(msg)
      cat(msg, "\n", file = log_file, append = TRUE)
    }
  )
}

# capturamos el primer argumento del llamado para ver si corre actualizando la base
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  update <- FALSE
} else if (tolower(args[1]) %in% c("true", "false")) {
  update <- as.logical(tolower(args[1]))
} else {
  update <- FALSE
}
run_scripts <- NULL
if (length(args) >= 2) {
  run_scripts <- args[-1]
  if (length(run_scripts) == 0) run_scripts <- NULL
}
setwd("/home/jmt/dev/r/outlier/cierre_jornada")
functions::setup(server = "GC")
suppressWarnings(invisible(outlier::theme_outlier()))
path = "/home/jmt/cierre-jornada"
path_source = "/home/jmt/dev/r/outlier/cierre_jornada"
update = F
log_file = file.path(path, "cierre.log")

# Silenciar mensajes/advertencias en todos los gráficos guardados
grabaGrafo_base <- grabaGrafo
grabaGrafo <- function(...) {
  suppressMessages(
    suppressWarnings(
      grabaGrafo_base(...)
    )
  )
}

functions::log_msg(
  paste("========================="),
  log_file = file.path(path, "cierre.log")
)
functions::log_msg(
  paste("Arranca proceso generador de cierre a las: ", Sys.time()),  
  log_file = file.path(path, "cierre.log")
)
if (!is.null(run_scripts)) {
  msg_scripts <- sprintf("[%s] run_scripts: %s",
                         format(Sys.time(), "%F %T"),
                         paste(run_scripts, collapse = ", "))
  message(msg_scripts)
  cat(msg_scripts, "\n", file = log_file, append = TRUE)
}
##############################
# Backup y limpieza del directorio
backup_path <- "/home/jmt/backup-cierre-jornada"
# Crear carpeta backup si no existe
if (!dir.exists(backup_path)) dir.create(backup_path)

# Listar archivos a respaldar
files <- list.files(path, full.names = TRUE)
info <- file.info(files) # buscamos todo lo que no es dir para que se pueda mantener backup
files <- files[!info$isdir] 

# Copiar archivos al backup
today <- format(Sys.Date(), "%Y%m%d")
backup_today <- file.path(backup_path, today)
if (!dir.exists(backup_today)) dir.create(backup_today)

functions::log_msg(
  paste("Moviendo archivos previos a carpeta backup"),  
  log_file = file.path(path, "cierre.log")
)
invisible(file.copy(from = files, to = backup_today, overwrite = TRUE))


# Limpieza de backups viejos
backups <- list.dirs(backup_path, recursive = FALSE, full.names = TRUE)
old_backups <- backups[as.Date(basename(backups), "%Y%m%d") < Sys.Date() - 7]
unlink(old_backups, recursive = TRUE)

# Eliminar los archivos originales
functions::log_msg(
  paste("Eliminando archivos anteriores"),  
  log_file = file.path(path, "cierre.log")
)
invisible(file.remove(files))
#############################



cal = bizdays::create.calendar('cal', functions::getFeriados(server = server, port = port), weekdays = c('saturday','sunday'))
cal_usa = bizdays::create.calendar('cal_usa', functions::dbGetTable(table = "calendario_feriados_usa", server = server, port = port)$date, weekdays = c('saturday','sunday'))
# viernes toma el valor TRUE si el día es viernes
viernes = (lubridate::wday(Sys.Date(), week_start = 1) == 5)
current_friday = Sys.Date() + days(5 - lubridate::wday(Sys.Date(), week_start = 1))
prev_friday_date = bizdays::adjust.previous(current_friday - days(7), cal)
from = as.Date(ifelse(viernes, prev_friday_date, bizdays::adjust.previous(Sys.Date() - 1, cal = cal))) 
to = Sys.Date()
from_dinamica = "2025-01-01"
from_fx = "2023-01-01"
start_date_inflabe = Sys.Date()  
end_date_inflabe = Sys.Date() 
start_date_inflabe_graph = "2025-01-01"
settlement = "A-24HS"
settle = "t+0" 
methodsPPI::getPPILogin() # crea el token de PPI que va a usar para todas las consultas.
comi = 0.000
fails = tibble(
  ticker = character()
)


#######################################################################
# Reservas
safe_source(file.path(path_source, "cierre_reservas.R"))

#######################################################################
# MULC
safe_source(file.path(path_source, "cierre_mulc.R"))

#######################################################################
# FX

safe_source(file.path(path_source, "cierre_fx.R"))


#######################################################################
# Lecaps
safe_source(file.path(path_source, 'cierre_lecaps_bonospesos.R'))

#######################################################################
# tamar
safe_source(file.path(path_source, 'cierre_tamar.R'))
safe_source(file.path(path_source, 'cierre_be_tamar.R'))

#######################################################################
# boncer
safe_source(file.path(path_source, 'cierre_boncer.R'))
safe_source(file.path(path_source, 'cierre_boncer_be.R'))

#######################################################################
# Linkers
safe_source(file.path(path_source, 'cierre_dl.R'))

#######################################################################
# caucion
safe_source(file.path(path_source, 'cierre_caucion.R'))

#######################################################################
# Carry con lecaps histórico
safe_source(file.path(path_source, 'cierre_lecaps_carry.R'))

#######################################################################
# inflación Break Even
safe_source(file.path(path_source, 'cierre_inflacionBE.R'))

#######################################################################
# Internacionales
safe_source(file.path(path_source, 'cierre_commodities.R'))
safe_source(file.path(path_source, 'cierre_etf_comparables.R'))
safe_source(file.path(path_source, 'cierre_monedas.R'))
safe_source(file.path(path_source, 'cierre_indices.R'))
safe_source(file.path(path_source, 'cierre_dxy_tnx.R'))
safe_source(file.path(path_source, 'cierre_adrs.R'))
safe_source(file.path(path_source, 'cierre_merval.R'))

#######################################################################
# Agregados
safe_source(file.path(path_source, 'cierre_depositos_gob.R'))
safe_source(file.path(path_source, 'cierre_depo_dolar.R'))
safe_source(file.path(path_source, 'cierre_tasas_adelantos.R'))

#######################################################################
# Bonos
safe_source(file.path(path_source, 'cierre_intradiario.R'))
safe_source(file.path(path_source, 'cierre_spread_legislacion.R'))
safe_source(file.path(path_source, 'cierre_riesgo_pais.R'))
safe_source(file.path(path_source, 'cierre_soberanos.R'))

#######################################################################
# Futuros
safe_source(file.path(path_source, 'cierre_int_rofex.R'))
safe_source(file.path(path_source, 'cierre_rofex_curva.R'))

#######################################################################
# Varios
safe_source(file.path(path_source, 'cierre_precios_indiferencia.R'))

# Render del HTML
safe_render(
  input       = file.path(path_source, "cierre_jornada.qmd"),
  output_file = file.path(path, "cierre_jornada.html"),
  envir       = .GlobalEnv
)



#######################################################################
# 1. Comando completo de gsutil rsync
# La ruta gsutil suele ser /usr/bin/gsutil o está en el PATH
# Esto actualizará el bucket reportes-cierre-jornada con los archivos generados y 
# que están en la carpeta /cierre-jornada
gsutil_args <- c(
  "-m",
  "rsync",
  "-d",
  "-r",
  paste0(path, "/"),
  "gs://reportes-cierre-jornada"
)

run_gsutil_sync <- function() {
  file_count <- length(list.files(path, recursive = TRUE))
  cat(sprintf("[%s] Archivos para sync: %s", format(Sys.time(), "%F %T"), file_count),
      "\n", file = log_file, append = TRUE)
  if (file_count > 0L) {
    msg <- sprintf("[%s] Lanzando gsutil rsync...", format(Sys.time(), "%F %T"))
    cat(msg, "\n", file = log_file, append = TRUE)
    gsutil_out <- tryCatch(
      system2("/usr/bin/gsutil", gsutil_args, stdout = TRUE, stderr = TRUE),
      error = function(e) paste("ERROR al ejecutar gsutil:", conditionMessage(e))
    )
    gsutil_status <- attr(gsutil_out, "status")
    if (is.null(gsutil_status)) {
      gsutil_status <- 0L
    }
    cat(sprintf("[%s] gsutil exit code: %s", format(Sys.time(), "%F %T"), gsutil_status),
        "\n", file = log_file, append = TRUE)
    if (length(gsutil_out) > 0L) {
      cat(gsutil_out, sep = "\n", file = log_file, append = TRUE)
      cat("\n", file = log_file, append = TRUE)
    }
  }
}


run_gsutil_sync()

functions::log_msg(
  paste("Finaliza proceso generador de cierre a las: ", Sys.time()),  
  log_file = file.path(path, "cierre.log")
)
functions::log_msg(
  paste("========================="),
  log_file = file.path(path, "cierre.log")
)