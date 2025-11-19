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

# vamos a llamar cada script con esta función para evitar que si una falle, se caiga el resto
safe_source <- function(file) {
  tryCatch(
    {
      message("Ejecutando: ", basename(file))
      source(file)
    },
    error = function(e) {
      msg <- sprintf("ERROR en %s: %s", basename(file), conditionMessage(e))
      message(msg)
      cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), msg, "\n",
          file = log_file, append = TRUE)
    }
  )
}

# capturamos el primer argumento del llamado para ver si corre actualizando la base
# args = commandArgs(trailingOnly = TRUE)
# if (length(args) == 0) {
#   update <- FALSE
# } else if (tolower(args[1]) %in% c("true", "false")) {
#   update <- as.logical(tolower(args[1]))
# } else {
#   update <- FALSE
# }
setwd("/home/jmt/dev/r/outlier/cierre_jornada")
functions::setup(server = "GC")
outlier::theme_outlier()
path = "/home/jmt/cierre-jornada"
path_source = "/home/jmt/dev/r/outlier/cierre_jornada"
update = T
##############################
# Backup y limpieza del directorio
backup_path <- file.path(path, "backup")
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

file.copy(from = files, to = backup_today, overwrite = TRUE)

# Limpieza de backups viejos
backups <- list.dirs(backup_path, recursive = FALSE, full.names = TRUE)
old_backups <- backups[as.Date(basename(backups), "%Y%m%d") < Sys.Date() - 7]
unlink(old_backups, recursive = TRUE)

# Eliminar los archivos originales
file.remove(files)
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
log_file = file.path(path, "cierre.log")


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
safe_source(file.path(path_source, 'cierrre_inflacionBE.R'))

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

rmarkdown::render(
  input = file.path(path_source, "cierre_jornada.qmd"),
  output_file = file.path(path, "cierre_jornada.html"),
  envir = .GlobalEnv
)


#######################################################################
# 1. Comando completo de gsutil rsync
# La ruta gsutil suele ser /usr/bin/gsutil o está en el PATH
# Esto actualizará el bucket reportes-cierre-jornada con los archivos generados y 
# que están en la carpeta /cierre-jornada
gsutil_comando <- paste0(
  "/usr/bin/gsutil rsync -d -r ",
  path, "/", 
  " gs://reportes-cierre-jornada")

# 2. Ejecutar el comando de shell
# La función system() ejecuta comandos de shell.
system(gsutil_comando)
