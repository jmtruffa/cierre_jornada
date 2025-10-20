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



#--------
# capturamos el primer argumento del llamado para ver si corre actualizando la base
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  update <- FALSE
} else if (tolower(args[1]) %in% c("true", "false")) {
  update <- as.logical(tolower(args[1]))
} else {
  update <- FALSE
}
functions::setup()
outlier::theme_outlier()
path = "/home/jmt/cierre-jornada"
path_source = "/home/jmt/dev/r/outlier/cierre_jornada"
cal = bizdays::create.calendar('cal', functions::getFeriados(server = server, port = port), weekdays = c('saturday','sunday'))
cal_usa = bizdays::create.calendar('cal_usa', functions::dbGetTable(table = "calendario_feriados_usa", server = server, port = port)$date, weekdays = c('saturday','sunday'))
# viernes toma el valor TRUE si el día es viernes
viernes = (lubridate::wday(Sys.Date(), week_start = 1) == 5)
current_friday = Sys.Date() + days(5 - lubridate::wday(Sys.Date(), week_start = 1))
prev_friday_date = bizdays::adjust.previous(current_friday - days(7), cal)
from = as.Date(ifelse(viernes, prev_friday_date, bizdays::adjust.previous(Sys.Date() - 1, cal = cal))) 
to = Sys.Date()
from_dinamica = "2025-01-01"
from_fx = "2016-01-01"
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
# MULC
source(file.path(path_source, "cierre_mulc.R"))

#######################################################################
# FX

source(file.path(path_source, "cierre_fx.R"))


#######################################################################
# Lecaps
source('./cierre_lecaps_bonospesos.R')

#######################################################################
# tamar
source('./cierre_tamar.R')
source('./cierre_be_tamar.R')

#######################################################################
# boncer
source('./cierre_boncer.R')
source('./cierre_boncer_be.R')

#######################################################################
# caucion
source('./cierre_caucion.R')

#######################################################################
# Carry con lecaps histórico
source('./cierre_lecaps_carry.R')

#######################################################################
# inflación Break Even
source('./cierrre_inflacionBE.R')

#######################################################################
# Internacionales
source('./cierre_commodities.R')
source('./cierre_etf_comparables.R')
source('./cierre_monedas.R')
source('./cierre_indices.R')
source('./cierre_dxy_tnx.R')
source('./cierre_adrs.R')
source('./cierre_merval.R')

#######################################################################
# Agregados
source('./cierre_depositos_gob.R')
source('./cierre_depo_dolar.R')
source('./cierre_tasas_adelantos.R')

#######################################################################
# Bonos
source('./cierre_intradiario.R')
source('./cierre_spread_legislacion.R')
source('./cierre_riesgo_pais.R')
source('./cierre_soberanos.R')

#######################################################################
# Futuros
source('./cierre_int_rofex.R')
source('./cierre_rofex_curva.R')

#######################################################################
# Varios
source('./cierre_precios_indiferencia.R')
rmarkdown::render(
  input = "./cierre_jornada.qmd",
  output_file = "/home/jmt/cierre-jornada/cierre_jornada.html",  # ruta completa deseada
  envir = .GlobalEnv
)


#######################################################################
# 1. Comando completo de gsutil rsync
# La ruta gsutil suele ser /usr/bin/gsutil o está en el PATH
# Esto actualizará el bucket reportes-cierre-jornada con los archivos generados y 
# que están en la carpeta /cierre-jornada
gsutil_comando <- paste0("gsutil rsync -d -r ", path, "/", " gs://reportes-cierre-jornada")

# 2. Ejecutar el comando de shell
# La función system() ejecuta comandos de shell.
system(gsutil_comando)