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

#--------
setup()
theme_outlier()
path = "/home/jmt/cierre-jornada"
cal = bizdays::create.calendar('cal', functions::getFeriados(server = server, port = port), weekdays = c('saturday','sunday'))
cal_usa = bizdays::create.calendar('cal_usa', functions::dbGetTable(table = "calendario_feriados_usa", server = server, port = port)$date, weekdays = c('saturday','sunday'))
# viernes toma el valor TRUE si el día es viernes
viernes = (wday(Sys.Date(), week_start = 1) == 5)
current_friday = Sys.Date() + days(5 - wday(Sys.Date(), week_start = 1))
prev_friday_date = adjust.previous(current_friday - days(7), cal)
from = as.Date(ifelse(viernes, prev_friday_date, adjust.previous(Sys.Date() - 1, cal = cal))) 
to = Sys.Date()
from_dinamica = "2025-01-01"
settlement = "A-24HS"
methodsPPI::getPPILogin() # crea el token de PPI que va a usar para todas las consultas.
comi = 0.000
fails = tibble(
  ticker = character()
)


#######################################################################
# MULC

#######################################################################
# FX
# lógica para chequear si son más de las 18:05 para que actualice el valor del ccl
# desde methodsPPI::getPPIDLR y actualicé la tabla de ccl
ba_now <- as.POSIXct(Sys.time(), tz = "America/Argentina/Buenos_Aires")
thresh <- as.POSIXct(strftime(ba_now, "%Y-%m-%d 18:10:00"),
                     tz = "America/Argentina/Buenos_Aires")

if (ba_now > thresh) {
  # vamos a tomar el valor del ccl y actualizar la ta bla
  query = "SELECT MAX(DATE) FROM ccl"
  from_tabla = dbExecuteQuery(query = query, server = server, port = port) %>% pull()
  df_to_save = methodsPPI::getPPIDLR(from = add.bizdays(from_tabla, 1, cal), to = Sys.Date(), settle = "t+0") %>% 
    select(date, ccl = cclAL, ccl3 = cclAL) %>% 
    # truncamos en lugar de redondear
    mutate(
      across(-date, ~ trunc(. * 100) / 100)
    )
  
  # grabamos
  functions::dbWriteDF(table = "ccl",
                       df = df_to_save, 
                       port = port,
                       server = server,
                       dbname = dbname,
                       append = T)
}

#######################################################################
# Lecaps
source('./cierre_lecaps_bonospesos.R')

#######################################################################
# tamar
source('./cierre_tamar.R')

#######################################################################
# boncer
source('./cierre_boncer.R')

#######################################################################
# 1. Comando completo de gsutil rsync
# La ruta gsutil suele ser /usr/bin/gsutil o está en el PATH
# Esto actualizará el bucket reportes-cierre-jornada con los archivos generados y 
# que están en la carpeta /cierre-jornada
gsutil_comando <- paste0("gsutil rsync -d -r ", path, "/", " gs://reportes-cierre-jornada")

# 2. Ejecutar el comando de shell
# La función system() ejecuta comandos de shell.
system(gsutil_comando)