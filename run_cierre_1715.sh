#!/usr/bin/env bash
set -euo pipefail

/usr/bin/Rscript /home/jmt/dev/r/outlier/cierre_jornada/cierre_jornada.r TRUE \
  cierre_mulc.R \
  cierre_fx.R \
  cierre_reservas.R \
  cierre_lecaps_bonospesos.R \
  cierre_tamar.R \
  cierre_be_tamar.R \
  cierre_boncer.R \
  cierre_boncer_be.R \
  cierre_dl.R \
  cierre_inflacionBE.R \
  cierre_intradiario.R \
  cierre_spread_legislacion.R \
  cierre_riesgo_pais.R \
  cierre_soberanos.R \
  >> /home/jmt/data/cron_cierre_1715.log 2>&1
 
