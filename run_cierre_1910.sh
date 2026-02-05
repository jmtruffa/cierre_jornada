#!/usr/bin/env bash
set -euo pipefail

/usr/bin/Rscript /home/jmt/dev/r/outlier/cierre_jornada/cierre_jornada.r TRUE \
  cierre_caucion.R \
  cierre_int_rofex.R \
  cierre_rofex_curva.R \
  cierre_precios_indiferencia.R \
  >> /home/jmt/data/cron_cierre_1910.log 2>&1
