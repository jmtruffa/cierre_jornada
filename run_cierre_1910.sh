#!/usr/bin/env bash
set -euo pipefail

/usr/bin/Rscript /home/jmt/dev/r/outlier/cierre_jornada/cierre_jornada.r TRUE \
  SCRIPT_1.R \
  SCRIPT_2.R \
  SCRIPT_3.R \
  SCRIPT_4.R \
  >> /home/jmt/data/cron_cierre_1910.log 2>&1
