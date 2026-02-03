#!/usr/bin/env bash
set -euo pipefail

/usr/bin/Rscript /home/jmt/dev/r/outlier/cierre_jornada/cierre_jornada.r TRUE \
  cierre_commodities.R \
  cierre_etf_comparables.R \
  cierre_monedas.R \
  cierre_indices.R \
  cierre_dxy_tnx.R \
  cierre_adrs.R \
  cierre_merval.R \
  cierre_depositos_gob.R \
  cierre_depo_dolar.R \
  cierre_tasas_adelantos.R \
  >> /home/jmt/data/cron_cierre_1820.log 2>&1
