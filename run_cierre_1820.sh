#!/usr/bin/env bash
set -euo pipefail

RSCRIPT=/usr/bin/Rscript
CIERRE=/home/jmt/dev/r/outlier/cierre_jornada/cierre_jornada.r
LOG=/home/jmt/data/cron_cierre_1820.log

"$RSCRIPT" "$CIERRE" TRUE \
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
  cierre_evol_repo_mae.r \
  >> "$LOG" 2>&1 || echo "[$(date '+%F %T')] batch 1820 termino con exit code $?" >> "$LOG"

# Segundo intento de cierre_evol_repo_mae.r: la API de MAE puede fallar de forma
# intermitente. El script es idempotente (upsert ON CONFLICT (fecha) y regeneracion
# del grafico), y si el primer pase ya dejo la tabla al dia ni siquiera llama a la API.
"$RSCRIPT" "$CIERRE" TRUE \
  cierre_evol_repo_mae.r \
  >> "$LOG" 2>&1 || echo "[$(date '+%F %T')] retry repo_mae 1820 termino con exit code $?" >> "$LOG"
