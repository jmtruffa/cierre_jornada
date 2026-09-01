#!/usr/bin/env bash
set -euo pipefail

RSCRIPT=/usr/bin/Rscript
CIERRE=/home/jmt/dev/r/outlier/cierre_jornada/cierre_jornada.r
LOG=/home/jmt/data/cron_cierre_1910.log

"$RSCRIPT" "$CIERRE" TRUE \
  cierre_caucion.R \
  cierre_evol_repo_mae.r \
  cierre_int_rofex.R \
  cierre_rofex_curva.R \
  cierre_precios_indiferencia.R \
  >> "$LOG" 2>&1 || echo "[$(date '+%F %T')] batch 1910 termino con exit code $?" >> "$LOG"

# Segundo intento de cierre_evol_repo_mae.r: la API de MAE puede fallar de forma
# intermitente. El script es idempotente (upsert ON CONFLICT (fecha) y regeneracion
# del grafico), y si el primer pase ya dejo la tabla al dia ni siquiera llama a la API.
"$RSCRIPT" "$CIERRE" TRUE \
  cierre_evol_repo_mae.r \
  >> "$LOG" 2>&1 || echo "[$(date '+%F %T')] retry repo_mae 1910 termino con exit code $?" >> "$LOG"
