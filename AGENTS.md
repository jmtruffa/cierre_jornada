# AGENTS.md — contexto para asistentes (cierre_jornada)

Este archivo resume hechos estables del repo para quien edite o depure código con un asistente. El detalle normativo sigue en **`README.md`**.

## Qué es el proyecto

Pipeline diario en **R** que genera el reporte de **cierre de jornada**: datos de mercado, gráficos en disco, informe HTML (`cierre_jornada.qmd` → `cierre_jornada.html`) y sincronización a **GCS** (`gs://reportes-cierre-jornada`).

Orquestador principal: **`cierre_jornada.r`**.

## Entorno de ejecución (servidor)

En código aparecen rutas absolutas típicas del job:

| Variable | Uso |
|----------|-----|
| `setwd` | `/home/jmt/dev/r/outlier/cierre_jornada` |
| `path` | Salida: `/home/jmt/cierre-jornada` (HTML, gráficos, `cierre.log`) |
| `path_source` | Código fuente: `/home/jmt/dev/r/outlier/cierre_jornada` |

En otra máquina hay que alinear `setwd`, `path` y `path_source`.

Setup: `functions::setup(server = "GC")`, `outlier::theme_outlier()`. Sub-scripts usan `server`, `port`, `cal` (calendario de negocio con feriados desde DB), `dbname` donde aplique.

## Cómo se invoca el pipeline

- **`Rscript cierre_jornada.r [true\|false] [script1 script2 …]`**  
  - Primer arg opcional: `update` (solo si es `true` o `false`).  
  - Si hay **dos o más** argumentos en total, `run_scripts` = todos salvo el primero → **solo** se `source`an esos basenames; el resto se omite (skip en log).

Los **cron** por horario usan este modo con lista acotada, por ejemplo:

- `run_cierre_1715.sh`, `run_cierre_1820.sh`, `run_cierre_1910.sh` → llaman a `cierre_jornada.r TRUE` + lista de archivos; salida append a logs bajo `/home/jmt/data/cron_cierre_*.log`.

## Comportamiento del orquestador

- **`safe_source`**: `source()` con timeout (600 s), errores no detienen el resto; trazas en `log_file`.
- **`log_file`**: `file.path(path, "cierre.log")`.
- **`grabaGrafo`**: envuelto con `suppressMessages` / `suppressWarnings`; los sub-scripts deben pasar **`path = path`** cuando graban gráficos con nombre explícito.
- Orden de `safe_source` en `cierre_jornada.r` **importa** (variables globales para QMD y scripts posteriores).

Orden relevante al final de la cadena de bonos / futuros / varios:

`… cierre_deuda_ponderada.r` → **`cierre_evol_repo_mae.r`** → `cierre_int_rofex.R` → `cierre_rofex_curva.R` → `cierre_precios_indiferencia.R` → render QMD → `gcloud storage rsync`.

## `cierre_evol_repo_mae.r` y tabla PostgreSQL `repo_a3`

- Fuente externa: API **MAE Market Data** (`api.marketdata.mae.com.ar`, endpoint repo `titulosfecha`); respuesta JSON con `details`.
- **Persistencia**: tabla **`repo_a3`** con columnas alineadas a lo que se extrae del API tras `bind_rows(details)` y `select`: **`fecha`** (PK), **`volumen`**, **`plazo`**, **`tpp`**, **`tppnbcra`** (en R: `tPP` / `tPPnBCRA` antes de mapear a SQL).
- No se guardan en tabla los campos derivados (participación BCRA, volúmenes estimados); se recalculan al leer con **`tasa_bcra_val`** del script para el gráfico.
- Flujo: `CREATE TABLE IF NOT EXISTS`, lectura incremental con solapamiento de días hábiles, **upsert** por `INSERT … ON CONFLICT (fecha) DO UPDATE`, luego lectura para plot y **`grabaGrafo(..., name = "g_evol_repo_bcra", path = path)`**.
- Logs con prefijo **`repo_a3:`** vía `functions::log_msg` en `cierre.log`.

Este script está en el cron **19:10** (`run_cierre_1910.sh`) además de integrarse en el pipeline completo cuando corre sin filtro `run_scripts`.

## Convenciones útiles

- Preferir **`functions::log_msg`** para mensajes estructurados al log del cierre.
- Scripts suelen asumir que fueron cargados **después** de `cierre_jornada.r` (librerías, `path`, `cal`, etc.).
- Detalle de tablas (`boncer_dinamica`, `fx`, `paridades_historicas_globales`, etc.): ver **`README.md`**.

## Qué no está en este archivo

- Lista completa de paquetes R y dependencias → README.
- Contenido del QMD y nombres de cada gráfico en el informe → revisar `cierre_jornada.qmd` y scripts individuales.
