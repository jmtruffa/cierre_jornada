# Cierre de jornada

## Descripción

Pipeline diario en R que genera el **reporte de cierre de jornada**: descarga y procesa datos de mercado (reservas, MULC, FX, bonos, curvas, internacionales, etc.), escribe gráficos y artefactos en un directorio de salida, renderiza un informe HTML (`cierre_jornada.qmd` → `cierre_jornada.html`) y sincroniza esa carpeta con Google Cloud Storage.

El orquestador principal es **`cierre_jornada.r`**, que carga paquetes, define helpers (`safe_source`, `safe_render`, `safe_ppi_login`), ejecuta los scripts de cierre en **orden fijo** y al final corre el render y `gcloud storage rsync`.

---

## Punto de entrada: `cierre_jornada.r`

| Aspecto | Comportamiento en código |
|--------|---------------------------|
| **Directorio de trabajo** | `setwd("/home/jmt/dev/r/outlier/cierre_jornada")` — rutas absolutas del entorno donde corre el job; en otra máquina hay que alinear `setwd`, `path` y `path_source`. |
| **Setup** | `functions::setup(server = "GC")` y `outlier::theme_outlier()`. |
| **Rutas** | `path` = salida (`"/home/jmt/cierre-jornada"`); `path_source` = código fuente de los scripts (`"/home/jmt/dev/r/outlier/cierre_jornada"`). |
| **`safe_source`** | Envuelve `source()` con `R.utils::withTimeout` (default 600 s), registra inicio/éxito/error en `log_file`, y si existe el vector global `run_scripts`, **omite** los archivos que no estén en esa lista. Los errores no detienen el resto del pipeline. |
| **`run_scripts`** | Si hay **al menos dos** argumentos, `run_scripts <- args[-1]` (todos salvo el primero). Solo se ejecutan esos `basename`. El **primer** argumento sigue siendo el flag `update` solo si es `"true"` o `"false"`; si no lo es, `update` queda en `FALSE` pero igual puede haber `run_scripts` si pasás más de un argumento. En la práctica conviene: `Rscript cierre_jornada.r false archivo.R …` para ejecutar solo esos scripts. |
| **Argumentos CLI** | `args <- commandArgs(trailingOnly = TRUE)`. El **primer** argumento, si es `"true"` o `"false"` (cualquier capitalización), define `update` como lógico; si no hay args o no coincide, `update <- FALSE`. |
| **`update`** | Variable global usada por varios sub-scripts (p. ej. `cierre_boncer.R`, `cierre_tamar.R`, `cierre_dl.R`, `cierre_inflacionBE.R`, `cierre_intradiario.R`, `cierre_soberanos.R`, `cierre_lecaps_bonospesos.R`) para ramas que actualizan o releen datos. |
| **`from_dinamica`** | Fecha de corte global (`"2025-01-01"` en código): acota el eje X en gráficos dinámicos que leen tablas con histórico más largo (p. ej. curva LECAPS dinámica, BONCER dinámica en otros scripts). No limita el bootstrap/backfill/incremental de `curva_lecaps_dinamica` respecto de `historico_lecaps`. |
| **Logging** | `log_file <- file.path(path, "cierre.log")`; mensajes con `functions::log_msg`, `message`/`cat` y trazas de `safe_source` / `safe_render` / `gcloud`. |
| **Backup** | Antes de generar salidas nuevas: copia a `backup_path <- "/home/jmt/backup-cierre-jornada/<YYYYMMDD>` los archivos **del día anterior o más viejos** en `path`, borra backups con fecha &lt; hoy−7 días, luego elimina esos archivos del directorio de trabajo. |
| **Gráficos** | Se redefine `grabaGrafo` para envolver la versión base con `suppressMessages` y `suppressWarnings`. |
| **Render** | `safe_render()` llama `rmarkdown::render()` sobre `cierre_jornada.qmd` con `output_file` en `path/cierre_jornada.html` y `envir = .GlobalEnv`. |
| **Sync GCS** | `run_gcloud_storage_sync()` ejecuta `system2("/usr/bin/gcloud", ...)` con `storage rsync` de `path` hacia `gs://reportes-cierre-jornada`, recursivo y con borrado en destino de objetos no presentes en origen. |

---

## Orden de los sub-scripts (`safe_source`)

El orden importa: variables globales (`path`, `server`, `port`, `cal`, fechas, `fails`, etc.) se van poblando para el QMD y scripts posteriores.

1. **Reservas / MULC / FX** — `cierre_reservas.R`, `cierre_mulc.R`, `cierre_fx.R`
2. **Lecaps** — `cierre_lecaps_bonospesos.R`
3. **Tamar** — `cierre_tamar.R`, `cierre_be_tamar.R`
4. **Boncer** — `cierre_boncer.R`, `cierre_boncer_be.R`, `nelson_siegel.r`
5. **Linkers** — `cierre_dl.R`
6. **Caución / carry / inflación BE** — `cierre_caucion.R`, `cierre_lecaps_carry.R`, `cierre_inflacionBE.R`
7. **Internacionales** — `cierre_commodities.R`, `cierre_etf_comparables.R`, `cierre_monedas.R`, `cierre_indices.R`, `cierre_dxy_tnx.R`, `cierre_adrs.R`, `cierre_panel_etfs.R`, `cierre_merval.R`
8. **Agregados** — `cierre_depositos_gob.R`, `cierre_depo_dolar.R`, `cierre_tasas_adelantos.R`
9. **Bonos** — `cierre_intradiario.R`, `cierre_spread_legislacion.R`, `cierre_riesgo_pais.R`, `cierre_soberanos.R`, `cierre_deuda_ponderada.r`
10. **Futuros** — `cierre_int_rofex.R`, `cierre_rofex_curva.R`
11. **Varios** — `cierre_precios_indiferencia.R`

Luego: **render** del QMD y **sync** a GCS.

**Fuera del pipeline principal:** `migrar_tablas.R` no aparece en `cierre_jornada.r` (utilidad aparte).

---

## Dependencias

### Paquetes R (`library` / `require` en `cierre_jornada.r`)

`bizdays`, `tidyverse`, `functions`, `bcra`, `finance`, `outlier`, `methodsPPI`, `bdscale`, `scales`, `ggthemes`, `ggrepel`, `flextable`, `slider`, `jsonlite`, `zoo`, `tidyquant`, `purrr`, `httr2`, `patchwork`, `gghighlight`, `rofex`, `officer`, `R.utils`.

Además, el render usa **`rmarkdown`** vía `rmarkdown::render()` sin `library(rmarkdown)` explícito.

### Servicios y datos externos

- **Base de datos:** `functions::setup`, `dbGetTable`, `dbExecuteQuery`, `dbWriteDF`, etc. (p. ej. feriados USA, `precios_bonos_cer`, **`boncer_dinamica`**, **`paridades_historicas_globales`** ([cierre_deuda_ponderada.r](cierre_deuda_ponderada.r)), **`curva_lecaps_dinamica`** ([cierre_lecaps_bonospesos.R](cierre_lecaps_bonospesos.R)) incremental desde `historico_lecaps`, tabla **`fx`** ([cierre_fx.R](cierre_fx.R)) incremental de tipos de cambio; **`nelson_siegel.r`** solo lee `boncer_dinamica`, sin `.rds`).
- **PPI:** `methodsPPI::getPPILogin()` en `safe_ppi_login()`; si falla, se registra el error y el proceso **continúa** (`ppi_login_ok` no corta el flujo en el orquestador).
- **Google Cloud:** CLI `gcloud` en `/usr/bin/gcloud` para `storage rsync` al bucket `gs://reportes-cierre-jornada`.

---

## Salidas principales

| Ubicación | Contenido típico |
|-----------|-------------------|
| **`path`** (`/home/jmt/cierre-jornada` en el código) | HTML del reporte, gráficos generados por `grabaGrafo` (envuelto en `cierre_jornada.r`), logs, RDS intermedios según cada script (la serie dinámica BONCER **no** se guarda en `.rds`; vive solo en la tabla `boncer_dinamica`). |
| **`cierre_jornada.html`** | Informe renderizado desde `cierre_jornada.qmd`. |
| **`cierre.log`** | Traza del proceso, errores por script, sync gcloud. |
| **Gráficos** | Los sub-scripts llaman a `grabaGrafo` (helper `functions` / `outlier`) escribiendo en `path`. |
| **Backup** | `backup_path`: copias por fecha bajo `/home/jmt/backup-cierre-jornada/<YYYYMMDD>`. |

---

## Tabla `boncer_dinamica` y `nelson_siegel.r`

- **`cierre_boncer.R`** mantiene la serie dinámica de BONCER en PostgreSQL en la tabla **`boncer_dinamica`**: `CREATE TABLE IF NOT EXISTS` y clave primaria **`(date, ticker)`**. El flujo es **incremental** respecto a `max(date)` en esa tabla:
  - **Tabla vacía o sin máximo válido:** **bootstrap** — se leen precios de `precios_bonos_cer` con `date >= from_dinamica`, se calculan yields (y columnas asociadas) para ese rango y se insertan con `functions::dbWriteDF` (`append = TRUE`).
  - **Tabla ya poblada:** **incremental** — solo se leen filas de `precios_bonos_cer` con **`date > max(date)`** de `boncer_dinamica`, se enriquecen y se hace **append** igual que arriba.
  - Para gráficos en el mismo script, la serie en memoria se arma con `SELECT * FROM boncer_dinamica WHERE date >= from_dinamica` (histórico acumulado en tabla).
- **Archivo `boncer_dinamica.rds`:** ya **no** se escribe ni se lee; la persistencia y el consumo posterior pasan **solo** por la tabla y consultas SQL.
- **`nelson_siegel.r`** corre **después** de `cierre_boncer.R` y `cierre_boncer_be.R` (orden fijado en `cierre_jornada.r`). Carga datos **únicamente** con `SELECT * FROM boncer_dinamica WHERE date >= from_dinamica` (sin lectura de RDS ni otro fallback). Si la consulta falla o no hay filas, registra advertencia en `cierre.log` y **omite** el resto; si hay pocas filas válidas tras filtros, también puede omitir con otro mensaje. Con datos suficientes, ajusta curvas reales CER con Nelson–Siegel y genera gráficos con `grabaGrafo` (mismo helper que el resto del cierre, envuelto en `cierre_jornada.r`).

## Tabla `paridades_historicas_globales` ([`cierre_deuda_ponderada.r`](cierre_deuda_ponderada.r))

- Persiste el resultado de `getPPIPriceHistoryMultiple3` + `getYields` (precio y paridad por bono global) con clave **`(date, ticker)`**.
- **Bootstrap** si la tabla está vacía: se pide la API desde `from` (`2020-09-01`) hasta `to` (`Sys.Date()`), se calculan paridades para todo el lote y se hace `append` con `dbWriteDF`.
- **Incremental** si ya hay datos: `max(date)` en la tabla; si `max(date) + 1 <= to`, se pide solo ese rango a la API, se enriquece y se append; si la tabla ya está al día, no se llama a la API.
- El análisis (ponderación y gráficos) sigue leyendo un **`SELECT * ... WHERE date >= from`** sobre la tabla, no el histórico completo recalculado en memoria cada vez.

## Tabla `curva_lecaps_dinamica` ([`cierre_lecaps_bonospesos.R`](cierre_lecaps_bonospesos.R))

Sección **3) LECAPS DINÁMICA**: resultado de `finance::tasasLecap` sobre precios de **`historico_lecaps`**, persistido con cobertura alineada al histórico de precios (el corte `from_dinamica` **no** limita qué fechas se insertan). Clave primaria **`(date, ticker)`**.

- **DDL y migración:** `ensure_curva_dinamica_table()` crea la tabla con `CREATE TABLE IF NOT EXISTS` e incluye la columna **`tasa`** (junto a `price`, etc.). Para bases ya existentes, se ejecuta **`ALTER TABLE curva_lecaps_dinamica ADD COLUMN IF NOT EXISTS tasa double precision`** para añadir `tasa` sin recrear la tabla.
- **Columnas persistidas:** además de `tasa`, las habituales de `tasasLecap`: `date`, `ticker`, `price`, `vf`, `date_vto`, `date_liq`, `settle`, `dias360`, `dias`, `tdirecta`, `tna`, `tea`, `tem`, `tna360`, `tea360`, `tem360`, `duration`, `mduration`. **`group`** no se persiste: `curva_dinamica_persist_cols()` la elimina si aparece.
- **Bootstrap** (`curva_lecaps_dinamica` vacía o sin `max(date)` válido): se lee **`historico_lecaps` completo** (`SELECT date, ticker, price … ORDER BY date, ticker`), **sin** filtrar por `from_dinamica`; luego `curva_dinamica_append_from_precios(..., "bootstrap")` → `tasasLecap` → `dbWriteDF`. Si `tasasLecap` devuelve 0 filas, no hay inserción.
- **Backfill** (tabla ya poblada pero `min(date)` en `curva_lecaps_dinamica` **>** `min(date)` en `historico_lecaps`): se insertan precios con `date` entre el mínimo del histórico y el mínimo ya guardado en curva (hueco inicial), vía `curva_dinamica_append_from_precios(..., "backfill")`.
- **Incremental:** si `max(historico) > max(curva)`, se leen filas con **`date > max(curva)`** en `historico_lecaps` (tampoco filtradas por `from_dinamica`); etapa `"incremental"`.
- **Helpers:** `curva_dinamica_pull_date()` extrae fechas min/max seguras desde consultas SQL; `curva_dinamica_append_from_precios()` centraliza `tasasLecap`, filtrado de columnas y escritura con logging por etapa.
- **Lectura para gráficos:** `SELECT * FROM curva_lecaps_dinamica ORDER BY date, ticker` (tabla **completa** en memoria). Los gráficos dinámicos TEM/TNA aplican **`filter(..., date >= as.Date(from_dinamica))`** solo para acotar el eje X (ya no se usa una fecha fija tipo `"2025-01-01"` dentro del gráfico: depende de la variable global del orquestador).

## Inflación BE ([`cierre_inflacionBE.R`](cierre_inflacionBE.R))

Tras cargar `db_infla_be`, se calcula **`dias_hasta_real = as.numeric(fechas_tasa_nominal - fecha)`** (días hasta el vencimiento nominal de la tasa asociada a cada fila).

Los gráficos dinámicos **`g_inflabe_dinamica`** y **`g_inflabe_dinamica_tea`**:

- Acotan además por **`fecha >= "2025-01-01"`** (fecha mínima explícita en el script, independiente de `from_dinamica`).
- Filtran con **`dias_hasta_real <= DIAS_MAX_A_GRAFICAR`** (`DIAS_MAX_A_GRAFICAR <- 180`): solo filas donde el plazo **`fechas_tasa_nominal - fecha`** es **como máximo 180 días** (tronco corto de la curva respecto del nominal).
- Construyen el factor **`mes`** con niveles en **orden cronológico** según `fechas_tasa_nominal` (`arrange` + `unique` de etiquetas).
- Las etiquetas (`ggrepel`) se muestran solo en **`fecha == max_fecha_global`** (último día del rango graficado).
- El eje X usa **`scale_x_date(limits = range(fecha) + c(0, 40))`**: 40 días de margen a la derecha para que las etiquetas no queden cortadas.

## ADRs y calendarios ([`cierre_adrs.R`](cierre_adrs.R))

El panel de variaciones (`finance::panel_variaciones_generico`) recibe un vector **`calendarios`** cuyos nombres deben coincidir con calendarios **registrados en `bizdays`** en `cierre_jornada.r`, no con nombres de tablas en la base. En código: **`cal_usa`** para la mayoría de tickers (incl. índices/ETF) y **`cal`** para **Merval CCL**. Antes, nombres tomados de tablas DB hacían fallback al calendario argentino para todos los símbolos y distorsionaban el **retorno 1D** en días de feriado local (p. ej. 2 de abril) para activos que deberían seguir el calendario USA.

## Tabla `fx` ([`cierre_fx.R`](cierre_fx.R))

- **Fuente de verdad** en PostgreSQL para un snapshot diario de tipos de cambio y ratios: clave primaria **`date`**, columnas `ccl`, `mepal`, `mepgd`, `cclgd`, `a3500`, `canje` (= `ccl / mepAL`), `brecha` (= `ccl / A3500`).
- **Flujo:** se lee la tabla al inicio de la sección FX. Por corrida hay **una sola** llamada a `getPPIDLR`: el `from` es el mínimo entre `from_fx`, el inicio del hueco incremental en `fx` (con solape de 5 días hábiles para `lag` / `lag(..., 5)`) y, si aplica el umbral horario, el día siguiente al `max(date)` de `ccl`. De ese resultado se derivan el append a `ccl`, el pipeline incremental de `fx` y la serie principal (`dlr` filtrado a `date >= from_fx`). Se filtran `ccl` en DB, `A3500` / `forex`, etc., como antes.
- **Persistencia:** `append` con `dbWriteDF` solo de filas nuevas; luego se **relee** la tabla. La serie mostrada en gráficos y `set`/`tabla_fx` usa el mismo lote `getPPIDLR` (filtrado a `from_fx`–`to`) con **`rows_patch`** desde `fx` para `ccl`, MEP, CCLGD y `A3500` cuando existen en la tabla (prioridad a lo persistido).

---

## Ejecución (referencia)

```bash
# Ejecución completa, sin modo update explícito (update=FALSE si no se pasa true/false)
Rscript cierre_jornada.r

# Primer argumento: actualizar bases donde el script lo use
Rscript cierre_jornada.r true

# Solo algunos scripts (ej.: solo boncer y nelson)
Rscript cierre_jornada.r false cierre_boncer.R cierre_boncer_be.R nelson_siegel.r
```

*(Las rutas de `Rscript` y del archivo deben apuntar al `path_source` real del servidor.)*

**Filtrado de scripts:** con un solo argumento que no sea `true`/`false` (p. ej. solo el nombre de un script), `run_scripts` sigue siendo `NULL` y corre el pipeline completo. Para limitar la ejecución, el primer argumento debe ser explícitamente `true` o `false` y los siguientes los basenames de los `.R`/`.r` a incluir.

---

## Estructura del repositorio

| Elemento | Rol |
|----------|-----|
| `cierre_jornada.r` | Orquestador: `source` de todos los módulos, render y sync. |
| `cierre_*.R` / `cierre_*.r` | Módulos por tema (FX, bonos, Rofex, etc.); se invocan solo desde el orquestador salvo pruebas manuales. |
| `cierre_jornada.qmd` | Fuente del informe HTML; usa objetos del `.GlobalEnv` tras los `source`. |
| Otros | Utilidades p. ej. `migrar_tablas.R` (no enlazada al pipeline principal). |

El código fuente vive en `path_source`; las salidas (HTML, PNG/PDF de gráficos, `.rds` donde cada script lo use, `cierre.log`) se escriben en `path` (en el código del servidor: rutas bajo `/home/jmt/...`). La dinámica BONCER no añade un `.rds` propio: queda en la tabla `boncer_dinamica`.

---

## Diagrama de flujo (alto nivel)

```mermaid
flowchart LR
  A[cierre_jornada.r] --> B[Backup y limpieza de path]
  B --> C[Calendarios y login PPI]
  C --> D[safe_source scripts en orden]
  D --> E[safe_render cierre_jornada.qmd]
  E --> F[gcloud storage rsync]
  F --> G[cierre.log final]
```
