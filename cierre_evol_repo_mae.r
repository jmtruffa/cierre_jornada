library(tidyverse)
library(outlier)
library(functions)
library(scales)

## Tabla PostgreSQL repo_a3 + API MAE incremental + gráfico -----------------

repo_a3_table <- "repo_a3"
repo_a3_log <- file.path(path, "cierre.log")
repo_a3_from <- as.Date("2026-02-27")
repo_a3_overlap_biz <- 3L

## Vacío con columnas tal como vienen en parsed$details (tras bind_rows + select)
repo_mae_api_empty <- function() {
  tibble::tibble(
    fecha = as.Date(character()),
    volumen = numeric(),
    plazo = numeric(),
    tPP = numeric(),
    tPPnBCRA = numeric()
  )
}

## Serie ya derivada en memoria (mismo criterio que antes) para el gráfico
repo_mae_derived_empty <- function() {
  tibble::tibble(
    fecha = as.Date(character()),
    volumen = numeric(),
    plazo = numeric(),
    tPP = numeric(),
    tPPnBCRA = numeric(),
    tasa_bcra = numeric(),
    participacion_bcra = numeric(),
    volumen_bcra = numeric(),
    volumen_no_bcra = numeric()
  )
}

ensure_repo_a3_table <- function() {
  ## Solo columnas alineadas con bind_rows(parsed$details) tras select(fecha, volumen, plazo, tPP, tPPnBCRA).
  ## Si ya existía repo_a3 con otras columnas, hace falta migrar o DROP TABLE repo_a3 y recrear.
  ddl <- "
  CREATE TABLE IF NOT EXISTS repo_a3 (
    fecha date NOT NULL,
    volumen double precision,
    plazo double precision,
    tpp double precision,
    tppnbcra double precision,
    PRIMARY KEY (fecha)
  );
  "
  tryCatch(
    functions::dbExecuteQuery(query = ddl, server = server, port = port),
    error = function(e) {
      functions::log_msg(
        paste("repo_a3: no se pudo crear/verificar tabla:", conditionMessage(e)),
        "WARN",
        log_file = repo_a3_log
      )
    }
  )
}

## GET + parse: solo columnas de la API (mismo select que antes sobre details)
fetch_repo_mae_api <- function(desde, hasta) {
  require(httr2)
  require(tidyverse)
  require(jsonlite)

  params <- jsonlite::toJSON(
    list(fechaDesde = desde, fechaHasta = hasta),
    auto_unbox = TRUE
  )
  url <- paste0(
    "https://api.marketdata.mae.com.ar/api/mercado/repo/titulosfecha?oTitulo=",
    URLencode(params)
  )
  response <- httr2::request(url) %>%
    httr2::req_method("GET") %>%
    httr2::req_perform()

  parsed <- jsonlite::fromJSON(rawToChar(response$body))

  if (is.null(parsed) || length(parsed) == 0) {
    return(repo_mae_api_empty())
  }

  bind_rows(parsed$details) %>%
    mutate(fecha = as.Date(fecha)) %>%
    select(fecha, volumen, plazo, tPP, tPPnBCRA)
}

## Estimaciones BCRA a partir de tasas (no se persisten; solo para gráfico)
repo_mae_add_derivados <- function(df, tasa_bcra) {
  tb <- tasa_bcra
  df %>%
    mutate(
      tasa_bcra = tb,
      participacion_bcra = if_else(
        tPPnBCRA == tasa_bcra,
        NA_real_,
        (tPPnBCRA - tPP) / (tPPnBCRA - tasa_bcra)
      ),
      volumen_bcra = volumen * participacion_bcra,
      volumen_no_bcra = volumen - volumen_bcra
    )
}

repo_df_to_persist <- function(df) {
  df %>%
    dplyr::transmute(
      fecha = as.Date(fecha),
      volumen = as.numeric(volumen),
      plazo = as.numeric(plazo),
      tpp = as.numeric(tPP),
      tppnbcra = as.numeric(tPPnBCRA)
    )
}

sql_double_or_null <- function(x) {
  if (length(x) != 1L) {
    return("NULL")
  }
  if (is.na(x)) {
    return("NULL")
  }
  sprintf("%.14g", as.numeric(x))
}

repo_a3_build_upsert_sql <- function(df_db) {
  if (nrow(df_db) == 0L) {
    return(character())
  }
  vals <- character(nrow(df_db))
  for (i in seq_len(nrow(df_db))) {
    r <- df_db[i, , drop = FALSE]
    vals[[i]] <- sprintf(
      "('%s', %s, %s, %s, %s)",
      as.character(as.Date(r$fecha)),
      sql_double_or_null(r$volumen),
      sql_double_or_null(r$plazo),
      sql_double_or_null(r$tpp),
      sql_double_or_null(r$tppnbcra)
    )
  }
  paste0(
    "INSERT INTO ", repo_a3_table, " ",
    "(fecha, volumen, plazo, tpp, tppnbcra) ",
    "VALUES ",
    paste(vals, collapse = ", "),
    " ON CONFLICT (fecha) DO UPDATE SET ",
    "volumen = EXCLUDED.volumen, ",
    "plazo = EXCLUDED.plazo, ",
    "tpp = EXCLUDED.tpp, ",
    "tppnbcra = EXCLUDED.tppnbcra;"
  )
}

repo_a3_upsert_chunked <- function(df_db, chunk_size = 80L) {
  if (nrow(df_db) == 0L) {
    return(invisible(NULL))
  }
  n <- nrow(df_db)
  starts <- seq(1L, n, by = chunk_size)
  for (s in starts) {
    e <- min(s + chunk_size - 1L, n)
    q <- repo_a3_build_upsert_sql(df_db[s:e, , drop = FALSE])
    functions::dbExecuteQuery(query = q, server = server, port = port)
  }
  invisible(NULL)
}

repo_db_to_plot_df <- function(db, tasa_bcra) {
  if (nrow(db) == 0L) {
    return(repo_mae_derived_empty())
  }
  out <- db %>%
    dplyr::mutate(fecha = as.Date(fecha)) %>%
    dplyr::transmute(
      fecha,
      volumen = as.numeric(.data$volumen),
      plazo = as.numeric(.data$plazo),
      tPP = as.numeric(.data$tpp),
      tPPnBCRA = as.numeric(.data$tppnbcra)
    ) %>%
    dplyr::arrange(fecha)
  repo_mae_add_derivados(out, tasa_bcra = tasa_bcra)
}

ensure_repo_a3_table()

max_repo_fecha <- tryCatch(
  {
    mx <- functions::dbExecuteQuery(
      query = paste0("SELECT max(fecha) AS m FROM ", repo_a3_table),
      server = server,
      port = port
    )
    if (nrow(mx) == 0L) {
      as.Date(NA)
    } else {
      v <- mx[[1]]
      if (length(v) == 0L || all(is.na(v))) as.Date(NA) else as.Date(v[1])
    }
  },
  error = function(e) as.Date(NA)
)

if (is.null(max_repo_fecha) || length(max_repo_fecha) == 0L) {
  max_repo_fecha <- as.Date(NA)
}

to <- Sys.Date()
tasa_bcra_val <- 20

if (is.na(max_repo_fecha)) {
  desde_api <- repo_a3_from
  functions::log_msg(
    sprintf("repo_a3: tabla vacía o sin max(fecha); bootstrap API desde %s hasta %s.",
            as.character(desde_api), as.character(to)),
    "INFO",
    log_file = repo_a3_log
  )
} else {
  if (max_repo_fecha >= to) {
    desde_api <- NULL
    functions::log_msg(
      sprintf("repo_a3: tabla al día (max fecha %s >= to %s); sin llamada API.",
              as.character(max_repo_fecha), as.character(to)),
      "INFO",
      log_file = repo_a3_log
    )
  } else {
    overlap_from <- bizdays::add.bizdays(max_repo_fecha, -repo_a3_overlap_biz, cal)
    desde_api <- max(as.Date(overlap_from), repo_a3_from)
    functions::log_msg(
      sprintf(
        "repo_a3: incremental/refresh API desde %s hasta %s (max en DB %s, overlap %d hábiles).",
        as.character(desde_api), as.character(to), as.character(max_repo_fecha),
        repo_a3_overlap_biz
      ),
      "INFO",
      log_file = repo_a3_log
    )
  }
}

if (!is.null(desde_api) && desde_api <= to) {
  chunk_api <- tryCatch(
    fetch_repo_mae_api(
      desde = as.character(desde_api),
      hasta = as.character(to)
    ),
    error = function(e) {
      functions::log_msg(
        paste("repo_a3: fallo GET API MAE:", conditionMessage(e)),
        "ERROR",
        log_file = repo_a3_log
      )
      repo_mae_api_empty()
    }
  )

  df_save <- repo_df_to_persist(chunk_api)

  if (nrow(df_save) > 0L) {
    tryCatch(
      {
        repo_a3_upsert_chunked(df_save)
        functions::log_msg(
          sprintf("repo_a3: upsert %d filas (chunked ON CONFLICT).", nrow(df_save)),
          "INFO",
          log_file = repo_a3_log
        )
      },
      error = function(e) {
        functions::log_msg(
          paste("repo_a3: fallo upsert PostgreSQL:", conditionMessage(e)),
          "ERROR",
          log_file = repo_a3_log
        )
      }
    )
  } else {
    functions::log_msg(
      "repo_a3: API devolvió 0 filas en ventana solicitada; sin upsert.",
      "WARN",
      log_file = repo_a3_log
    )
  }
}

df <- tryCatch(
  {
    raw <- functions::dbExecuteQuery(
      query = paste0(
        "SELECT * FROM ", repo_a3_table,
        " WHERE fecha >= '", as.character(repo_a3_from), "' ORDER BY fecha"
      ),
      server = server,
      port = port
    )
    repo_db_to_plot_df(raw, tasa_bcra = tasa_bcra_val)
  },
  error = function(e) {
    functions::log_msg(
      paste("repo_a3: fallo lectura tabla para gráfico:", conditionMessage(e)),
      "ERROR",
      log_file = repo_a3_log
    )
    repo_mae_derived_empty()
  }
)

if (nrow(df) == 0L) {
  functions::log_msg(
    "repo_a3: sin filas para graficar — se omite grabaGrafo.",
    "WARN",
    log_file = repo_a3_log
  )
} else {
  functions::log_msg(
    sprintf("repo_a3: serie para gráfico %d filas (desde %s hasta %s).",
            nrow(df), as.character(min(df$fecha)), as.character(max(df$fecha))),
    "INFO",
    log_file = repo_a3_log
  )

  tasa_min <- 15
  tasa_max <- 35
  max_vol <- max(df$volumen, na.rm = TRUE) / 1e6
  if (!is.finite(max_vol) || max_vol <= 0) {
    functions::log_msg(
      "repo_a3: volumen no finito o cero; no se construye escala ni gráfico.",
      "WARN",
      log_file = repo_a3_log
    )
  } else {
  scale_factor <- max_vol / (tasa_max - tasa_min)

  g_repo <- df %>%
    select(fecha, volumen_bcra, volumen_no_bcra) %>%
    pivot_longer(-fecha, names_to = "serie", values_to = "valor") %>%
    mutate(serie = factor(serie, levels = c("volumen_no_bcra", "volumen_bcra"))) %>%
    ggplot(aes(x = fecha)) +
    theme_outlier() +
    geom_col(aes(y = valor / 1e6, fill = serie)) +
    geom_line(
      data = df,
      aes(y = (tPPnBCRA - tasa_min) * scale_factor, color = "tPPnBCRA"),
      linewidth = 1
    ) +
    geom_hline(
      aes(yintercept = (tasa_bcra_val - tasa_min) * scale_factor, color = "Tasa BCRA"),
      linetype = 2,
      linewidth = 0.8
    ) +
    scale_y_continuous(
      breaks = breaks_extended(14),
      sec.axis = sec_axis(
        ~ . / scale_factor + tasa_min,
        name = "Tasa (TNA %)",
        breaks = seq(15, 50, by = 5)
      )
    ) +
    scale_x_cont_dates(
      name = "",
      business.dates = df$fecha,
      labels = label_date(format = "%d-%b", locale = "es"),
      max.major.breaks = 10
    ) +
    scale_fill_manual(
      name = "",
      labels = c("Volumen sin BCRA", "Volumen BCRA"),
      values = .paleta[1:2]
    ) +
    scale_color_manual(
      name = "",
      labels = c("tPPnBCRA" = "Tasa sin BCRA", "Tasa BCRA" = "Tasa BCRA"),
      values = c("tPPnBCRA" = .paleta[3], "Tasa BCRA" = .paleta[4])
    ) +
    labs(
      title = "VOLUMEN REPO MAE",
      subtitle = paste0(
        "Estimado Tasa BCRA ", tasa_bcra_val,
        "%. Volumen total vs. volumen estimado BCRA (en Billones de Pesos). Último dato: ",
        tail(df, n = 1) %>% pull(fecha)
      ),
      y = "Volumen Billones",
      x = "",
      caption = paste0(.pie, " en base a A3 Market Data")
    )

  grabaGrafo(variable = g_repo, name = "g_evol_repo_bcra", path = path)
  functions::log_msg(
    "repo_a3: grabaGrafo g_evol_repo_bcra OK.",
    "INFO",
    log_file = repo_a3_log
  )
  }
}
