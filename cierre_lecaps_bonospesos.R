## =========================
## INIT
## =========================
fails                 <- tibble::tibble()
lecaps                <- tibble::tibble()
curva_lecaps          <- tibble::tibble()
lecap_dinamica        <- tibble::tibble()
curva_lecaps_dinamica <- tibble::tibble()

methodsPPI::getPPILogin()

## Helper para elegir fecha from segura cuando max(date) es NA
safe_from <- function(from, max_fecha) {
  if (is.na(max_fecha) || is.null(max_fecha)) as.Date(from) else min(as.Date(from), as.Date(max_fecha) + 1)
}

## =========================
## 1) LECAPS (precios → DB → releer → curva + DUALES)
## =========================
max_fecha_lecap <- functions::dbExecuteQuery(
  query = "select max(date) as max from historico_lecaps", server = server, port = port
) %>% dplyr::pull(max)

lecaps_consultar <- finance::getLecaps(server = server, port = port) %>%
  dplyr::mutate(type = ifelse(stringr::str_detect(ticker, "^S"), "LETRAS", "BONOS")) %>%
  dplyr::filter(date_vto >= as.Date(ifelse(is.na(max_fecha_lecap), from, max_fecha_lecap)))

lecap_prices <- suppressMessages(
  methodsPPI::check_getPPIPrices(
    token$token,
    ticker     = lecaps_consultar$ticker,
    type       = lecaps_consultar$type,
    from       = safe_from(from, max_fecha_lecap),
    to         = to,
    settlement = settlement,
    server     = server,
    port       = port
  )
)

if (!lecap_prices$ok && is.null(lecap_prices$data)) {
  functions::log_msg(paste("LECAPS: fallo descarga precios:", lecap_prices$msg), "ERROR", log_file = log_file)
  # seguimos con curva_lecaps vacía
} else {
  if (!is.null(lecap_prices$fail) && nrow(lecap_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, lecap_prices$fail)
  }

  lecaps <- lecap_prices$data

  # Actualizar DB solo con lo nuevo
  if (isTRUE(update) && nrow(lecaps) > 0) {
    functions::dbWriteDF(
      table = "historico_lecaps",
      df    = dplyr::filter(lecaps, date > as.Date(ifelse(is.na(max_fecha_lecap), "1900-01-01", max_fecha_lecap))),
      server = server, port = port, append = TRUE
    )
  }

  # Releer desde DB para cubrir gaps/vencidos verificando que si no grabamos, agregue lo nuevo.
  lecaps_db <- functions::dbExecuteQuery(
    query  = paste0("select date, ticker, price from historico_lecaps where date >= '", as.Date(from), "'"),
    server = server, port = port
  )
  lecaps_db$date = as.Date(lecaps_db$date)
  
  lecaps = lecaps %>% select(date, ticker, price)
  if (isTRUE(update)) {
    lecaps = lecaps_db
  } else {
    lecaps = dplyr::bind_rows(lecaps_db,
                              anti_join(lecaps, lecaps_db)
                              ) %>% arrange(date)
  }

  if (!is.null(lecaps) && nrow(lecaps) > 0) {
    
    curva_lecaps <- finance::tasasLecap(lecaps, server = server, port = port) %>%
      dplyr::mutate(group = "LECAPS")

    ## DUALES TAMAR — se calculan sobre subset de la curva LECAPS (tickers TT)
    tasa_tamar <- bcra::getDatosVariable(idVariable = 136, desde = "2024-01-01", hasta = to) %>%
      dplyr::mutate(valor = valor / 100)

    duales <- curva_lecaps %>%
      dplyr::filter(stringr::str_detect(ticker, "TT")) %>%
      dplyr::mutate(
        ticker     = paste0(ticker, "_tmr"),
        date_start = bizdays::add.bizdays(date_liq, -10, cal),
        date_end   = bizdays::add.bizdays(date,    -9,  cal)
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        tamar_prom_tna = mean(
          dplyr::filter(tasa_tamar, date >= date_start & date <= date_end) %>% dplyr::pull(valor),
          na.rm = TRUE
        ),
        tamar_tem = ((1 + tamar_prom_tna * 32 / 365)^(365/32))^(1/12) - 1,
        vpv       = 100 * (1 + tamar_tem)^((days360(date_liq, date_vto) / 360) * 12),
        tem       = (vpv / price)^(1 / ((as.numeric(date_vto - date) / 360) * 12)) - 1,
        group     = "DUALES"
      ) %>%
      dplyr::ungroup()
  
    if (!is.null(duales) && nrow(duales) > 0) {
      curva_lecaps <- dplyr::bind_rows(
        curva_lecaps,
        dplyr::select(duales, -date_start, -date_end, -tamar_prom_tna, -tamar_tem, -vpv)
      )
    }
    # Acá le saco los TT a tasa fija, por eso dejo los que terminan en _tmr
    curva_lecaps <- curva_lecaps %>% 
      filter(!grepl("^TT(?!.*_tmr$)", ticker, perl = TRUE)) %>% 
      filter(ticker != "TTM26_tmr")
  } else {
    functions::log_msg("LECAPS: releída DB vacía desde 'from'; curva_lecaps queda vacía.", "WARN", log_file = log_file)
  }
}

## =========================
## 2) BONOS EN PESOS (precios → DB → releer → yields → append a curva_lecaps)
## =========================
max_fecha_bonos_pesos <- functions::dbExecuteQuery(
  query = "select max(date) as max from precios_bonos_pesos", server = server, port = port
) %>% dplyr::pull(max)

tickers_bonos_pesos <- suppressMessages(
  purrr::map_dfr(.x = "bonosPesos", .f = methodsPPI::sets, server = server, port = port)
)
vtos <- functions::dbGetTable("vencTitulos", server = server, port = port)

tickers_bonos_pesos <- dplyr::left_join(tickers_bonos_pesos, vtos, by = "ticker") %>%
  dplyr::filter(vto > as.Date(from))

bonos_pesos_prices <- suppressMessages(
  methodsPPI::check_getPPIPrices(
    token$token,
    ticker     = tickers_bonos_pesos$ticker,
    type       = tickers_bonos_pesos$type,
    from       = safe_from(from, max_fecha_bonos_pesos),
    to         = to,
    settlement = settlement,
    server     = server,
    port       = port
  )
)

if (!bonos_pesos_prices$ok && is.null(bonos_pesos_prices$data)) {
  functions::log_msg(paste("BONOS PESOS: fallo descarga precios:", bonos_pesos_prices$msg), "ERROR", log_file = log_file)
  # seguimos; no se agregan BOTES
} else {
  if (!is.null(bonos_pesos_prices$fail) && nrow(bonos_pesos_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, bonos_pesos_prices$fail)
  }

  bonos_pesos_prices_df <- bonos_pesos_prices$data

  if (isTRUE(update) && nrow(bonos_pesos_prices_df) > 0) {
    functions::dbWriteDF(
      table  = "precios_bonos_pesos",
      df     = dplyr::filter(bonos_pesos_prices_df, date > as.Date(ifelse(is.na(max_fecha_bonos_pesos), "1900-01-01", max_fecha_bonos_pesos))),
      server = server, port = port, append = TRUE
    )
  }

  # Releer desde DB solo si update=TRUE; si no, reutilizamos lo descargado
  if (isTRUE(update)) {
    bonos_pesos_prices_all <- functions::dbExecuteQuery(
      query = paste0("select date, ticker, price from precios_bonos_pesos where date >= '", as.Date(from), "'"),
      server = server, port = port
    )
  } else {
    bonos_pesos_prices_all <- bonos_pesos_prices_df %>% dplyr::filter(date >= as.Date(from)) %>% select(-volume, -openingPrice, -max, -min, -previousClose, -marketChange, -marketChangePercent)
  }
  
  #bonos_pesos_prices_all = bonos_pesos_prices_all

  if (!is.null(bonos_pesos_prices_all) && nrow(bonos_pesos_prices_all) > 0) {
    # Yields (no corta el proceso si falla)
    res_y <- functions::check_getYields(
      letras         = bonos_pesos_prices_all$ticker,
      settlementDate = as.character(bizdays::offset(bonos_pesos_prices_all$date, ifelse(settlement == "INMEDIATA", 0, 1), cal = cal)),
      precios        = bonos_pesos_prices_all$price,
      initialFee     = comi,
      endpoint       = "yield"
    )

    if (!res_y$ok && is.null(res_y$data)) {
      functions::log_msg(paste("BONOS PESOS (yields): fallo:", res_y$msg), "ERROR", log_file = log_file)
    } else {
      if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
        functions::log_msg(sprintf("BONOS PESOS: issues en yields (NA): %d", nrow(res_y$issues)), "WARN", log_file = log_file)
      } else {
        functions::log_msg("BONOS PESOS: yields OK.", "INFO", log_file = log_file)
      }

      apr_bonos_pesos <- res_y$data
      apr_bonos_pesos$maturity <- as.Date(apr_bonos_pesos$maturity)

      bonos_pesos <- dplyr::bind_cols(bonos_pesos_prices_all, apr_bonos_pesos) %>%
        dplyr::select(-letras, -precios, -endingFee, -initialFee) %>%
        dplyr::select(-parity, -techValue, -residual) %>%
        dplyr::rename(date_vto = maturity) %>%
        dplyr::mutate(
          tem       = (1 + yield)^(1/12) - 1,
          mduration = mduration * 365,
          tna       = ((1 + yield)^(1/2) - 1) * 2,
          group     = "BOTES"
        )

      # Append a curva_lecaps (si viene vacía, queda solo BOTES)
      curva_lecaps <- dplyr::bind_rows(curva_lecaps, bonos_pesos)
      saveRDS(curva_lecaps, file.path(path, "curva_lecaps.rds"))
      functions::log_msg(
        "curva_lecaps guardada en /home/jmt/cierre-jornada/curva_lecaps.rds",
        "INFO",
        log_file = log_file
      )
    }
  } else {
    functions::log_msg("BONES PESOS: releída DB vacía desde 'from'; no se calculan yields.", "WARN", log_file = log_file)
  }
}

## =========================
## 3) LECAPS DINÁMICA (tabla incremental curva_lecaps_dinamica ← historico_lecaps)
## =========================
curva_dinamica_table <- "curva_lecaps_dinamica"
curva_dinamica_log <- file.path(path, "cierre.log")

ensure_curva_dinamica_table <- function() {
  ddl <- "
  CREATE TABLE IF NOT EXISTS curva_lecaps_dinamica (
    date date NOT NULL,
    ticker text NOT NULL,
    price double precision,
    vf double precision,
    date_vto date,
    date_liq date,
    settle date,
    dias360 double precision,
    dias double precision,
    tdirecta double precision,
    tna double precision,
    tea double precision,
    tem double precision,
    tna360 double precision,
    tea360 double precision,
    tem360 double precision,
    duration double precision,
    mduration double precision,
    PRIMARY KEY (date, ticker)
  );
  "
  tryCatch(
    functions::dbExecuteQuery(query = ddl, server = server, port = port),
    error = function(e) {
      functions::log_msg(
        paste("curva_lecaps_dinamica: no se pudo crear/verificar tabla:", conditionMessage(e)),
        "WARN",
        log_file = curva_dinamica_log
      )
    }
  )
}

## Output de tasasLecap (no incluye `group`; si el join a `lecaps` trajera esa columna, no la persistimos).
## Si `lecaps` en DB tuviera otras columnas extra respecto al DDL, añadirlas aquí y en ensure_curva_dinamica_table.
curva_dinamica_persist_cols <- function(df) {
  dplyr::select(df, -dplyr::any_of("group"))
}

ensure_curva_dinamica_table()

max_curva_din <- tryCatch(
  {
    mx <- functions::dbExecuteQuery(
      query = paste0("SELECT max(date) AS m FROM ", curva_dinamica_table),
      server = server,
      port = port
    )
    if (nrow(mx) == 0L) {
      NA
    } else {
      v <- mx[[1]]
      if (length(v) == 0L || all(is.na(v))) NA else as.Date(v[1])
    }
  },
  error = function(e) NA
)

if (is.null(max_curva_din) || length(max_curva_din) == 0L) {
  max_curva_din <- NA
}

if (is.na(max_curva_din)) {
  lecap_dinamica <- functions::dbExecuteQuery(
    query = paste0(
      "SELECT date, ticker, price FROM historico_lecaps WHERE date >= '",
      as.Date(from_dinamica), "'"
    ),
    server = server,
    port = port
  )
  if (!is.null(lecap_dinamica) && nrow(lecap_dinamica) > 0L) {
    lecap_dinamica$date <- as.Date(lecap_dinamica$date)
    curva_new <- finance::tasasLecap(lecap_dinamica, server = server, port = port)
    df_save <- curva_dinamica_persist_cols(curva_new)
    functions::dbWriteDF(
      table = curva_dinamica_table,
      df = df_save,
      server = server,
      port = port,
      append = TRUE
    )
    functions::log_msg(
      sprintf("curva_lecaps_dinamica: bootstrap insertadas %d filas.", nrow(df_save)),
      "INFO",
      log_file = curva_dinamica_log
    )
  } else {
    functions::log_msg(
      "LECAPS DINÁMICA: historico_lecaps vacío desde from_dinamica; sin bootstrap de curva.",
      "WARN",
      log_file = log_file
    )
  }
} else {
  lecap_nuevos <- functions::dbExecuteQuery(
    query = paste0(
      "SELECT date, ticker, price FROM historico_lecaps WHERE date > '",
      as.character(as.Date(max_curva_din)),
      "' AND date >= '", as.Date(from_dinamica), "'"
    ),
    server = server,
    port = port
  )
  if (!is.null(lecap_nuevos) && nrow(lecap_nuevos) > 0L) {
    lecap_nuevos$date <- as.Date(lecap_nuevos$date)
    curva_new <- finance::tasasLecap(lecap_nuevos, server = server, port = port)
    df_save <- curva_dinamica_persist_cols(curva_new)
    functions::dbWriteDF(
      table = curva_dinamica_table,
      df = df_save,
      server = server,
      port = port,
      append = TRUE
    )
    functions::log_msg(
      sprintf("curva_lecaps_dinamica: incremental insertadas %d filas.", nrow(df_save)),
      "INFO",
      log_file = curva_dinamica_log
    )
  } else {
    functions::log_msg(
      "curva_lecaps_dinamica: sin fechas nuevas en historico_lecaps — skip tasasLecap.",
      "INFO",
      log_file = curva_dinamica_log
    )
  }
}

curva_lecaps_dinamica <- tryCatch(
  functions::dbExecuteQuery(
    query = paste0(
      "SELECT * FROM ", curva_dinamica_table,
      " WHERE date >= '", as.Date(from_dinamica), "' ORDER BY date, ticker"
    ),
    server = server,
    port = port
  ),
  error = function(e) tibble::tibble()
)

if (nrow(curva_lecaps_dinamica) > 0L) {
  curva_lecaps_dinamica <- curva_lecaps_dinamica %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "price", "vf", "dias360", "dias", "tdirecta", "tna", "tea", "tem",
          "tna360", "tea360", "tem360", "duration", "mduration"
        )),
        as.numeric
      )
    )
  for (d in c("date_vto", "date_liq", "settle")) {
    if (d %in% names(curva_lecaps_dinamica)) {
      curva_lecaps_dinamica[[d]] <- as.Date(curva_lecaps_dinamica[[d]])
    }
  }
} else {
  functions::log_msg(
    "LECAPS DINÁMICA: curva vacía desde tabla curva_lecaps_dinamica.",
    "WARN",
    log_file = log_file
  )
}

## =========================
## (Opcional) Log de estado final
## =========================
functions::log_msg(sprintf("LECAPS filas: %d | Curva total filas: %d | Curva dinámica filas: %d",
                           nrow(lecaps), nrow(curva_lecaps), nrow(curva_lecaps_dinamica)),
                   "INFO", log_file = log_file)
if (nrow(fails) > 0) {
  functions::log_msg(sprintf("Fails acumulados: %d", nrow(fails)), "WARN", log_file = log_file)
}


############################################################
# Graficamos
g_lecap_tem = suppressMessages(
  suppressWarnings(
    curva_lecaps %>% 
      filter(date == from | date == to) %>% 
      ggplot(aes(x=mduration, y=tem, label = ticker, group = date, color = as.factor(date))) +
      theme_usado() +
      geom_point(size=1) +
      geom_smooth(
        data = dplyr::filter(curva_lecaps %>% filter(date == from | date == to), grepl("_tmr$", ticker)),
        method = "lm", formula = y ~ poly(x, 2), se = FALSE,
        linewidth = 1, linetype = "dashed", show.legend = FALSE
      ) +
      geom_smooth(
        data = dplyr::filter(curva_lecaps %>% filter(date == from | date == to), !grepl("_tmr$", ticker)),
        method = "lm", formula = y ~ poly(x, 2), se = FALSE,
        linewidth = 1, linetype = "solid", show.legend = FALSE
      ) +
      #geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE, linewidth = 1) +
      ggrepel::geom_text_repel(show.legend = F) +
      scale_color_manual(name = NULL, values = .paleta) +  
      scale_y_continuous(breaks = breaks_extended(10), 
                         labels = scales::percent,
                         #limits = c(.03, .0425)
      ) +
      scale_x_continuous(breaks = breaks_extended(10)) +
      labs(title = "CURVA LECAPS",
           subtitle = paste0('Último dato: ', max(curva_lecaps$date)),
           y = 'TEM',
           x = 'duration (días)',
           caption = paste0(.pie, " en base a precios de mercado."))
  )
)
  
  grabaGrafo(variable = g_lecap_tem, path = path)

g_lecap_tna = suppressMessages(
  suppressWarnings(
    curva_lecaps %>%
      filter(date == from |date == to) %>%
      ggplot(aes(x=mduration, y=tna, group = date, color = as.factor(date))) +
      theme_usado() +
      geom_point(size=1) +
      #geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE, linewidth = 1) +
      geom_smooth(
        data = dplyr::filter(curva_lecaps %>% filter(date == from | date == to), grepl("_tmr$", ticker)),
        method = "lm", formula = y ~ poly(x, 2), se = FALSE,
        linewidth = 1, linetype = "dashed", show.legend = FALSE
      ) +
      geom_smooth(
        data = dplyr::filter(curva_lecaps %>% filter(date == from | date == to), !grepl("_tmr$", ticker)),
        method = "lm", formula = y ~ poly(x, 2), se = FALSE,
        linewidth = 1, linetype = "solid", show.legend = FALSE
      ) +
    
      ggrepel::geom_text_repel(aes(label = ticker), show.legend = F) +
      scale_color_manual(name = NULL, values = .paleta) +  
      scale_y_continuous(breaks = breaks_extended(10), 
                         labels = scales::percent,
                         #limits = c(.03, .0425)
      ) +
      scale_x_continuous(breaks = breaks_extended(10)) +
      labs(title = "CURVA PESOS",
           subtitle = paste0('Último dato: ', max(curva_lecaps$date)),
           y = 'TNA',
           x = 'duration (días)',
           caption = paste0(.pie, " en base a precios de mercado."))
  )
)
    
grabaGrafo(variable = g_lecap_tna, path = path)

if (nrow(curva_lecaps_dinamica) > 0L) {
  ultima_curva_din <- curva_lecaps_dinamica %>%
    dplyr::slice_max(date, n = 1, with_ties = FALSE) %>%
    dplyr::pull(date)

  g_lecap_dinamica_tem <- suppressMessages(
    suppressWarnings(
      curva_lecaps_dinamica %>%
        dplyr::filter(tem > 0, date >= "2025-01-01") %>%
        ggplot(aes(x = date, y = tem, color = ticker, label = ticker)) +
        theme_usado() +
        geom_point() +
        geom_line(linewidth = 1) +
        scale_x_date(
          date_breaks = "1 month",
          labels = date_format("%d-%b", locale = "es"),
          expand = c(0.07, 0.0)
        ) +
        scale_y_continuous(
          breaks = breaks_extended(10),
          labels = scales::percent
        ) +
        labs(
          title = "CURVA LECAP - DINAMICA",
          subtitle = paste0("Último dato: ", ultima_curva_din),
          y = "TEM",
          x = "",
          caption = paste0(.pie, " en base a precios de mercado.")
        ) +
        theme(legend.title = element_blank()) +
        guides(color = guide_legend(ncol = 14))
    )
  )

  grabaGrafo(variable = g_lecap_dinamica_tem, path = path)

  g_lecap_dinamica_tna <- suppressMessages(
    suppressWarnings(
      curva_lecaps_dinamica %>%
        dplyr::filter(tem > 0, date >= "2025-01-01") %>%
        ggplot(aes(x = date, y = tna, color = ticker, label = ticker)) +
        theme_usado() +
        geom_point() +
        geom_line(linewidth = 1) +
        scale_x_date(
          date_breaks = "1 month",
          labels = date_format("%d-%b", locale = "es"),
          expand = c(0.07, 0.0)
        ) +
        scale_y_continuous(
          breaks = breaks_extended(10),
          labels = scales::percent
        ) +
        labs(
          title = "CURVA LECAP - DINAMICA",
          subtitle = paste0("Último dato: ", ultima_curva_din),
          y = "TNA",
          x = "",
          caption = paste0(.pie, " en base a precios de mercado.")
        ) +
        theme(legend.title = element_blank()) +
        guides(color = guide_legend(ncol = 14))
    )
  )

  grabaGrafo(variable = g_lecap_dinamica_tna, path = path)
}