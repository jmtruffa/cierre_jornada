max_fecha_boncer = functions::dbExecuteQuery(query = "select max(date) from precios_bonos_cer", server = server, port = port) %>% pull()
methodsPPI::getPPILogin() # Vuelvo a ponerlo total chequea que esté vigente.
query = glue::glue("
SELECT DISTINCT
    bt.ticker,
    db.maturity AS vto,
    bt.tipo_instr_temp_ppi as type
FROM bonds_db db
JOIN bonds_tickers bt 
    ON db.id = bt.bond_id
WHERE db.maturity >= '{as.Date(from)}'
  AND db.index_type_id = 1
  AND bt.cotizacion = 1
ORDER BY bt.ticker
")
tickersBonosCER = dbExecuteQuery(query = query, server = server, port = port)

#tickers_boncer = map_dfr(.x = "bonosCER", .f = methodsPPI::sets, server = server, port = port)
#vtos = dbGetTable("vencTitulos", server = server, port = port)
#tickers_boncer = tickers_boncer %>% left_join(vtos, join_by(ticker)) %>% 
#  filter(vto > from)

boncer_prices = methodsPPI::check_getPPIPrices(
  token$token, 
  ticker = tickers_boncer$ticker, 
  type = tickers_boncer$type, 
  from = min(from, max_fecha_boncer), 
  to = to, 
  settlement = settlement, 
  server = server, 
  port = port)

if (!boncer_prices$ok && is.null(boncer_prices$data)) {
  functions::log_msg(
    "Abortamos proceso con boncer", 
    "ERROR", 
    log_file = "./cierre.log"
  ) 
  # objetos vacíos para seguir el pipeline y que no tire error por objeto no encontrado
  boncer = tibble::tibble()
  

} else {
  ## si no hubo error
  # puede haber `fail` con tickers sin datos aunque haya `data`
  if (!is.null(boncer_prices$fail) && nrow(boncer_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, boncer_prices$fail)
  }

  boncer = boncer_prices$data 

  # actualizamos la base SOLO con lo nuevo
  if (update) {
    functions::dbWriteDF(
      table = "precios_bonos_cer",
      df    = dplyr::filter(boncer, date > max_fecha_boncer),
      server = server, port = port, append = TRUE
    )
  }

  # ahora re-leemos desde la tabla (para cubrir gaps si la API ya no trae vencidos)
  if (update) {
    boncer = dbExecuteQuery(
      query = paste0("select date, ticker, price from precios_bonos_cer where date >= '", from, "'"), 
      server = server, port = port)
  }
  
  temp_boncer = boncer
  res_y <- functions::check_getYields(
    letras         = temp_boncer$ticker,
    settlementDate = as.character(
      bizdays::offset(temp_boncer$date, ifelse(settlement == "INMEDIATA", 0, 1), cal = cal)
    ),
    precios        = temp_boncer$price,
    initialFee     = comi,
    endpoint       = "yield"
  )

  if (!res_y$ok && is.null(res_y$data)) {
    functions::log_msg(
        paste("Abortamos proceso con Boncer (yields):", res_y$msg), 
        "ERROR",
        log_file = "./cierre.log"
    )

  } else {

    # OJO: acá NO existe res_y$fail (solo issues)
    if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
      functions::log_msg(
        sprintf("Boncer con issues (filas con NA en rendimiento): %d", nrow(res_y$issues)),
        "WARN",
        log_file = "./cierre.log"
      )
    } else {
      functions::log_msg("Boncer OK (rendimientos sin NA).", "INFO", log_file = "./cierre.log")
    }

    apr_boncer = res_y$data
    apr_boncer$maturity = as.Date(apr_boncer$maturity)

    boncer = dplyr::bind_cols(temp_boncer, apr_boncer)  %>% 
      dplyr::select(-letras, -precios, -endingFee, -initialFee) %>%
      dplyr::select(-parity, -techValue, -residual) %>%
      dplyr::rename(date_vto = maturity) %>%
      dplyr::mutate(
          tem       = (1 + yield)^(1/12) - 1,
          tna       = ((1 + yield)^(1/2) - 1) * 2,
          group     = "BONCER"
      )
    
    
  }
}

# --- boncer_dinamica: tabla incremental (no recalcular todo el histórico cada vez) ---
boncer_dinamica_table <- "boncer_dinamica"
boncer_dinamica_log <- file.path(path, "cierre.log")

ensure_boncer_dinamica_table <- function() {
  ddl <- "
  CREATE TABLE IF NOT EXISTS boncer_dinamica (
    date date NOT NULL,
    ticker text NOT NULL,
    price double precision,
    date_vto date,
    yield double precision,
    tem double precision,
    tna double precision,
    \"group\" text,
    mduration double precision,
    duration double precision,
    convexity double precision,
    PRIMARY KEY (date, ticker)
  );
  "
  tryCatch(
    functions::dbExecuteQuery(query = ddl, server = server, port = port),
    error = function(e) {
      functions::log_msg(
        paste("boncer_dinamica: no se pudo crear/verificar tabla:", conditionMessage(e)),
        "WARN",
        log_file = boncer_dinamica_log
      )
    }
  )
}

boncer_dinamica_persist_cols <- function(df) {
  cols <- c(
    "date", "ticker", "price", "date_vto", "yield", "tem", "tna", "group",
    "mduration", "duration", "convexity"
  )
  dplyr::select(df, dplyr::any_of(cols))
}

enrich_boncer_dinamica_from_prices <- function(temp_boncer_dinamica) {
  res_y <- functions::check_getYields(
    letras = temp_boncer_dinamica$ticker,
    settlementDate = as.character(
      bizdays::offset(
        temp_boncer_dinamica$date,
        ifelse(settlement == "INMEDIATA", 0, 1),
        cal = cal
      )
    ),
    precios = temp_boncer_dinamica$price,
    initialFee = comi,
    endpoint = "yield"
  )
  list(res_y = res_y)
}

apply_enrich_boncer_dinamica <- function(temp_boncer_dinamica, res_y) {
  apr_boncer_dinamica <- res_y$data
  apr_boncer_dinamica$maturity <- as.Date(apr_boncer_dinamica$maturity)
  dplyr::bind_cols(temp_boncer_dinamica, apr_boncer_dinamica) %>%
    dplyr::select(-dplyr::any_of(c("letras", "precios", "endingFee", "initialFee"))) %>%
    dplyr::select(-dplyr::any_of(c("parity", "techValue", "residual"))) %>%
    dplyr::rename(date_vto = maturity) %>%
    dplyr::mutate(
      tem = (1 + yield)^(1 / 12) - 1,
      tna = ((1 + yield)^(1 / 2) - 1) * 2,
      group = "BONCER"
    )
}

ensure_boncer_dinamica_table()

max_boncer_dinamica <- tryCatch(
  {
    mx <- functions::dbExecuteQuery(
      query = paste0("SELECT max(date) AS m FROM ", boncer_dinamica_table),
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

if (is.null(max_boncer_dinamica) || length(max_boncer_dinamica) == 0L) {
  max_boncer_dinamica <- NA
}

if (is.na(max_boncer_dinamica)) {
  # Bootstrap: poblar desde from_dinamica (tabla vacía o sin max)
  precios_dm <- dbExecuteQuery(
    query = paste0(
      "SELECT date, ticker, price FROM precios_bonos_cer WHERE date >= '",
      from_dinamica, "'"
    ),
    server = server,
    port = port
  )
  if (nrow(precios_dm) == 0L) {
    functions::log_msg(
      "Boncer dinamico: bootstrap sin precios en rango — tabla sin cambios.",
      "WARN",
      log_file = boncer_dinamica_log
    )
  } else {
    out <- enrich_boncer_dinamica_from_prices(precios_dm)
    res_y <- out$res_y
    if (!res_y$ok && is.null(res_y$data)) {
      functions::log_msg(
        paste("Abortamos proceso con Boncer Dinamico (yields, bootstrap):", res_y$msg),
        "ERROR",
        log_file = "./cierre.log"
      )
    } else if (!is.null(res_y$data)) {
      if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
        functions::log_msg(
          sprintf(
            "Boncer dinamico bootstrap con issues (filas con NA en rendimiento): %d",
            nrow(res_y$issues)
          ),
          "WARN",
          log_file = "./cierre.log"
        )
      } else {
        functions::log_msg(
          "Boncer dinamico OK (bootstrap, rendimientos sin NA).",
          "INFO",
          log_file = "./cierre.log"
        )
      }
      boncer_dm_enriched <- apply_enrich_boncer_dinamica(precios_dm, res_y)
      df_save <- boncer_dinamica_persist_cols(boncer_dm_enriched)
      functions::dbWriteDF(
        table = boncer_dinamica_table,
        df = df_save,
        server = server,
        port = port,
        append = TRUE
      )
      functions::log_msg(
        sprintf("boncer_dinamica: bootstrap insertadas %d filas.", nrow(df_save)),
        "INFO",
        log_file = boncer_dinamica_log
      )
    }
  }
} else {
  # Incremental: solo fechas posteriores al máximo ya guardado
  precios_dm <- dbExecuteQuery(
    query = paste0(
      "SELECT date, ticker, price FROM precios_bonos_cer WHERE date > '",
      as.character(as.Date(max_boncer_dinamica)), "'"
    ),
    server = server,
    port = port
  )
  if (nrow(precios_dm) == 0L) {
    functions::log_msg(
      "Boncer dinamico: sin fechas nuevas respecto a boncer_dinamica — skip yields.",
      "INFO",
      log_file = boncer_dinamica_log
    )
  } else {
    out <- enrich_boncer_dinamica_from_prices(precios_dm)
    res_y <- out$res_y
    if (!res_y$ok && is.null(res_y$data)) {
      functions::log_msg(
        paste("Abortamos proceso con Boncer Dinamico (yields, incremental):", res_y$msg),
        "ERROR",
        log_file = "./cierre.log"
      )
    } else if (!is.null(res_y$data)) {
      if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
        functions::log_msg(
          sprintf(
            "Boncer dinamico incremental con issues (filas con NA en rendimiento): %d",
            nrow(res_y$issues)
          ),
          "WARN",
          log_file = "./cierre.log"
        )
      } else {
        functions::log_msg(
          "Boncer dinamico OK (incremental, rendimientos sin NA).",
          "INFO",
          log_file = "./cierre.log"
        )
      }
      boncer_dm_enriched <- apply_enrich_boncer_dinamica(precios_dm, res_y)
      df_save <- boncer_dinamica_persist_cols(boncer_dm_enriched)
      functions::dbWriteDF(
        table = boncer_dinamica_table,
        df = df_save,
        server = server,
        port = port,
        append = TRUE
      )
      functions::log_msg(
        sprintf("boncer_dinamica: incremental insertadas %d filas.", nrow(df_save)),
        "INFO",
        log_file = boncer_dinamica_log
      )
    }
  }
}

# Serie para gráficos / RDS / Nelson: leída desde la tabla (histórico acumulado)
boncer_dinamica <- tryCatch(
  dbExecuteQuery(
    query = paste0(
      "SELECT * FROM ", boncer_dinamica_table,
      " WHERE date >= '", from_dinamica, "' ORDER BY date, ticker"
    ),
    server = server,
    port = port
  ),
  error = function(e) tibble::tibble()
)

#####################################
# Graficamos
g_boncer = boncer %>%
    filter(date == from | date == to) %>%
    group_by(ticker) %>%
    ggplot(aes(x = mduration, y = yield, color = as.factor(date), group = date, label = ticker)) +
    theme_usado() +
    geom_point(size=1) +
    geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE) +
    ggrepel::geom_text_repel(show.legend = F, max.overlaps = 14) +
    scale_color_manual(name = NULL, values = .paleta) +  
    scale_y_continuous(breaks = breaks_extended(14), labels = scales::percent, 
                       #limits = c(.0,.15)
    ) +
    scale_x_continuous(breaks = breaks_extended(10)) +
    
    
    labs(title = "CURVA BONCER",
         subtitle = paste0('Último dato: ', max(boncer$date)),
         y = 'TIR',
         x = 'Duration Modificada',
         caption = paste0(.pie, " en base a precios de mercado."))
  
grabaGrafo(variable = g_boncer, path = path)

if (nrow(boncer_dinamica) > 0L) {
  ultima_fecha_dm <- boncer_dinamica %>% dplyr::slice_max(date, n = 1, with_ties = FALSE) %>% dplyr::pull(date)
  g_boncer_dinamica <- boncer_dinamica %>%
    dplyr::select(ticker, date, yield) %>%
    tidyr::drop_na() %>%
    ggplot(aes(x = date, y = yield, color = ticker, label = ticker)) +
    theme_usado() +
    geom_point() +
    scale_x_date(
      date_breaks = "2 month",
      labels = date_format("%d-%b-%Y", locale = "es"),
      expand = c(0.07, 0.0)
    ) +
    scale_y_continuous(
      breaks = breaks_extended(10),
      labels = scales::percent,
      limits = c(-0.05, 0.40)
    ) +
    labs(
      title = "CURVA REAL CER",
      subtitle = paste0("Último dato: ", ultima_fecha_dm),
      y = "TIR",
      x = "",
      caption = paste0(.pie, " en base a precios de mercado.")
    ) +
    geom_hline(yintercept = 0, color = "black") +
    theme(legend.title = element_blank()) +
    guides(color = guide_legend(ncol = 8))

  grabaGrafo(variable = g_boncer_dinamica, path = path)
}
