############################################################
# DOLLAR LINKED - ESTÁTICO
############################################################

max_fecha_dl = dbExecuteQuery(
  query = "select max(date) from precios_dl",
  server = server, port = port
) %>% pull()

methodsPPI::getPPILogin()  # chequea que el login esté vigente

tickersDL = purrr::map_dfr(
  .x = "bonosDL",
  .f = methodsPPI::sets,
  server = server,
  port   = port
)

vtos = dbGetTable("vencTitulos", server = server, port = port)

tickersDL = tickersDL %>%
  dplyr::left_join(vtos, dplyr::join_by(ticker)) %>%
  dplyr::filter(vto > from)

dl_prices = methodsPPI::check_getPPIPrices(
  token$token,
  ticker     = tickersDL$ticker,
  type       = tickersDL$type,
  from       = min(from, max_fecha_dl),
  to         = to,
  settlement = settlement,
  server     = server,
  port       = port
)

if (!dl_prices$ok && is.null(dl_prices$data)) {
  functions::log_msg(
    "Abortamos proceso con linkers",
    "ERROR",
    log_file = "./cierre.log"
  )
  # objetos vacíos para seguir el pipeline
  dl <- tibble::tibble()

} else {
  # puede haber `fail` con tickers sin datos aunque haya `data`
  if (!is.null(dl_prices$fail) && nrow(dl_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, dl_prices$fail)
  }

  dl <- dl_prices$data

  # actualizamos la base SOLO con lo nuevo
  if (update) {
    functions::dbWriteDF(
      table  = "precios_dl",
      df     = dplyr::filter(dl, date > max_fecha_dl),
      server = server, port = port, append = TRUE
    )
  }

  # re-leemos desde la tabla (para cubrir gaps si la API ya no trae vencidos)
  dl <- dbExecuteQuery(
    query  = paste0(
      "select date, ticker, price from precios_dl where date >= '", from, "'"
    ),
    server = server, port = port
  )

  # Ajuste por tipo de cambio (pasamos a price_adj)
  dl <- dl %>%
    dplyr::left_join(
      tc %>% dplyr::select(date, tc = last_mlc),
      by = "date"
    ) %>%
    dplyr::mutate(price_adj = price / tc)

  temp_dl <- dl

  res_y <- functions::check_getYields(
    letras         = temp_dl$ticker,
    settlementDate = as.character(
      bizdays::offset(
        temp_dl$date,
        ifelse(settlement == "INMEDIATA", 0, 1),
        cal = cal
      )
    ),
    precios        = temp_dl$price_adj,
    initialFee     = comi,
    endpoint       = "yield"
  )

  if (!res_y$ok && is.null(res_y$data)) {
    functions::log_msg(
      paste("Abortamos proceso con linkers (yields):", res_y$msg),
      "ERROR",
      log_file = "./cierre.log"
    )

  } else {

    # acá NO existe res_y$fail (solo issues)
    if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
      functions::log_msg(
        sprintf(
          "Linkers con issues (filas con NA en rendimiento): %d",
          nrow(res_y$issues)
        ),
        "WARN",
        log_file = "./cierre.log"
      )
    } else {
      functions::log_msg(
        "Linkers OK (rendimientos sin NA).",
        "INFO",
        log_file = "./cierre.log"
      )
    }

    apr_dl <- res_y$data
    apr_dl$maturity <- as.Date(apr_dl$maturity)

    dl <- dplyr::bind_cols(temp_dl, apr_dl) %>%
      dplyr::select(-letras, -precios, -endingFee, -initialFee) %>%
      dplyr::select(-parity, -techValue, -residual) %>%
      dplyr::rename(date_vto = maturity) %>%
      dplyr::mutate(
        tem = (1 + yield)^(1/12) - 1,
        tna = ((1 + yield)^(1/2) - 1) * 2,
        group = "DL"
      )
  }
}

############################################################
# DOLLAR LINKED - DINÁMICA
############################################################

dl_dinamica <- dbExecuteQuery(
  query  = paste0(
    "select date, ticker, price from precios_dl where date >= '",
    from_dinamica, "'"
  ),
  server = server, port = port
)

# Ajuste por tipo de cambio para la dinámica
dl_dinamica <- dl_dinamica %>%
  dplyr::left_join(
    tc %>% dplyr::select(date, tc = last_mlc),
    by = "date"
  ) %>%
  dplyr::mutate(price_adj = price / tc)

temp_dl_dinamica <- dl_dinamica

res_y <- functions::check_getYields(
  letras         = temp_dl_dinamica$ticker,
  settlementDate = as.character(
    bizdays::offset(
      temp_dl_dinamica$date,
      ifelse(settlement == "INMEDIATA", 0, 1),
      cal = cal
    )
  ),
  precios        = temp_dl_dinamica$price_adj,
  initialFee     = comi,
  endpoint       = "yield"
)

if (!res_y$ok && is.null(res_y$data)) {
  functions::log_msg(
    paste("Abortamos proceso con Linkers Dinamico (yields):", res_y$msg),
    "ERROR",
    log_file = "./cierre.log"
  )

} else {

  if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
    functions::log_msg(
      sprintf(
        "Linkers dinamico con issues (filas con NA en rendimiento): %d",
        nrow(res_y$issues)
      ),
      "WARN",
      log_file = "./cierre.log"
    )
  } else {
    functions::log_msg(
      "Linkers dinamico OK (rendimientos sin NA).",
      "INFO",
      log_file = "./cierre.log"
    )
  }

  apr_dl_dinamica <- res_y$data
  apr_dl_dinamica$maturity <- as.Date(apr_dl_dinamica$maturity)

  dl_dinamica <- dplyr::bind_cols(temp_dl_dinamica, apr_dl_dinamica) %>%
    dplyr::select(-letras, -precios, -endingFee, -initialFee) %>%
    dplyr::select(-parity, -techValue, -residual) %>%
    dplyr::rename(date_vto = maturity) %>%
    dplyr::mutate(
      tem   = (1 + yield)^(1/12) - 1,
      tna   = ((1 + yield)^(1/2) - 1) * 2,
      group = "DL"
    )
}

############################################################
# GRAFs (los tuyos, sin cambios estructurales)
############################################################

g_linkers = dl %>%
  dplyr::filter(date == from | date == to) %>%
  dplyr::group_by(ticker) %>%
  ggplot(aes(x = mduration, y = yield, color = as.factor(date), group = date)) +
  theme_usado() +
  geom_point(size = 1) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = ticker), show.legend = FALSE, max.overlaps = 14) +
  scale_color_manual(name = NULL, values = .paleta) +
  scale_y_continuous(
    breaks = breaks_extended(14),
    labels = scales::percent
  ) +
  scale_x_continuous(breaks = breaks_extended(10)) +
  labs(
    title    = "CURVA DOLLAR LINKED",
    subtitle = paste0("Último dato: ", max(dl$date)),
    y        = "TIR",
    x        = "Duration Modificada",
    caption  = paste0(.pie, " en base a precios de mercado.")
  )

grabaGrafo(variable = g_linkers, path = path)

g_linkers = dl_dinamica %>%
  dplyr::select(ticker, date, yield) %>%
  tidyr::drop_na() %>%
  ggplot(aes(x = date, y = yield, color = ticker)) +
  theme_usado() +
  geom_point() +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  scale_x_date(
    date_breaks = "1 month",
    labels      = date_format("%d-%b-%Y", locale = "es"),
    expand      = c(0.07, 0.0)
  ) +
  scale_y_continuous(
    breaks = breaks_extended(10),
    labels = scales::percent,
    limits = c(-.05, 0.15)
  ) +
  labs(
    title    = "CURVA DOLLAR LINKED",
    subtitle = paste0("Último dato: ", dl_dinamica %>% dplyr::pull(date) %>% max()),
    y        = "TIR",
    x        = "",
    caption  = paste0(.pie, " en base a precios de mercado.")
  ) +
  geom_hline(yintercept = 0, color = "black") +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(ncol = 8))

grabaGrafo(variable = g_linkers, path = path)
