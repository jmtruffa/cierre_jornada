#######################################################################
# Lecaps
max_fecha_lecap = dbExecuteQuery(query="select max(date) from historico_lecaps", server = server, port = port) %>% pull()
lecaps_consultar = finance::getLecaps(server = server, port = port) %>% 
  mutate(
    type = ifelse(str_detect(ticker, "^S"), "LETRAS", "BONOS")
  ) %>% filter(date_vto >= max_fecha_lecap)
lecap_prices = methodsPPI::check_getPPIPrices(
  token$token, 
  ticker = lecaps_consultar$ticker, 
  type = lecaps_consultar$type,
  from = min(from, max_fecha_lecap + 1), # voy a buscar la menor de ambas fechas
  to = to, 
  settlement = settlement,
  server = server,
  port = port
)
if (!lecap_prices$ok && is.null(lecap_prices$data)) {
  functions::log_msg(
    paste("Abortamos proceso con Lecaps:", lecap_prices$msg),
    "ERROR",
    log_file = log_file
  )
  # objetos vacíos para seguir el pipeline
  lecaps        <- tibble::tibble()
  curva_lecaps  <- tibble::tibble()
} else {
  # puede haber `fail` con tickers sin datos aunque haya `data`
  if (!is.null(lecap_prices$fail) && nrow(lecap_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, lecap_prices$fail)
  }
  
  lecaps <- lecap_prices$data
  
  # actualizamos la base SOLO con lo nuevo
  if (update) {
    functions::dbWriteDF(
      table = "historico_lecaps",
      df    = dplyr::filter(lecaps, date > max_fecha_lecap),
      server = server, port = port, append = TRUE
    )
  }
  
  # ahora re-leemos desde la tabla (para cubrir gaps si la API ya no trae vencidos)
  lecaps <- functions::dbExecuteQuery(
    query  = paste0("select date, ticker, price from historico_lecaps where date >= '", from, "'"),
    server = server, port = port
  )
  
  curva_lecaps <- finance::tasasLecap(lecaps, server = server, port = port) %>%
    dplyr::mutate(group = "LECAPS")
  
  ## ---- DUALES TAMAR ----
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
        dplyr::filter(tasa_tamar, date >= date_start & date <= date_end) |> dplyr::pull(valor),
        na.rm = TRUE
      ),
      tamar_tem = ((1 + tamar_prom_tna * 32 / 365)^(365/32))^(1/12) - 1,
      vpv       = 100 * (1 + tamar_tem)^((days360(date_liq, date_vto) / 360) * 12),
      tem       = (vpv / price)^(1 / ((as.numeric(date_vto - date) / 360) * 12)) - 1,
      group     = "DUALES"
    ) %>%
    dplyr::ungroup()
  
  curva_lecaps <- dplyr::bind_rows(
    curva_lecaps,
    dplyr::select(duales, -date_start, -date_end, -tamar_prom_tna, -tamar_tem, -vpv)
  )
  
  ## ---- BONOS EN PESOS ----
  max_fecha_bonos_pesos <- functions::dbExecuteQuery(
    query = "select max(date) from precios_bonos_pesos", server = server, port = port
  ) |> dplyr::pull(max)
  
  tickers_bonos_pesos <- purrr::map_dfr(.x = "bonosPesos", .f = methodsPPI::sets, server = server, port = port)
  
  vtos <- functions::dbGetTable("vencTitulos", server = server, port = port)
  
  tickers_bonos_pesos <- dplyr::left_join(tickers_bonos_pesos, vtos, dplyr::join_by(ticker)) %>%
    dplyr::filter(vto > from)
  
  bonos_pesos_prices <- methodsPPI::check_getPPIPrices(
    token$token,
    ticker = tickers_bonos_pesos$ticker,
    type   = tickers_bonos_pesos$type,
    from   = min(from, max_fecha_bonos_pesos + 1),
    to     = to,
    settlement = settlement,
    server = server,
    port   = port
  )
  
  if (!bonos_pesos_prices$ok && is.null(bonos_pesos_prices$data)) {
    functions::log_msg(
      paste("Abortamos proceso con Bonos Pesos:", bonos_pesos_prices$msg),
      "ERROR",
      log_file = log_file
    )
    # objetos vacíos y NO seguimos con yields de BOTES
    temp_bonos_pesos <- tibble::tibble()
  } else {
    if (!is.null(bonos_pesos_prices$fail) && nrow(bonos_pesos_prices$fail) > 0) {
      fails <- dplyr::bind_rows(fails, bonos_pesos_prices$fail)
    }
    
    bonos_pesos_prices_df <- bonos_pesos_prices$data
    
    if (update) {
      functions::dbWriteDF(
        table = "precios_bonos_pesos",
        df    = dplyr::filter(bonos_pesos_prices_df, date > max_fecha_bonos_pesos),
        server = server, port = port, append = TRUE
      )
    }
    
    # re-leer desde tabla por eventuales vencidos
    bonos_pesos_prices_all <- functions::dbExecuteQuery(
      query = paste0("select date, ticker, price from precios_bonos_pesos where date >= '", from, "'"),
      server = server, port = port
    )
    temp_bonos_pesos <- bonos_pesos_prices_all
    
    # Yields (WRAPPER) — si falla, log y seguimos sin rendimientos
    res_y <- functions::check_getYields(
      letras         = temp_bonos_pesos$ticker,
      settlementDate = as.character(
        bizdays::offset(temp_bonos_pesos$date, ifelse(settlement == "INMEDIATA", 0, 1), cal = cal)
      ),
      precios        = temp_bonos_pesos$price,
      initialFee     = comi,
      endpoint       = "yield"
    )
    
    if (!res_y$ok && is.null(res_y$data)) {
      functions::log_msg(
        paste("Abortamos proceso con Bonos Pesos (yields):", res_y$msg),
        "ERROR",
        log_file = log_file
      )
      # no cortamos el flujo; simplemente no agregamos BOTES a la curva
    } else {
      # OJO: acá NO existe res_y$fail (solo issues)
      if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
        functions::log_msg(
          sprintf("Bonos Pesos con issues (filas con NA en rendimiento): %d", nrow(res_y$issues)),
          "WARN",
          log_file = log_file
        )
      } else {
        functions::log_msg("Bonos Pesos OK (rendimientos sin NA).", "INFO", log_file = log_file)
      }
      
      apr_bonos_pesos <- res_y$data
      apr_bonos_pesos$maturity <- as.Date(apr_bonos_pesos$maturity)
      
      bonos_pesos <- dplyr::bind_cols(temp_bonos_pesos, apr_bonos_pesos) %>%
        dplyr::select(-letras, -precios, -endingFee, -initialFee) %>%
        dplyr::select(-parity, -techValue, -residual) %>%
        dplyr::rename(date_vto = maturity) %>%
        dplyr::mutate(
          tem       = (1 + yield)^(1/12) - 1,
          mduration = mduration * 365,
          tna       = ((1 + yield)^(1/2) - 1) * 2,
          group     = "BOTES"
        )
      
      curva_lecaps <- dplyr::bind_rows(curva_lecaps, bonos_pesos)

      lecap_dinamica = dbExecuteQuery(query = paste0("select date, ticker, price from historico_lecaps where date >= '", from_dinamica, "'"), server = server, port = port)
      curva_lecaps_dinamica = finance::tasasLecap(lecap_dinamica, server = server, port = port)

    }
    }
  }

############################################################
# Graficamos
g_lecap_tem = curva_lecaps %>% 
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
  
  grabaGrafo(variable = g_lecap_tem, path = path)

g_lecap_tna = curva_lecaps %>%
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
    
grabaGrafo(variable = g_lecap_tna, path = path)

g_lecap_dinamica_tem = curva_lecaps_dinamica %>%
    filter(tem > 0, date>="2025-01-01") %>% 
    ggplot(aes(x = date, y = tem, color = ticker, label = ticker)) + 
    
    theme_usado() +
    geom_point() +
    geom_line(linewidth = 1) +
    #geom_smooth(se = F) +
    
    scale_x_date(date_breaks = "1 month", labels = date_format("%d-%b", locale = "es"),
                 expand = c(0.07,0.0)) +
    
    scale_y_continuous(breaks = breaks_extended(10), 
                       labels = scales::percent) +
    
    labs(title = "CURVA LECAP - DINAMICA",
         subtitle = paste0('Último dato: ', tail(curva_lecaps_dinamica, n = 1) %>% pull(date)),
         y = 'TEM',
         x = '',
         caption = paste0(.pie, " en base a precios de mercado."))+
    theme(legend.title =  element_blank()) + guides(color = guide_legend(ncol = 14))
  
  grabaGrafo(variable = g_lecap_dinamica_tem, path = path) 

g_lecap_dinamica_tna = curva_lecaps_dinamica %>%
    filter(tem > 0, date>="2025-01-01") %>% 
    ggplot(aes(x = date, y = tna, color = ticker, label = ticker)) + 
    
    theme_usado() +
    geom_point() +
    geom_line(linewidth = 1) +
    #geom_smooth(se = F) +
    
    scale_x_date(date_breaks = "1 month", labels = date_format("%d-%b", locale = "es"),
                 expand = c(0.07,0.0)) +
    
    scale_y_continuous(breaks = breaks_extended(10), 
                       labels = scales::percent) +
    
    labs(title = "CURVA LECAP - DINAMICA",
         subtitle = paste0('Último dato: ', tail(curva_lecaps_dinamica, n = 1) %>% pull(date)),
         y = 'TNA',
         x = '',
         caption = paste0(.pie, " en base a precios de mercado."))+
    theme(legend.title =  element_blank()) + guides(color = guide_legend(ncol = 14))
  
  grabaGrafo(variable = g_lecap_dinamica_tna, path = path) 
