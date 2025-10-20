max_fecha_boncer = functions::dbExecuteQuery(query = "select max(date) from precios_bonos_cer", server = server, port = port) %>% pull()
methodsPPI::getPPILogin() # Vuelvo a ponerlo total chequea que esté vigente.
tickers_boncer = map_dfr(.x = "bonosCER", .f = methodsPPI::sets, server = server, port = port)
vtos = dbGetTable("vencTitulos", server = server, port = port)
tickers_boncer = tickers_boncer %>% left_join(vtos, join_by(ticker)) %>% 
  filter(vto > from)

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
  boncer = dbExecuteQuery(
    query = paste0("select date, ticker, price from precios_bonos_cer where date >= '", from, "'"), 
    server = server, port = port)
  
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

boncer_dinamica = dbExecuteQuery(query = paste0("select date, ticker, price from precios_bonos_cer where date >= '", from_dinamica, "'"), server = server, port = port)
### ahora tengo que pegarle a getyields con cada una
temp_boncer_dinamica = boncer_dinamica
res_y = functions::check_getYields(temp_boncer_dinamica$ticker,
                          settlementDate = as.character(bizdays::offset(temp_boncer_dinamica$date, ifelse(settlement == "INMEDIATA", 0, 1), cal = cal)),
                          precios = temp_boncer_dinamica$price,
                          initialFee = comi,
                          endpoint = 'yield')

if (!res_y$ok && is.null(res_y$data)) {
  functions::log_msg(
      paste("Abortamos proceso con Boncer Dinamico (yields):", res_y$msg), 
      "ERROR",
      log_file = "./cierre.log"
  )

} else {

  # OJO: acá NO existe res_y$fail (solo issues)
    if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
      functions::log_msg(
        sprintf("Boncer dinamico con issues (filas con NA en rendimiento): %d", nrow(res_y$issues)),
        "WARN",
        log_file = "./cierre.log"
      )
    } else {
      functions::log_msg("Boncer dinamico OK (rendimientos sin NA).", "INFO", log_file = "./cierre.log")
    }

    apr_boncer_dinamica = res_y$data
    apr_boncer_dinamica$maturity = as.Date(apr_boncer_dinamica$maturity)

    boncer_dinamica = dplyr::bind_cols(temp_boncer_dinamica, apr_boncer_dinamica)  %>% 
      dplyr::select(-letras, -precios, -endingFee, -initialFee) %>%
      dplyr::select(-parity, -techValue, -residual) %>%
      dplyr::rename(date_vto = maturity) %>%
      dplyr::mutate(
          tem       = (1 + yield)^(1/12) - 1,
          tna       = ((1 + yield)^(1/2) - 1) * 2,
          group     = "BONCER"
      )


  }

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
         subtitle = paste0('Último dato: ', max(bonosCER$date)),
         y = 'TIR',
         x = 'Duration Modificada',
         caption = paste0(.pie, " en base a precios de mercado."))
  
grabaGrafo(variable = g_boncer, path = path)

g_boncer_dinamica = boncer_dinamica %>% 
  select(ticker, date, yield) %>% 
    drop_na() %>% 
    ggplot(aes(x = date, y = yield, color = ticker, label = ticker)) + 
    theme_usado() +
    geom_point()+
    scale_x_date(date_breaks = "2 month", labels = date_format("%d-%b-%Y", locale = "es"),
                 expand = c(0.07,0.0)) +    
    scale_y_continuous(breaks = breaks_extended(10), labels = scales::percent) + 
                       
    labs(title = "CURVA REAL CER",
         subtitle = paste0('Último dato: ', tail(boncer_dinamica, n = 1) %>% pull(date)),
         y = 'TIR',
         x = '',
         caption = paste0(.pie, " en base a precios de mercado."))+
    geom_hline(yintercept = 0, color = 'black') +
    theme(legend.title =  element_blank()) + guides(color = guide_legend(ncol = 8))
  
  grabaGrafo(variable = g_boncer_dinamica, path = path)  