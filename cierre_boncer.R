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
  curva_boncer = tibble::tibble()
} else {
  ## si no hubo error
  # puede haber `fail` con tickers sin datos aunque haya `data`
  if (!is.null(boncer_prices$fail) && nrow(boncer_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, boncer_prices$fail)
  }

  boncer = boncer_prices$data 

  # actualizamos la base SOLO con lo nuevo
  functions::dbWriteDF(
    table = "precios_bonos_cer",
    df    = dplyr::filter(lecaps, date > max_fecha_boncer),
    server = server, port = port, append = TRUE
  )

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
        paste("Abortamos proceso con Bonos Pesos (yields):", res_y$msg), 
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
