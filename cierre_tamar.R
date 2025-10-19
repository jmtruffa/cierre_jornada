max_fecha_tamar = functions::dbExecuteQuery(query="select max(date) from historico_tamar", server = server, port = port) %>% pull()
methodsPPI::getPPILogin() # Vuelvo a ponerlo total chequea que esté vigente.
query = paste0("SELECT * FROM tamar")
tamar = functions::dbExecuteQuery(query = query, port = port, server = server)
tamar$type = "LETRAS"
tamar = tamar %>% filter(date_vto > from)
tamar_prices = methodsPPI::check_getPPIPrices(
  token$token, 
  ticker = tamar$ticker, 
  type = tamar$type,
  from = min(from, max_fecha_tamar), 
  to = to, 
  settlement = settlement,
  server = server,
  port = port)

if (!tamar_prices$ok && is.null(tamar_prices$data)) {
  functions::log_msg(
    "Abortamos proceso con tamar", 
    "ERROR", 
    log_file = "./cierre.log"
  )
  # objetos vacíos para seguir el pipeline
  letras_tamar = tibble::tibble()
  curva_tamar = tibble::tibble()
} else {
  ## si no hubo error
  # puede haber `fail` con tickers sin datos aunque haya `data`
  if (!is.null(tamar_prices$fail) && nrow(tamar_prices$fail) > 0) {
    fails <- dplyr::bind_rows(fails, tamar_prices$fail)
  }
  
  letras_tamar = tamar_prices$data # esto tiene el DF con los precios
    
  # actualizamos la base SOLO con lo nuevo
  functions::dbWriteDF(
    table = "historico_tamar",
    df    = dplyr::filter(lecaps, date > max_fecha_tamar),
    server = server, port = port, append = TRUE
  )
  
  # ahora re-leemos desde la tabla (para cubrir gaps si la API ya no trae vencidos)
  letras_tamar = dbExecuteQuery(
    query = paste0("select date, ticker, price from historico_tamar where date >= '", from, "'"), 
    server = server, port = port)
  curva_tamar = finance::tasasTamar(letras_tamar, server = server, port = port) %>% 
    dplyr::mutate(group = "TAMAR")

  ###
  # DINAMICA
  letras_tamar_dinamica = functions::dbExecuteQuery(
    query = paste0("select date, ticker, price from historico_tamar where date >= '", from_dinamica, "'"), server = server, port = port
  )
  curva_tamar_dinamica = finance::tasasTamar(letras_tamar_dinamica, server = server, port = port)
}



