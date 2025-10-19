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
    log_file = "./cierre.log"
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
  functions::dbWriteDF(
    table = "historico_lecaps",
    df    = dplyr::filter(lecaps, date > max_fecha_lecap),
    server = server, port = port, append = TRUE
  )
  
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
      log_file = "./cierre.log"
    )
    # objetos vacíos y NO seguimos con yields de BOTES
    temp_bonos_pesos <- tibble::tibble()
  } else {
    if (!is.null(bonos_pesos_prices$fail) && nrow(bonos_pesos_prices$fail) > 0) {
      fails <- dplyr::bind_rows(fails, bonos_pesos_prices$fail)
    }
    
    bonos_pesos_prices_df <- bonos_pesos_prices$data
    
    functions::dbWriteDF(
      table = "precios_bonos_pesos",
      df    = dplyr::filter(bonos_pesos_prices_df, date > max_fecha_bonos_pesos),
      server = server, port = port, append = TRUE
    )
    
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
        log_file = "./cierre.log"
      )
      # no cortamos el flujo; simplemente no agregamos BOTES a la curva
    } else {
      # OJO: acá NO existe res_y$fail (solo issues)
      if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
        functions::log_msg(
          sprintf("Bonos Pesos con issues (filas con NA en rendimiento): %d", nrow(res_y$issues)),
          "WARN",
          log_file = "./cierre.log"
        )
      } else {
        functions::log_msg("Bonos Pesos OK (rendimientos sin NA).", "INFO", log_file = "./cierre.log")
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
    }
  }
}

