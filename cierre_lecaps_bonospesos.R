## =========================
## INIT
## =========================
fails                 <- tibble::tibble()
lecaps                <- tibble::tibble()
curva_lecaps          <- tibble::tibble()
lecap_dinamica        <- tibble::tibble()
curva_lecaps_dinamica <- tibble::tibble()

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

lecap_prices <- methodsPPI::check_getPPIPrices(
  token$token,
  ticker     = lecaps_consultar$ticker,
  type       = lecaps_consultar$type,
  from       = safe_from(from, max_fecha_lecap),
  to         = to,
  settlement = settlement,
  server     = server,
  port       = port
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

  # Releer desde DB para cubrir gaps/vencidos
  lecaps <- functions::dbExecuteQuery(
    query  = paste0("select date, ticker, price from historico_lecaps where date >= '", as.Date(from), "'"),
    server = server, port = port
  )

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

tickers_bonos_pesos <- purrr::map_dfr(.x = "bonosPesos", .f = methodsPPI::sets, server = server, port = port)
vtos <- functions::dbGetTable("vencTitulos", server = server, port = port)

tickers_bonos_pesos <- dplyr::left_join(tickers_bonos_pesos, vtos, dplyr::join_by(ticker)) %>%
  dplyr::filter(vto > as.Date(from))

bonos_pesos_prices <- methodsPPI::check_getPPIPrices(
  token$token,
  ticker     = tickers_bonos_pesos$ticker,
  type       = tickers_bonos_pesos$type,
  from       = safe_from(from, max_fecha_bonos_pesos),
  to         = to,
  settlement = settlement,
  server     = server,
  port       = port
)

if (!bonos_pesos_prices$ok && is.null(bonos_pesos_prices$data)) {
  functions::log_msg(paste("BONES PESOS: fallo descarga precios:", bonos_pesos_prices$msg), "ERROR", log_file = log_file)
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

  # Releer desde DB
  bonos_pesos_prices_all <- functions::dbExecuteQuery(
    query = paste0("select date, ticker, price from precios_bonos_pesos where date >= '", as.Date(from), "'"),
    server = server, port = port
  )

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
      functions::log_msg(paste("BONES PESOS (yields): fallo:", res_y$msg), "ERROR", log_file = log_file)
    } else {
      if (!is.null(res_y$issues) && nrow(res_y$issues) > 0) {
        functions::log_msg(sprintf("BONES PESOS: issues en yields (NA): %d", nrow(res_y$issues)), "WARN", log_file = log_file)
      } else {
        functions::log_msg("BONES PESOS: yields OK.", "INFO", log_file = log_file)
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
    }
  } else {
    functions::log_msg("BONES PESOS: releída DB vacía desde 'from'; no se calculan yields.", "WARN", log_file = log_file)
  }
}

## =========================
## 3) LECAPS DINÁMICA (solo DB → curva dinámica)
## =========================
lecap_dinamica <- functions::dbExecuteQuery(
  query  = paste0("select date, ticker, price from historico_lecaps where date >= '", as.Date(from_dinamica), "'"),
  server = server, port = port
)

if (!is.null(lecap_dinamica) && nrow(lecap_dinamica) > 0) {
  curva_lecaps_dinamica <- finance::tasasLecap(lecap_dinamica, server = server, port = port)
} else {
  functions::log_msg("LECAPS DINÁMICA: DB vacía desde 'from_dinamica'; curva_lecaps_dinamica queda vacía.", "WARN", log_file = log_file)
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