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
  if (update) {
    functions::dbWriteDF(
    table = "historico_tamar",
    df    = dplyr::filter(letras_tamar, date > max_fecha_tamar),
    server = server, port = port, append = TRUE
    )
  }
  
  
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

  curva_pesos = curva_lecaps %>% mutate(tea = ifelse(group == "BOTES", yield, tea)) %>% select(-yield) %>% 
    rbind(curva_tamar %>% select(-tamar_prom_tna, -tamar_tem, vf = vpv)) 
}


############################################################
# Graficamos
g_tamar_tem = curva_tamar %>% 
    filter(date == from | date == to) %>% 
    filter(tem < Inf) %>% 
    ggplot(aes(x=mduration, y=tem, group = date, color = as.factor(date))) +
    theme_usado() +
    geom_point(size=1) +
    geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE, linewidth = 1) +
    ggrepel::geom_text_repel(aes(label = ticker), show.legend = F) +
    scale_color_manual(name = NULL, values = .paleta) +  
    scale_y_continuous(breaks = breaks_extended(10), 
                       labels = scales::percent,
                       #limits = c(.03, .0425)
    ) +
    scale_x_continuous(breaks = breaks_extended(10)) +
    labs(title = "CURVA TAMAR",
         subtitle = paste0('Último dato: ', max(curva_lecaps$date)),
         y = 'TEM',
         x = 'duration (días)',
         caption = paste0(.pie, " en base a precios de mercado."))
  
  
grabaGrafo(variable = g_tamar_tem, path = path)

g_tamar_tna = curva_tamar %>% 
    filter(date == from | date == to) %>% 
    filter(tem < Inf) %>% 
    ggplot(aes(x=mduration, y=tna, group = date, color = as.factor(date))) +
    theme_usado() +
    geom_point(size=1) +
    geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE, linewidth = 1) +
    ggrepel::geom_text_repel(aes(label = ticker), show.legend = F) +
    scale_color_manual(name = NULL, values = .paleta) +  
    scale_y_continuous(breaks = breaks_extended(10), 
                       labels = scales::percent,
                       #limits = c(.03, .0425)
    ) +
    scale_x_continuous(breaks = breaks_extended(10)) +
    labs(title = "CURVA TAMAR",
         subtitle = paste0('Último dato: ', max(curva_lecaps$date)),
         y = 'TNA',
         x = 'duration (días)',
         caption = paste0(.pie, " en base a precios de mercado."))
  
  
grabaGrafo(variable = g_tamar_tem, path = path)

g_curva_pesos_tem = curva_pesos %>% 
    filter(date == from | date == to) %>% 
    filter(tem < Inf) %>% 
    ggplot(aes(x=mduration, y=tem, group = date, color = as.factor(date))) +
    theme_usado() +
    geom_point(size=1) +
    
    geom_smooth(
      data = dplyr::filter(curva_pesos %>% filter(date == from | date == to), grepl("DUALES|TAMAR", group)),
      method = "lm", formula = y ~ poly(x, 2), se = FALSE,
      linewidth = 1, linetype = "dashed", show.legend = FALSE
    ) +
    geom_smooth(
      data = dplyr::filter(curva_pesos %>% filter(date == from | date == to), grepl("LECAPS|BOTES", group)),
      method = "lm", formula = y ~ poly(x, 2), se = FALSE,
      linewidth = 1, linetype = "solid", show.legend = FALSE
    )  +
    
    #geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE, linewidth = 1) +
    ggrepel::geom_text_repel(aes(label = ticker), show.legend = F, max.overlaps = 20) +
    scale_color_manual(name = NULL, values = .paleta) +  
    scale_y_continuous(breaks = breaks_extended(10), 
                       labels = scales::percent,
                       #limits = c(.03, .0425)
    ) +
    scale_x_continuous(breaks = breaks_extended(10)) +
    labs(title = "CURVA PESOS (LECAPS + DUALES + TAMAR)",
         subtitle = paste0('Último dato: ', max(curva_pesos$date)),
         y = 'TEM',
         x = 'duration (días)',
         caption = paste0(.pie, " en base a precios de mercado."))
grabaGrafo(variable = g_curva_pesos_tem, path = path)

g_curva_pesos_tna = curva_pesos %>% 
    filter(date == from | date == to) %>% 
    filter(tem < Inf) %>% 
    ggplot(aes(x=mduration, y=tna, group = date, color = as.factor(date))) +
    theme_usado() +
    geom_point(size=1) +
    
    geom_smooth(
      data = dplyr::filter(curva_pesos %>% filter(date == from | date == to), grepl("DUALES|TAMAR", group)),
      method = "lm", formula = y ~ poly(x, 2), se = FALSE,
      linewidth = 1, linetype = "dashed", show.legend = FALSE
    ) +
    geom_smooth(
      data = dplyr::filter(curva_pesos %>% filter(date == from | date == to), grepl("LECAPS|BOTES", group)),
      method = "lm", formula = y ~ poly(x, 2), se = FALSE,
      linewidth = 1, linetype = "solid", show.legend = FALSE
    )  +
    
    #geom_smooth(method = "lm", formula = y ~ poly(x,2), se=F, show.legend = FALSE, linewidth = 1) +
    ggrepel::geom_text_repel(aes(label = ticker), show.legend = F, max.overlaps = 20) +
    scale_color_manual(name = NULL, values = .paleta) +  
    scale_y_continuous(breaks = breaks_extended(10), 
                       labels = scales::percent,
                       #limits = c(.03, .0425)
    ) +
    scale_x_continuous(breaks = breaks_extended(10)) +
    labs(title = "CURVA PESOS (LECAPS + DUALES + TAMAR)",
         subtitle = paste0('Último dato: ', max(curva_pesos$date)),
         y = 'TNA',
         x = 'duration (días)',
         caption = paste0(.pie, " en base a precios de mercado."))
grabaGrafo(variable = g_curva_pesos_tna, path = path)

g_tamar_dinamica_tem = curva_tamar_dinamica %>% 
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
    
    labs(title = "CURVA TAMAR - DINAMICA",
         subtitle = paste0('Último dato: ', tail(curva_tamar_dinamica, n = 1) %>% pull(date)),
         y = 'TEM',
         x = '',
         caption = paste0(.pie, " en base a precios de mercado."))+
    theme(legend.title =  element_blank()) + guides(color = guide_legend(ncol = 14))
grabaGrafo(variable = g_tamar_dinamica_tem, path = path)  

g_tamar_dinamica_tna = curva_tamar_dinamica %>% 
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
    
    labs(title = "CURVA TAMAR - DINAMICA",
         subtitle = paste0('Último dato: ', tail(curva_tamar_dinamica, n = 1) %>% pull(date)),
         y = 'TNA',
         x = '',
         caption = paste0(.pie, " en base a precios de mercado."))+
    theme(legend.title =  element_blank()) + guides(color = guide_legend(ncol = 14))
grabaGrafo(variable = g_tamar_dinamica_tna, path = path)  