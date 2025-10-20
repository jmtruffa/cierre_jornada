# Define the ticker symbols
tickers <- c("^VIX", "GC=F", "^NDX", "^GSPC", "^STOXX50E", "^N225") #, "HG=F")

# Download data for all tickers and combine into a single dataframe
futures_data <- tq_get(tickers, from = "2002-12-29", to = Sys.Date() + 1)

futures_prices = futures_data[c(1,2,8)] %>% relocate(date, symbol) %>% 
  mutate(symbol = case_when(
    symbol == "GC=F" ~ "GOLD",
    symbol == "^VIX" ~ "VIX",
    symbol == "^NDX" ~ "NDX",
    symbol == "^GSPC" ~ "SPX",
    symbol == "^STOXX50E" ~"EUROSTOCK50",
    symbol == "^N225" ~"NIKKEI",
    TRUE ~ symbol  # Keep any other symbols unchanged
  )) %>%
  drop_na() %>% 
  #mutate(adjusted = ifelse(symbol == "SOY", adjusted * 0.3674, adjusted)) %>% 
  group_by(symbol)

g_indices = futures_prices %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) + 
  theme_usado() +
  geom_line(linewidth = 1) +
  #ggrepel::geom_text_repel(data = valores, show.legend = F, nudge_x = 10) +
  scale_y_continuous(breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "3 year", labels = label_date(format = "%b-%y", locale = "es")) +
  scale_color_manual(values = grDevices::colorRampPalette(.paleta)(length(unique(futures_prices$symbol)))) +
  labs(title = "EVOLUCION INDICES USA - GOLD - VIX",
       subtitle = paste0("Data al: ", tail(futures_offset, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a yahoo finance"),
       x = "",
       y = "USD") +
  theme(legend.title = element_blank()) +
  facet_wrap(~symbol, scales = "free_y")

grabaGrafo(variable = g_indices, path = path)

g_indices_ytd = futures_prices %>%
  # Asegurarse de que no haya agrupamiento previo
  dplyr::ungroup() %>%
  # Unir con las últimas fechas de diciembre por symbol
  left_join(
    futures_prices %>%
      filter(lubridate::year(date) == lubridate::year(Sys.Date()) - 1 & lubridate::month(date) == 12) %>%
      group_by(symbol) %>%
      summarise(last_date = max(date, na.rm = TRUE), .groups = "drop"),
    by = "symbol"
  ) %>%
  # Filtrar fechas mayores o iguales a la última fecha de diciembre por symbol
  filter(date >= last_date) %>% 
  # Eliminar la columna auxiliar last_date
  select(-last_date) %>%
  ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) + 
  theme_usado() +
  geom_line(linewidth = 1) +
  #ggrepel::geom_text_repel(data = valores, show.legend = F, nudge_x = 10) +
  scale_y_continuous(breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "1 month", labels = label_date(format = "%b-%y", locale = "es")) +
  scale_color_manual(values = grDevices::colorRampPalette(.paleta)(length(unique(futures_prices$symbol)))) +
  labs(title = "EVOLUCION INDICES USA - GOLD - VIX",
       subtitle = paste0("Data al: ", tail(futures_offset, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a yahoo finance"),
       x = "",
       y = "USD") +
  theme(legend.title = element_blank()) +
  facet_wrap(~symbol, scales = "free_y")

grabaGrafo(variable = g_indices_ytd, path = path)
