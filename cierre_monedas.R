# Define the ticker symbols
tickers <- c("CLPUSD=X", "USDBRL=X", "MXNUSD=X")

futures_data <- tq_get(tickers, from = "2025-03-01", to = Sys.Date() + 1)

futures_prices = futures_data[c(1,2,8)] %>% relocate(date, symbol) %>% 
   mutate(symbol = case_when(
     symbol == "CLPUSD=X" ~ "PESO CHILENO",
     symbol == "USDBRL=X" ~ "REAL BRASILEÑO",
     symbol == "MXNUSD=X" ~ "PESO MEXICANO",
    TRUE ~ symbol  # Keep any other symbols unchanged
  )) %>% 
  mutate(adjusted = case_when(
    symbol == "PESO CHILENO" ~ 1 / adjusted,
    symbol == "PESO MEXICANO" ~ ifelse(adjusted > 10, 1 / adjusted, adjusted),
    TRUE ~ adjusted
  )) %>% 
  drop_na() %>% 
  #mutate(adjusted = ifelse(symbol == "SOY", adjusted * 0.3674, adjusted)) %>% 
  group_by(symbol)

g_monedas = futures_prices %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) + 
  theme_usado() +
  geom_line(linewidth = 1) +
  #ggrepel::geom_text_repel(data = valores, show.legend = F, nudge_x = 10) +
  scale_y_continuous(breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "3 month", labels = label_date(format = "%b-%y", locale = "es")) +
  scale_color_manual(values = .paleta) +
  labs(title = "EVOLUCION MONEDAS DE LA REGION",
       subtitle = paste0("Data al: ", tail(futures_offset, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a yahoo finance"),
       x = "",
       y = "USD") +
  theme(legend.title = element_blank()) +
  facet_wrap(~symbol, scales = "free_y")

grabaGrafo(variable = g_monedas, path = path)