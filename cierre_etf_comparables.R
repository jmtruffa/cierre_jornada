# Define the ticker symbols
tickers <- c("EWZ", "ILF", "EMB") #, "HG=F")

# Download data for all tickers and combine into a single dataframe
futures_data <- tq_get(tickers, from = "2023-12-29", to = Sys.Date() + 1)

futures_prices = futures_data[c(1,2,8)] %>% relocate(date, symbol) %>% 
  # mutate(symbol = case_when(
  #   symbol == "CL=F" ~ "WTI",
  #   symbol == "ZS=F" ~ "SOY",
  #   symbol == "HG=F" ~ "COPPER",
  #   symbol == "ZC=F" ~ "CORN",
  #   TRUE ~ symbol  # Keep any other symbols unchanged
  # )) %>% 
  drop_na() %>% 
  #mutate(adjusted = ifelse(symbol == "SOY", adjusted * 0.3674, adjusted)) %>% 
  group_by(symbol)

g_etfs = futures_prices %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) + 
  theme_usado() +
  geom_line(linewidth = 1) +
  #ggrepel::geom_text_repel(data = valores, show.legend = F, nudge_x = 10) +
  scale_y_continuous(breaks = breaks_extended(10)) +
  scale_x_date(date_breaks = "3 month", labels = label_date(format = "%b-%y", locale = "es")) +
  scale_color_manual(values = .paleta) +
  labs(title = "EVOLUCION ETF",
       subtitle = paste0("Data al: ", tail(futures_offset, n=1) %>% pull(date)),
       caption = paste0(.pie, " en base a yahoo finance"),
       x = "",
       y = "USD") +
  theme(legend.title = element_blank()) +
  facet_wrap(~symbol, scales = "free_y")

grabaGrafo(variable = g_etfs, path = path)