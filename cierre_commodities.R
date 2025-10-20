# Define the ticker symbols
tickers <- c("CL=F", "ZS=F", "ZC=F") #, "HG=F")

# Download data for all tickers and combine into a single dataframe
futures_data <- tq_get(tickers, from = "2023-12-29", to = Sys.Date() + 1)

# Display the first few rows of the combined dataframe
#head(futures_data)

futures_offset  =
  futures_data[c(1,2,8)] %>% relocate(date, symbol) %>% 
  mutate(symbol = case_when(
    symbol == "CL=F" ~ "WTI",
    symbol == "ZS=F" ~ "SOY BEAN",
    symbol == "HG=F" ~ "COPPER",
    symbol == "ZC=F" ~ "CORN",
    TRUE ~ symbol  # Keep any other symbols unchanged
  )) %>% 
    group_by(symbol) %>% 
    functions::offset()

valores = futures_offset %>% filter(date == max(futures_offset$date))    
  
# futures_offset %>% 
#   ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) + 
#   theme_usado() +
#   geom_line(linewidth = 1) +
#   ggrepel::geom_text_repel(data = valores, show.legend = F, nudge_x = 10) +
#   scale_y_continuous(breaks = breaks_extended(10)) +
#   scale_x_date(date_breaks = "1 month", labels = label_date(format = "%b-%y", locale = "es")) +
#   
#   labs(title = "WTI - SOJA - COBRE",
#        subtitle = paste0("En base a futuros próximo mes. Base 100 = 29-Dic-2023. Data al: ", tail(futures_offset, n=1) %>% pull(date)),
#        caption = paste0(.pie, " en base a yahoo finance"),
#        x = "",
#        y = "INDICE USD") +
#   theme(legend.title = element_blank()) 
  #geom_vline(xintercept = as.Date("2024-11-05"))


 ## sin offset

futures_prices = futures_data[c(1,2,8)] %>% relocate(date, symbol) %>% 
  mutate(symbol = case_when(
    symbol == "CL=F" ~ "WTI",
    symbol == "ZS=F" ~ "SOY BEAN",
    symbol == "HG=F" ~ "COPPER",
    symbol == "ZC=F" ~ "CORN",
    TRUE ~ symbol  # Keep any other symbols unchanged
  )) %>% 
  drop_na() %>% 
  mutate(adjusted = case_when(
    symbol == "SOY BEAN" ~ adjusted * 0.3674,
    symbol == "CORN" ~adjusted * 0.39368, 
    TRUE ~ adjusted)) %>% 
  group_by(symbol)

g_commodities = futures_prices %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) + 
    theme_usado() +
    geom_line(linewidth = 1) +
    #ggrepel::geom_text_repel(data = valores, show.legend = F, nudge_x = 10) +
    scale_y_continuous(breaks = breaks_extended(10)) +
    scale_x_date(date_breaks = "3 month", labels = label_date(format = "%b-%y", locale = "es")) +
  scale_color_manual(values = .paleta) +
    labs(title = "MAIZ - SOJA - WTI",
         subtitle = paste0("En base a futuros próximo mes. Data al: ", tail(futures_offset, n=1) %>% pull(date)),
         caption = paste0(.pie, " en base a yahoo finance"),
         x = "",
         y = "USD") +
    theme(legend.title = element_blank()) +
    #geom_vline(xintercept = as.Date("2025-01-10"), linetype = 2) + 
  facet_wrap(~symbol, scales = "free_y")
grabaGrafo(variable = g_commodities, path = path)


     