
# Define the ticker symbols
tickers <- c("^VIX", "GC=F", "^NDX", "^GSPC", "^STOXX50E", "^N225") #, "HG=F")


# Download data for all tickers and combine into a single dataframe
futures_data <- tq_get(
  tickers,
  from = "2002-12-29",
  to = Sys.Date() + 1
)


# Get latest quotes from quantmod as fallback
latest_quotes <- quantmod::getQuote(tickers) %>%
  tibble::rownames_to_column("symbol") %>%
  transmute(
    symbol,
    date = as.Date(`Trade Time`),
    quote_last = as.numeric(Last)
  )


# Build futures_prices
futures_prices <- futures_data[c(1,2,8)] %>%
  
  relocate(date, symbol) %>%
  
  # Join latest quotes.
  # full_join ensures that the latest row is added even if tq_get
  # did not create it.
  full_join(
    latest_quotes,
    by = c("symbol", "date")
  ) %>%
  
  # Use adjusted when available.
  # If adjusted is NA, use Last from quantmod::getQuote().
  mutate(
    adjusted = coalesce(adjusted, quote_last)
  ) %>%
  
  select(-quote_last) %>%
  
  mutate(symbol = case_when(
    symbol == "GC=F" ~ "GOLD",
    symbol == "^VIX" ~ "VIX",
    symbol == "^NDX" ~ "NDX",
    symbol == "^GSPC" ~ "SPX",
    symbol == "^STOXX50E" ~ "EUROSTOCK50",
    symbol == "^N225" ~ "NIKKEI",
    TRUE ~ symbol
  )) %>%
  
  # Remove only rows where adjusted is still NA
  drop_na(adjusted) %>%
  
  #mutate(adjusted = ifelse(symbol == "SOY", adjusted * 0.3674, adjusted)) %>%
  
  group_by(symbol)


futures_prices_ytd <- futures_prices %>%
  ungroup() %>%
  left_join(
    futures_prices %>%
      filter(
        lubridate::year(date) == lubridate::year(Sys.Date()) - 1 &
          lubridate::month(date) == 12
      ) %>%
      group_by(symbol) %>%
      summarise(
        last_date = max(date, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "symbol"
  ) %>%
  filter(date >= last_date) %>%
  select(-last_date)

# Último valor disponible de cada símbolo
ultimos_valores <- futures_prices %>%
  ungroup() %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(
    previo = lag(adjusted),
    rango = max(adjusted, na.rm = TRUE) - min(adjusted, na.rm = TRUE)
  ) %>%
  slice_tail(n = 1) %>%
  mutate(
    # Si viene subiendo -> label arriba
    # Si viene bajando -> label abajo
    label_y = adjusted + if_else(
      adjusted >= previo,
      rango * 0.05,
      -rango * 0.05
    ),
    
    label = scales::label_number(
      accuracy = 0.01,
      big.mark = ".",
      decimal.mark = ","
    )(adjusted)
  )

ultimos_valores_ytd <- futures_prices_ytd %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(
    previo = lag(adjusted),
    rango = max(adjusted, na.rm = TRUE) - min(adjusted, na.rm = TRUE)
  ) %>%
  slice_tail(n = 1) %>%
  mutate(
    label_y = adjusted + if_else(
      adjusted >= previo,
      rango * 0.05,
      -rango * 0.05
    ),
    label = scales::label_number(
      accuracy = 0.01,
      big.mark = ".",
      decimal.mark = ","
    )(adjusted)
  )

# -----------------------------------------------------------------------------
# LONG-TERM CHART
# -----------------------------------------------------------------------------

g_indices <- futures_prices %>% 
  
  ggplot(aes(
    x = date,
    y = adjusted,
    color = symbol,
    label = symbol
  )) + 
  
  theme_usado() +
  
  geom_line(linewidth = 1) +
  
  #ggrepel::geom_text_repel(
  #  data = valores,
  #  show.legend = FALSE,
  #  nudge_x = 10
  #) +
  
  geom_point(
    data = ultimos_valores,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  geom_label(
    data = ultimos_valores,
    aes(
      y = label_y,
      label = label
    ),
    nudge_x = -3,
    hjust = 1,
    size = 3.2,
    fill = scales::alpha("white", 0.75),
    label.size = 0.2,
    show.legend = FALSE
  ) +
  
  scale_y_continuous(
    breaks = breaks_extended(10)
  ) +
  
  scale_x_date(
    date_breaks = "4 year",
    labels = label_date(
      format = "%b-%y",
      locale = "es"
    )
  ) +
  
  scale_color_manual(
    values = grDevices::colorRampPalette(.paleta)(
      length(unique(futures_prices$symbol))
    )
  ) +
  
  labs(
    title = "EVOLUCION INDICES USA - GOLD - VIX",
    subtitle = paste0(
      "Data al: ",
      max(futures_prices$date)
    ),
    caption = paste0(
      .pie,
      " en base a yahoo finance"
    ),
    x = "",
    y = "USD"
  ) +
  
  theme(
    legend.title = element_blank()
  ) +
  
  facet_wrap(
    ~symbol,
    scales = "free_y"
  )


grabaGrafo(
  variable = g_indices,
  path = path
)


# -----------------------------------------------------------------------------
# YTD CHART
# -----------------------------------------------------------------------------

g_indices_ytd <- futures_prices_ytd %>%
  ggplot(aes(x = date, y = adjusted, color = symbol, label = symbol)) +
  theme_usado() +
  
  geom_line(linewidth = 1) +
  
  geom_point(
    data = ultimos_valores_ytd,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  geom_label(
    data = ultimos_valores_ytd,
    aes(y = label_y, label = label),
    nudge_x = -3,
    hjust = 1,
    size = 3.2,
    fill = scales::alpha("white", 0.65),
    label.size = 0.2,
    show.legend = FALSE
  ) +
  
  scale_y_continuous(
    breaks = breaks_extended(10)
  ) +
  
  scale_x_date(
    date_breaks = "2 month",
    labels = label_date(format = "%b-%y", locale = "es")
  ) +
  
  scale_color_manual(
    values = grDevices::colorRampPalette(.paleta)(
      length(unique(futures_prices$symbol))
    )
  ) +
  
  labs(
    title = "EVOLUCION INDICES USA - GOLD - VIX",
    subtitle = paste0("Data al: ", max(futures_prices$date)),
    caption = paste0(.pie, " en base a yahoo finance"),
    x = "",
    y = "USD"
  ) +
  
  theme(legend.title = element_blank()) +
  
  facet_wrap(~symbol, scales = "free_y")

grabaGrafo(
  variable = g_indices_ytd,
  path = path
)


