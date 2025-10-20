# Define the bond pairs
bond_pairs = tibble(
  bond1 = c("GD41D", "GD30D", "GD35D", "GD38D"),
  bond2 = c("AL41D", "AL30D", "AL35D", "AE38D")
)


# PPI login
methodsPPI::getPPILogin()

calculate_and_plot_spread <- function(pair, prices) {
  bond1 <- pair[["bond1"]]
  bond2 <- pair[["bond2"]]

  spread <- prices[,c(1,2,3)] %>%
    filter(ticker %in% c(bond1, bond2)) %>%
    pivot_wider(names_from = ticker, values_from = price) %>%
    mutate(
      spread = (.[[bond1]] / .[[bond2]]) - 1,
      spreadTXT = paste0(format(round(spread * 100, 2), nsmall = 2), "%")
    ) %>% 
    drop_na()
  
  plot <- spread %>%
    ggplot(aes(x = date, y = spread, label = spreadTXT)) +
    theme_usado() +
    geom_line(col = .paleta[1], linewidth = 1) +
    geom_text_repel(data = spread %>% tail(n = 1), nudge_y = -0.0, nudge_x = 10) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = scales::percent, breaks = breaks_extended(10)) +
    labs(
      title = paste0("SPREAD LEGISLACIÓN (VIA ", bond1, "-", bond2, ")"),
      subtitle = paste0("En base al precio del: ", prices %>% tail(n = 1) %>% pull(date)),
      y = "Spread",
      x = "",
      caption = paste0(.pie, " en base a precios de mercado.")
    )
  
  return(list(plot, spread))
}


# Get prices for all tickers
all_tickers <- unique(c(bond_pairs$bond1, bond_pairs$bond2))
prices <- getPPIPriceHistoryMultiple3(
  token = token$token,
  ticker = all_tickers,
  type = rep("BONOS", length(all_tickers)),
  from = "2020-09-16",
  to = Sys.Date(),
  settlement = settlement
)
# Check for fails
# prices

# Keep prices dataframe
prices <- prices[[1]]

# Iterate over bond pairs and generate spreads and plots
plots <- bond_pairs %>%
  rowwise() %>%
  mutate(plot = list(calculate_and_plot_spread(pick(c(bond1, bond2)), prices)))

# Access individual plots

plots %>%
  rowwise() %>%
  mutate(
    graba_result = list({
      ggplot_name <- paste0("g_spread_", bond1, "_", bond2)
      grabaGrafo(variable = plot[[1]], name = ggplot_name, path = path)
    })
  )
