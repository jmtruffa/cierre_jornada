cutoff = "2023-01-01"
tickers = c("^TNX", "DX-Y.NYB")
prices = tq_get(tickers, from = cutoff, to = Sys.Date()  + 1 )

df = prices %>% 
  mutate(symbol = ifelse(symbol == "^TNX", "TNX", "DXY")) %>% 
  drop_na()

m = (max(df[df$symbol == "DXY", "adjusted"]) - min(df[df$symbol == "DXY", "adjusted"])) / (max(df[df$symbol == "TNX", "adjusted"]) - min(df[df$symbol == "TNX", "adjusted"]))   # ≈ 8.910
b = min(df[df$symbol == "DXY", "adjusted"]) - m * min(df[df$symbol == "TNX", "adjusted"])  

gdfValores = df %>% 
  group_by(symbol) %>% 
  do(tail(., n = 1)) %>% 
  mutate(adjusted_scaled = ifelse(symbol == "TNX", m * adjusted + b, adjusted))


#ambos
g_dxy_tnx = df %>% 
  ggplot(aes(x = date, color = symbol)) +
  theme_usado() +  # Assuming this is your custom theme
  geom_line(data = filter(df, symbol == "TNX"), aes(y = m * adjusted + b), linewidth = 1) +
  geom_line(data = filter(df, symbol == "DXY"), aes(y = adjusted), linewidth = 1) +
  ggrepel::geom_text_repel(
    data = gdfValores, 
    aes(
      label = ifelse(symbol == "TNX", 
                     paste0(symbol, ": ", round(adjusted, 2)),  # Original TNX value
                     paste0(symbol, ": ", round(adjusted, 2))),
      y = adjusted_scaled  # Scaled values for positioning
    ), 
    nudge_y = ifelse(gdfValores$symbol == "TNX", -1, 1),
    nudge_x = 15,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    name = "DXY",
    limits = c(80, 120),  # Fits DXY range
    breaks = seq(80, 120, by = 5),
    sec.axis = sec_axis(
      ~ (. - b) / m,  # Transform back to TNX scale
      name = "TNX",
      breaks = seq(0, 5, by = 0.5)  # Fits TNX range
    )
  ) +
  scale_color_manual(name = NULL, values = .paleta) +  # Assuming .paleta is defined
  scale_x_date(
    expand = c(.02, 10), 
    date_breaks = "3 months", 
    labels = date_format("%b\n%Y", locale = "es")
  ) +
  labs(
    title = "US DOLLAR INDEX - 10Y TREASURY YIELD",
    subtitle = paste0('Datos al ', df %>% tail(n = 1) %>% pull(date)),
    y = '',
    x = '',
    caption = paste0(.pie, " en base a Yahoo")  # Assuming .pie is defined
  )
grabaGrafo(variable = g_dxy_tnx, path = path)

# dxy
dxy = df %>% filter(symbol == "DXY")
dxyValores = gdfValores %>% filter(symbol == "DXY")
g_dxy = dxy %>% 
  ggplot(aes(x = date, color = symbol)) +
  theme_usado() +
  geom_line(aes(y = adjusted), linewidth = 1) +
  ggrepel::geom_text_repel(
    data = dxyValores, 
    aes(
      label = paste0(symbol, ": ", round(adjusted, 2)),
      y = adjusted
    ), 
    nudge_y = 0.5,
    nudge_x = 15 ,
    show.legend = F
  ) +
  scale_y_continuous(name = "DXY",
                     #limits = c(12.5, 130),
                     breaks_extended(20)
  ) +
  scale_color_manual(name = NULL, values = .paleta) +
  scale_x_date(expand = c(.02,10), 
               date_breaks="2 months", labels=date_format("%b\n%Y", locale = "es")) +
  labs(title = "US DOLLAR INDEX",
       subtitle = paste0('Datos al ', dxy %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a Yahoo"))

grabaGrafo(variable = g_dxy, path = path)

#tnx
tnx = df %>% filter(symbol == "TNX")
tnxValores = tnx %>% tail(n=1)
g_tnx = tnx %>% 
  ggplot(aes(x = date, color = symbol)) +
  theme_usado() +
  geom_line(aes(y = adjusted), linewidth = 1) +
  ggrepel::geom_text_repel(
    data = tnxValores, 
    aes(
      label = paste0(symbol, ": ", round(adjusted, 2)),
      y = adjusted
    ), 
    nudge_y = -0.05,
    nudge_x = 15 ,
    show.legend = F
  ) +
  scale_y_continuous(name = "TNX",
                     breaks_extended(20)
  ) +
  scale_color_manual(name = NULL, values = .paleta) +
  scale_x_date(expand = c(.02,10), 
               date_breaks="2 months", labels=date_format("%b\n%Y", locale = "es")) +
  labs(title = "10Y TREASURY RATE",
       subtitle = paste0('Datos al ', tnx %>% tail(n=1) %>% pull(date)),
       y = '',
       x = '',
       caption = paste0(.pie, " en base a Yahoo"))

grabaGrafo(variable = g_tnx, path = path)

