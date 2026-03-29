vol_mulc_todos_plazos = "
SELECT 
  \"forex\".date, 
  SUM(CASE WHEN TRIM(\"currency_out\") = 'UST' THEN \"monto\" ELSE 0 END)::NUMERIC AS volumen,
  SUM(CASE WHEN TRIM(\"currency_out\") = 'USMEP' THEN \"monto\" ELSE 0 END)::NUMERIC AS volumen_usmep
FROM 
  \"forex\"
WHERE 
  TRIM(\"currency_out\") IN ('UST','USMEP')
GROUP BY 
  \"forex\".date
ORDER BY 
  \"forex\".date;
"
vol_mulc = as_tibble(functions::dbExecuteQuery(vol_mulc_todos_plazos, server = server, port = port)) %>% arrange(date)
compras_bcra = dbGetTable(table = "comprasMULCBCRA", server = server, port = port) %>% arrange(date) %>% mutate(comprasBCRA = comprasBCRA * 1e6)
mulc = vol_mulc %>% left_join(compras_bcra, by = "date") %>% mutate(vol_total = volumen + volumen_usmep, pctBCRA = (comprasBCRA / (volumen + volumen_usmep ))) 
saveRDS(mulc, file.path(path, "mulc.rds"))
functions::log_msg(
  "mulc guardado en /home/jmt/cierre-jornada/mulc.rds",
  "INFO",
  log_file = log_file
)

######## 
# Graficos

mlc_volumenMLC_long <- mulc %>%
  select(date, volumen, volumen_usmep) %>%
  pivot_longer(cols = c(volumen, volumen_usmep), 
               names_to = "type", 
               values_to = "value")
fechas = mlc_volumenMLC_long %>% distinct(date) %>% pull(date)
g_mlc_volumenMLC = suppressMessages(
  suppressWarnings(
    ggplot(mlc_volumenMLC_long, aes(x = date, y = value, fill = type)) +
      geom_bar(stat = "identity", position = "stack", width = 1) + 
      geom_point(
        data = mulc %>% select(date, pctBCRA), 
        aes(x = date, y = pctBCRA * max(mlc_volumenMLC_long$value), color = "Compras BCRA"),
        inherit.aes = FALSE, 
      ) +
      geom_point(
        data = mulc %>% select(date, pctBCRA) %>% tail(n=1),
        aes(x = date, y = pctBCRA * max(mlc_volumenMLC_long$value), color = "Compras BCRA"),
        size = 4.5,    
        inherit.aes = FALSE, 
      ) +
      geom_smooth(
        data = mulc %>% select(date, pctBCRA), 
        aes(x = date, y = pctBCRA * max(mlc_volumenMLC_long$value)),  # Specify y aesthetic here
        color = .paleta[2],  # Use a specific color instead of fill
        inherit.aes = FALSE,  # Prevent inheriting fill from the main ggplot call
        se = F
      ) +
      theme_usado() +
      scale_y_continuous(
        name = "Millones USD",
        labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ","),
        breaks_extended(10),
        sec.axis = sec_axis(
          ~ . / max(mlc_volumenMLC_long$value),
          name = "pctBCRA",
          labels = scales::percent,
          breaks = breaks_extended(10)
        )
      ) +
      scale_x_cont_dates(
        name = "",
        business.dates = fechas,
        labels = label_date(format = "%b-%y", locale = "es"),
        max.major.breaks = 20
      ) +
      scale_fill_manual(
        name = "",
        values = .paleta[1:2],  # Use two colors from your palette
        labels = c("Volumen MLC", "Volumen USMEP")  # Customize labels
      ) +
      scale_color_manual(
        name = "",
        values = .paleta[2],
        labels = "Compras BCRA"
      ) +
      labs(
        title = "VOLUMEN MULC (DIVISA Y MEP TODOS LOS PLAZOS) Y COMPRAS BCRA",
        subtitle = paste0("Compras destacada última. Datos al ", mulc %>% tail(n = 1) %>% pull(date)),
        y = "",
        x = "",
        caption = paste0(.pie, " en base a BCRA")
      )
  )
)


suppressMessages(
  grabaGrafo(variable = g_mlc_volumenMLC, path = path)
)

##########################################################
### comprasBCRA en el MLC
graf = mulc %>% 
  mutate(resul = ifelse(comprasBCRA < 0, "Ventas", "Compras")) 
graf$Fecha = graf$date
g_mlc_comprasBCRA = suppressMessages(
  suppressWarnings(
    graf %>% 
      drop_na() %>% 
      ggplot(aes(x=Fecha, y=comprasBCRA)) +
      #mpt::theme_mpt() +
      theme_outlier() +
      geom_bar(stat = "identity", aes(fill = as.factor(resul))) +
      
      geom_smooth(se = F, show.legend = F) + 
      #geom_vline(xintercept = as.Date("2023-12-10")) +
      
      scale_fill_manual(name = "", label = c("Compras", "Ventas"), values = .paleta) +
      
      scale_x_cont_dates(name = "", business.dates = graf$date, labels=label_date(format = "%b-%y", locale = "es"), max.major.breaks=20) +
      #scale_x_date(date_breaks = "1 week", date_labels = "%d-%m") +
      scale_y_continuous(labels = unit_format(unit = "M", style_negative = "parens", scale = 1e-6),
                         name = "Millones de dólares",
                         breaks = breaks_extended(8)) +
      
      labs(title = "Operación del BCRA en el MLC",
           subtitle = paste0('Datos al ', mulc %>% tail(n=1) %>% pull(date)),
           y = '',
           x = '',
           caption = paste0(.pie, " en base a BCRA")) +
      theme(legend.position = "bottom")
  )
)

suppressMessages(
  grabaGrafo(variable = g_mlc_comprasBCRA, path = path)
)



######################################################################
#### compras y volumen mensual
# la serieCal trae los datos de los días hábiles, basado en un calendario, para 
# no omitir ningun día y asegurar que si alguna de las tablas no tiene valroes para un día
# que en realidad fue hábil, se vería un NA en la tabla.
g_mlc_tablaMLC = serieCal(from = min(vol_mulc$date), to = max(vol_mulc$date), server = server, port = port) %>% 
  left_join(vol_mulc, by = "date") %>%  
  left_join(compras_bcra, by = "date") %>% 
  filter(date>="2023-12-01") %>% 
  group_by(`Año` = year(date), Mes = month(date)) %>% 
  summarise(`Compras BCRA` = sum(comprasBCRA, na.rm = T) / 1e6,
            `Volumen Divisa` = sum(volumen, na.rm = T) / 1e6,
            `Volumen MEP` = sum(volumen_usmep, na.rm = T) / 1e6,
            `Prom Dia Compras` = mean(comprasBCRA, na.rm = T) / 1e6 ,
            `Prom Dia Volumen` = mean(volumen, na.rm = T) / 1e6,
            `Prom Día MEP` = mean(volumen_usmep, na.rm = T) / 1e6
  , .groups = "drop") %>% 
  flextable::flextable() %>% 
  flextable::footnote(i = 1, 
                      j = 1, 
                      value = as_paragraph(paste0('Millones USD al: ', vol_mulc %>% tail(n=1) %>% pull(date)))) %>% 
  colformat_double(digits = 0, big.mark = ".", decimal.mark = ",") %>% 
  colformat_char(j=3) %>% 
  align(align="center", part = "header") %>% 
  align(align="center", part = "body") %>% 
  fontsize(size = 10, part = "header") %>% 
  font(fontname = "MS Sans Serif", part = "header") %>% bg(bg = "white", part = "all")
 
grabaTabla2(variable = g_mlc_tablaMLC, path = path)


###### ultima semana
g_mlc_compras_semana_mulc = serieCal(from = min(vol_mulc$date), to = max(vol_mulc$date), server = server, port = port) %>% 
  left_join(vol_mulc, by = "date") %>%  
  left_join(compras_bcra, by = "date") %>% 
  filter(date >= lubridate::floor_date(Sys.Date(), unit="week") +1) %>% 
  summarise(`Compras BCRA` = sum(comprasBCRA, na.rm = T) / 1e6,
            `Volumen Divisa` = sum(volumen, na.rm = T) / 1e6,
            `Volumen MEP` = sum(volumen_usmep, na.rm = T) / 1e6,
            `Prom Dia Compras` = mean(comprasBCRA, na.rm = T) / 1e6,
            `Prom Dia Volumen` = mean(volumen, na.rm = T) / 1e6,
            `Prom Día MEP` = mean(volumen_usmep, na.rm = T) / 1e6
  , .groups = "drop") %>% 
  flextable::flextable() %>% 
  flextable::footnote(i = 1, 
                      j = 1, 
                      value = as_paragraph(paste0('Millones USD al: ', vol_mulc %>% tail(n=1) %>% pull(date)))) %>% 
  colformat_double(digits = 0, big.mark = ".", decimal.mark = ",") %>% 
  colformat_char(j=3) %>% 
  align(align="center", part = "header") %>% 
  align(align="center", part = "body") %>% 
  fontsize(size = 10, part = "header") %>% 
  font(fontname = "MS Sans Serif", part = "header") %>% 
  bg(bg = "white", part = "all")
  
suppressMessages(
grabaTabla2(variable = g_mlc_compras_semana_mulc, path = path)
)

######################################################################
# DEMANDA PUNTUAL

fecha_max <- max(mulc$date, na.rm = TRUE)
fecha_inicio <- floor_date(fecha_max, "month") %m-% months(11)   # ej: si max es 2026-01 -> 2025-02-01

niveles_meses <- c(month(fecha_inicio):12, 1:month(fecha_max))   # ej: 2:12,1
ultimo_dia_fila <- mulc %>% 
  mutate(demanda = volumen - comprasBCRA) %>% 
  filter(date >= fecha_inicio) %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% 
  tail(n = 1) %>% 
  pull(fila)

g_mlc_demanda_mlc_puntual <- mulc %>% 
  mutate(demanda = volumen - comprasBCRA) %>% 
  filter(date >= fecha_inicio) %>% 
  group_by(mes = month(date)) %>% 
  mutate(fila = row_number()) %>% 
  mutate(mes = factor(mes, levels = niveles_meses)) %>% 
  ggplot(aes(x = fila, y = demanda, group = mes)) +
  theme_usado() +
  geom_col(aes(fill = as.factor(mes)), width = 0.5, position = position_dodge(width = 0.6)) +
  scale_y_continuous(
    name = "Millones USD",
    labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ","),
    breaks_extended(10)
  ) +
  scale_x_continuous(breaks = seq(1, 30, 1)) +
  scale_fill_manual(name = "Mes", values = colorRampPalette(.paleta)(12)) +
  labs(
    title = "DEMANDA PRIVADA DIARIA MULC (DIVISA TODOS LOS PLAZOS)",
    subtitle = paste0("VOLUMEN - COMPRAS BCRA. Datos al ", fecha_max),
    y = "",
    x = "",
    caption = paste0(.pie, " en base a BCRA")
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      size = ifelse(seq(1, 30) == ultimo_dia_fila, 14, 10),
      color = ifelse(seq(1, 30) == ultimo_dia_fila, "red", "black")
    )
  )

suppressMessages(
grabaGrafo(variable = g_mlc_demanda_mlc_puntual, path = path)
)

######################################################################
# DEMANDA ACUMULADA COMPARADA POR RUEDAS
# NUEVO FORMATO
fecha_max <- max(mulc$date, na.rm = TRUE)
fecha_inicio <- floor_date(fecha_max, "month") %m-% months(13)

meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

mulc_prepared <- mulc %>%
  filter(date >= fecha_inicio) %>%
  mutate(
    demanda = volumen - replace_na(comprasBCRA, 0),
    ym = floor_date(date, "month")
  ) %>%
  group_by(ym) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    fila = row_number(),
    demandaAc = cumsum(demanda)
  ) %>%
  ungroup()

# Descartar primer mes si está incompleto
primer_mes <- min(mulc_prepared$ym)
primer_fecha <- min(mulc_prepared$date[mulc_prepared$ym == primer_mes])
if (day(primer_fecha) > 5) {
  mulc_prepared <- mulc_prepared %>% filter(ym > primer_mes)
}

mulc_prepared <- mulc_prepared %>%
  mutate(
    mes_lbl = paste0(meses_es[month(ym)], "-", substr(year(ym), 3, 4)),
    mes_lbl = factor(mes_lbl, levels = unique(mes_lbl[order(ym)])),
    es_actual = ym == max(ym)
  )

labels_df <- mulc_prepared %>%
  group_by(mes_lbl) %>%
  filter(fila == max(fila)) %>%
  ungroup()

n_meses <- nlevels(mulc_prepared$mes_lbl)

g_mlc_demanda_mlc_acum <- suppressMessages(
  suppressWarnings(
    mulc_prepared %>%
      ggplot(aes(x = fila, y = demandaAc, group = mes_lbl, color = mes_lbl)) +
      theme_usado() +
      geom_line(aes(linewidth = es_actual)) +
      geom_point(aes(size = es_actual)) +
      scale_linewidth_manual(values = c("FALSE" = 1, "TRUE" = 2), guide = "none") +
      scale_size_manual(values = c("FALSE" = 1, "TRUE" = 1.5), guide = "none") +
      ggrepel::geom_text_repel(
        data = labels_df,
        aes(label = mes_lbl),
        nudge_x = 0.5,
        nudge_y = 0.5,
        size = 4,
        color = "black",
        show.legend = FALSE
      ) +
      scale_color_manual(name = "Mes", values = colorRampPalette(.paleta)(n_meses)) +
      scale_y_continuous(
        name = "Millones USD",
        labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ","),
        breaks = breaks_extended(10)
      ) +
      scale_x_continuous(
        breaks = seq(1, 30, 1)
      ) +
      labs(
        title = "DEMANDA ACUMULADA MULC (DIVISA TODOS LOS PLAZOS)",
        subtitle = paste0("VOLUMEN - COMPRAS BCRA. Datos al ", fecha_max),
        y = "",
        x = "Ruedas de cada mes",
        caption = paste0(.pie, " en base a BCRA")
      ) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(
          size = 10,
          color = "black"
        )
      ) +
      guides(
        color = guide_legend(nrow = 1, title = "Mes")
      )
  )
)
suppressMessages(
grabaGrafo(variable = g_mlc_demanda_mlc_acum, path = path)
)
######################################################################
# DIVISA Y MEP EN MULC
valores = mulc %>% 
  select(date, volumen, volumen_usmep) %>% tail(n=1) %>% pivot_longer(!date)
g_mlc_volumen_divisa_mep = suppressMessages(
  suppressWarnings(
    mulc %>% 
      select(date, volumen, volumen_usmep) %>% 
      pivot_longer(!date) %>%
      ggplot(aes(x=date, y=value, color = name)) + 
      theme_usado() +
      geom_point(size = 0.75, show.legend = F) +
      geom_point(data = valores, size = 4.5, show.legend = F) +
      geom_smooth(se = F)  +
      scale_y_continuous(name = "Volumen en Millones de USD", labels = label_comma(scale = 1/1e6, big.mark = ".", decimal.mark = ",")) +
      scale_color_manual(name = "Plaza", values = .paleta, labels = c("Divisa", "MEP")) +
      labs(title = "DIVISA Y MEP EN MULC",
           subtitle = paste0('Últimos valores destacados. Datos al ', mulc %>% tail(n=1) %>% pull(date)),
           y = '',
           x = '',
           caption = paste0(.pie, " en base a BCRA")) +
      theme(legend.position = "bottom")
  )
)
suppressMessages(
  grabaGrafo(variable = g_mlc_volumen_divisa_mep, path = path)
)

####
# pctMEP
g_mlc_pct_mep_mlc = suppressMessages(
  suppressWarnings(
    mulc %>% 
      mutate(pctMEP = volumen_usmep / (volumen + volumen_usmep)) %>% 
      drop_na() %>% 
      ggplot(aes(date, pctMEP)) +
      theme_usado() +
      geom_point(size=0.75) +
      geom_smooth(se = F) +
      scale_x_cont_dates(name = "", business.dates = mulc$date, labels=label_date(format = "%b-%y", locale = "es"), max.major.breaks=20) +
      scale_y_continuous(name = "Porcentaje MEP",
                         labels = label_percent()
      ) +
      #scale_x_continuous(breaks = seq(1,30, 1)) +
      scale_fill_manual(name = "Mes", values = .paleta) +
      labs(title = "MEP COMO PORCENTAJE DEL VOLUMEN TOTAL",
           subtitle = paste0('Datos al ', mulc %>% tail(n=1) %>% pull(date)),
           y = '',
           x = '',
           caption = paste0(.pie, " en base a BCRA")) +
      theme(legend.position = "bottom")
  )
)


suppressMessages(
  grabaGrafo(variable = g_mlc_pct_mep_mlc, path = path)
)

##################################
# DISTRIBUCION RUEDA MLC
g_mlc_pct_areas_mlc = suppressWarnings(
  mulc %>% 
    mutate(pctMEP =  (volumen_usmep / (volumen + volumen_usmep)),
           pctDivisa =  (volumen / (volumen + volumen_usmep))) %>% 
    
    select(date, pctMEP, pctDivisa) %>%
    pivot_longer(!date) %>% 
    drop_na() %>% 
    
    ggplot(aes(x=date, y=value, fill = name)) +
    theme_usado() +
    geom_area(stat="identity") +
    
    scale_x_cont_dates(name = "", business.dates = mulc$date, labels=label_date(format = "%b-%y", locale = "es"), max.major.breaks=20) +
    
    scale_y_continuous(name = "Total MLC",
                       labels = label_percent(),
                       breaks_extended(10)) +
    scale_fill_manual(name = NULL, values = .paleta, labels = c("% Divisa", "% MEP")) +
    labs(title = "DISTRIBUCION DE LA RUEDA MLC",
         subtitle = paste0('Datos al ', mulc %>% tail(n=1) %>% pull(date)),
         y = '',
         x = '',
         caption = paste0(.pie, " en base a BCRA")) +
    theme(legend.position = "bottom")
)


grabaGrafo(variable = g_mlc_pct_areas_mlc, path = path)
