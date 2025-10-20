query = "
select	
    sd.date,
    CAST(sd.\"princPasSaldosMEDepGobierno\" / sd.\"tipoCambio\" AS NUMERIC(10,2)) AS deposUSD,
    CAST((sd.\"princPasSaldosMEDepGobierno\" / sd.\"tipoCambio\") -
        LAG(sd.\"princPasSaldosMEDepGobierno\" / sd.\"tipoCambio\") OVER (ORDER BY bb.date) AS NUMERIC(10,2)) AS varDeposUSD,
    CAST(sd.\"princPasSaldosPESOSDepGobierno\" AS NUMERIC(10,2)) AS deposPesos,
    CAST(sd.\"princPasSaldosPESOSDepGobierno\" - LAG(sd.\"princPasSaldosPESOSDepGobierno\") OVER (ORDER BY sd.date) AS NUMERIC(10,2)) AS varDeposPesos,
    CAST(bb.\"vdFeOtrasOpTNResto\" AS NUMERIC(10,2)) AS OtrasOpTesoroResto,
    /*CAST(sd.\"tipoCambio\" AS NUMERIC(10,2)) AS TC,*/
    cast(bb.\"vdFeComTN\" / sd.\"tipoCambio\" AS NUMERIC(10,2)) as comprasUSDTesoro,
    cast(sd.\"reservasIntSaldosUSDTotal\" as numeric(10,2)) as reservasBrutas
FROM \"serieDiaria\" sd
LEFT JOIN \"bmBCRA\" bb ON sd.\"date\" = bb.\"date\"
    AND bb.\"tipoSerie\" = 'D'
    AND bb.date > '2024-05-30'
WHERE sd.date > '2024-05-30' and sd.\"princPasSaldosMEDepGobierno\" is not null

"
depos = functions::dbExecuteQuery(query = query, 
                       server = server, port = port)

g_depos_gobierno = depos %>% 
  ggplot(aes(x=date, y=depospesos)) +
  theme_usado() +
  geom_col(fill = .paleta[1]) +
  labs(title = "DEPOSITOS DEL GOBIERNO",
       subtitle = paste0("Información al ", max(depos$date)),
       y = "Billones de Pesos",
       x = "Fecha",
       caption = paste0(.pie, " en base a BCRA")) +
  scale_y_continuous(labels = scales::label_currency(scale = 1/1e6), breaks = breaks_extended(10)) +
  scale_x_cont_dates(name = "", 
                     business.dates = depos$date, labels=label_date(format = "%b-%y", locale = "es"), 
                     max.major.breaks=20) 
  
grabaGrafo(variable = g_depos_gobierno, path = path)
