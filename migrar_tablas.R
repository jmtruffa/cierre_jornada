library(functions)

# Config origen
source_server <- "medina"
source_port <- 12259

# Config destino
dest_server <- "localhost"
dest_port <- 5432
dest_db <- "data"
dest_password <- "Postgresql-1"

# Tablas detectadas en repo + paquetes propios
tablas <- c(
  "A3500",
  "bmBCRA",
  "calendarioFeriados",
  "calendario_feriados_usa",
  "caucionesBYMA",
  "ccl",
  "comprasMULCBCRA",
  "datos_infla_be",
  "forex2",
  "historical_prices",
  "historico_bopreales",
  "historico_lecaps",
  "historico_tamar",
  "precios_bonos_cer",
  "precios_bonos_pesos",
  "precios_dl",
  "precios_intradiarios",
  "reservas_scrape",
  "riesgo_pais",
  "rofexHis",
  "sectores",
  "serieDiaria",
  "sets",
  "tamar",
  "USCPI",
  "vencTitulos",
  "bonds",
  "bond_cashflows",
  "index_types",
  "day_count_convention",
  "depositos",
  "CER",
  "IPCIndec",
  "lecaps",
  "yields_api_keys"
)

for (nombre_tabla in tablas) {
  message(sprintf("Migrando tabla: %s", nombre_tabla))
  
  tabla <- dbGetTable(
    table = nombre_tabla,
    server = source_server,
    port = source_port,
    user = "readonly_user",
    password = "solo_lee"
  )
  
  if (nombre_tabla == "forex2") {
    nombre_tabla = "forex"
  }
  
  dbWriteDF(
    table = nombre_tabla,
    df = tabla,
    port = dest_port,
    server = dest_server,
    overwrite = TRUE,
    password = dest_password,
    db = dest_db
  )
}