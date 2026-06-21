library(functions)

# pass gcloud server: "Solo_lee1$"
# pass medina server: "solo_lee"

# Config origen
source_cfg <- list(
  server = "local",
  port = 5432,
  db = "data",
  user = "readonly_user",
  password = "solo_lee"
)

# Config destino
dest_cfg <- list(
  server = "localhost",
  port = 5432,
  db = "data",
  user = "postgres",
  password = "Postgresql-1"
)

# Tablas detectadas en repo + paquetes propios
# tablas <- c(
#   "A3500",
#   "bmBCRA",
#   "calendarioFeriados",
#   "calendario_feriados_usa",
#   "caucionesBYMA",
#   "ccl",
#   "comprasMULCBCRA",
#   "datos_infla_be",
#   "forex2",
#   "historical_prices",
#   "historico_bopreales",
#   "historico_lecaps",
#   "historico_tamar",
#   "precios_bonos_cer",
#   "precios_bonos_pesos",
#   "precios_dl",
#   "precios_intradiarios",
#   "reservas_scrape",
#   "riesgo_pais",
#   "rofexHis",
#   "sectores",
#   "serieDiaria",
#   "sets",
#   "tamar",
#   "USCPI",
#   "vencTitulos",
#   "bonds",
#   "bond_cashflows",
#   "index_types",
#   "day_count_convention",
#   "depositos",
#   "CER",
#   "IPCIndec",
#   "lecaps",
#   "yields_api_keys"
# )

tablas <- c(
  "outstanding_soberanos"
)
for (nombre_tabla in tablas) {
  message(sprintf("Migrando tabla: %s", nombre_tabla))
  
  source_args <- source_cfg[names(source_cfg) %in% names(formals(functions::dbGetTable))]
  tabla <- do.call(
    functions::dbGetTable,
    c(list(table = nombre_tabla), source_args)
  )

  # if (nombre_tabla == "forex2") {
  #   nombre_tabla = "forex"
  # }
  
  dest_args <- dest_cfg[names(dest_cfg) %in% names(formals(functions::dbWriteDF))]
  do.call(
    functions::dbWriteDF,
    c(list(table = nombre_tabla, df = tabla, overwrite = TRUE), dest_args)
  )
}
