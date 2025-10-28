# code to prepare `app_translations` dataset goes here
app_translations <- tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  # tabs translations
  "main_tab_translation", "Mapa diari", "Daily map", "Mapa diario",
  "ts_tab_translation", "Sèries temporals", "Timeseries", "Series temporales",
  "tech_specs_tab_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",
  # variables
  "Theta", "Contingut d'humitat (m3/m3)", "Soil moisture content (m3/m3)", "Contenido de humedad del suelo (m3/m3)",
  "Psi", "Potencial hídric del sòl (-MPa)", "Soil water potential (-MPa)", "Potencial hídrico del suelo (-MPa)",
  "REW", "Aigua extraïble relativa (%)", "Relative extractable water (%)", "Agua extraible relativa (%)",
  "ELW", "Aigua exportada (mm/dia)", "Exported liquid water (mm/day)", "Agua exportada (mm/día)",
  "Precipitation", "Precipitació (mm/dia)", "Precipitation (mm/day)", "Precipitación (mm/día)",
  "PET", "Evapo-transpiració potencial (mm/dia)", "Potential evapo-transpiration (mm/day)", "Evapo-transpiración potencial (mm/día)",
  "AET", "Evapo-transpiració actual (mm/dia)", "Actual evapo-transpiration (mm/day)", "Evapo-transpiración actual (mm/día)",
  "LAI", "Índex d'àrea foliar (m2/m2)", "Leaf area index (m2/m2)", "Índice de área foliar (m2/m2)",
  "DDS", "Intensitat de l'estrès (%)", "Stress intensity (%)", "Intensidad del estrés (%)",
  "LFMC", "Contingut d'humitat de el combustible viu (%)", "Live Fuel Moisture Content (%)", "Contenido de humedad del combustible vivo (%)",
  # user_inputs
  "map_controls", "Mapa", "Map controls", "Mapa",
  "user_var", "Variable:", "Variable:", "Variable:",
  "user_date", "Data:", "Date:", "Fecha:",
  "user_agg", "Afegeix per província", "Aggregate by province", "Agregar por provincia",
  "ts_controls", "Sèries temporals", "Time series controls", "Series temporales",
  "user_ts_type", "Calcular per coordenades", "Calculate for coordinates", "Calcular para coordenadas",
  "user_province", "Provincia:", "Province:", "Provincia:",
  "user_longitude", "Longitud", "Longitude", "Longitud",
  "user_latitude", "Latitud", "Latitude", "Latitud",
  "user_longitude_help", "La longitud ha d'estar entre -9.5 i 4", "Longitude must be between -9.5 and 4", "La longitud debe estar entre -9.5 y 4",
  "user_latitude_help", "La latitud ha d'estar entre 35.5 i 44", "Latitude must be between 35.5 and 44", "La latitud debe estar entre 35.5 y 44",
  "user_ts_calculate", "Calcular sèries temporals", "Calculate time series", "Calcular series temporales",
  "user_ts_refresh", "Recalcular sèries temporals", "Refresh time series", "Recalcular series temporales",
  "user_ts_refresh_calculating", "Calculant, això pot portar un temps...", "Calculating, this can take a while...", "Calculando, esto puede llevar un tiempo...",
  "user_info", "Info:", "Info:", "Info:",
  "user_info_p1", "El mapa té una resolució de 2 km²", "Map has a resolution of 2 km²", "El mapa tiene una resolucion de 2 km²",
  "user_info_p2", "Les sèries temporals es calculen a una resolució de 500 m²", "Time series are calculated at a 500 m² resolution", "Las series temporales se calculan a una resolución de 500 m²",
  # download outputs
  "download_maps_title", "Descarrega de mapas", "Maps download", "Descarga de mapas",
  "download_maps_text", "Els mapes diaris a 500 m² estan disponibles en el repositori de dades públiques de l'EMF.", "Daily maps at 500 m² resolution are available at the public EMF data repository.", "Los mapas diarios a resolucion de 500 m² están disponibles en el repositorio de datos públicos de la EMF.",
  "download_maps_link", "Repositori de mapes", "Map files repository", "Repositorio de mapas",
  "download_ts_title", "Descarrega de sèries temporals", "Time series download", "Descarga de series temporales",
  "download_ts_text", "Un cop calculada, la sèrie temporal es pot descarregar en format text (arxiu csv).", "Once calculated, time series can be downloaded in text format (csv file).", "Una vez calculada, la serie temporal se puede descargar en formato texto (archivo csv).",
  "download_ts_button", "Descarrega csv", "Dowload csv", "Descarga csv",
  # map outputs
  "map_tooltip", "Coordenades seleccionades", "Selected coordinates", "Coordenadas seleccionadas",
  # cv ui
  "cv_date", "Data:", "Date:", "Fecha:",
  "cv_var", "Variable:", "Variable:", "Variable:",
  # waiting messages
  "getting_data_for", "Obtenint dades per", "Getting data for", "Obteniendo datos para",
  "please_wait", "Per favor, espere...", "Please wait...", "Por favor, espere...",
  # alerts
  "alert_no_data_text", "per a la combinació de data i variable seleccionada", "for the selected combination of date and variable", "para la combinación de fecha y variable seleccionada",
  "alert_no_data_title", "Sense dades", "No data", "Sin datos",
  "alert_dismiss", "Tancar", "Dismiss", "Cerrar",
  # emty string
  "", "", "", ""
)

province_names <- arrow::s3_bucket(
  "forestdrought-spain-app-pngs",
  access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  scheme = "https",
  endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"),
  region = ""
) |>
  arrow::open_dataset(
    factory_options = list(
      selector_ignore_prefixes = c("daily_medfateland_bitmaps.parquet")
    )
  ) |>
  dplyr::select(provincia) |>
  dplyr::distinct() |>
  dplyr::arrange(provincia) |>
  dplyr::pull(provincia, as_vector = TRUE)

# internal data for package
usethis::use_data(
  # app_translations
  app_translations,
  # province names
  province_names,
  # opts
  internal = TRUE, overwrite = TRUE
)
