## code to prepare `app_translations` dataset goes here

app_translations <- tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  # tabs translations
  "main_tab_translation", "Explora", "Explore", "Explora",
  "download_tab_translation", "Descarrega", "Download", "Descarga",
  "cv_tab_translation", "Validacions creuades", "Cross Validations", "Validaciones cruzadas",
  "tech_specs_tab_translation", "Especificacions tècniques", "Technical specifications", "Especificaciones técnicas",
  # variables
  "MeanTemperature", "Temperatura mitjana [°C]", "Mean Temperature [°C]", "Temperatura media [°C]",
  "MinTemperature", "Temperatura mínima [°C]", "Min Temperature [°C]", "Temperatura mínima [°C]",
  "MaxTemperature", "Temperatura màxima [°C]", "Max Temperature [°C]", "Temperatura máxima [°C]",
  "MeanRelativeHumidity", "Humitat relativa mitjana [%]", "Mean Relative Humidity [%]", "Humedad relativa media [%]",
  "MinRelativeHumidity", "Humitat relativa mínima [%]", "Min Relative Humidity [%]", "Humedad relativa mínima [%]",
  "MaxRelativeHumidity", "Humitat relativa màxima [%]", "Max Relative Humidity [%]", "Humedad relativa máxima [%]",
  "Precipitation", "Precipitació [mm]", "Precipitation [mm]", "Precipitación [mm]",
  "Radiation", "Radiació [MJ/m2]", "Radiation [MJ/m2]", "Radiación [MJ/m2]",
  "WindSpeed", "Velocitat del vent [m/s]", "Wind Speed [m/s]", "Velocidad del viento [m/s]",
  "WindDirection", "Direcció del vent [° des del N]", "Wind Direction [° from N]", "Dirección del viento [° desde N]",
  "ThermalAmplitude", "Amplitud Tèrmica [°C]", "Thermal Amplitude [°C]", "Amplitud Térmica [°C]",
  "PET", "PET [mm]", "PET [mm]", "PET [mm]",
  "RangeTemperature", "Rang de Temperatura [°C]", "Temperature range [°C]", "Rango de Temperatura [°C]",
  "RelativeHumidity", "Humitat relativa [%]", "Relative Humidity [%]", "Humedad relativa [%]",
  "TotalPrecipitation", "Precipitació Total [mm]", "Total Precipitation [mm]", "Precipitación Total [mm]",
  "StationsPrecipitation", "Precipitació Estacions [mm]", "Stations Precipitation [mm]", "Precipitación Estaciones [mm]",
  # cv stats
  "bias", "Setge", "Bias", "Sesgo",
  "relative_bias", "Sesgo relatiu", "Relative bias", "Sesgo relativo",
  "mae", "Error Absolut Mitjà", "Mean Absolute Error (MAE)", "Error Absoluto Medio (MAE)",
  "r2", "R quadrat", "R squared", "R cuadrado",
  "n_stations", "Nombre d'estacions", "Number of stations", "Número de estaciones",
  # user_inputs
  "user_var_date_title", "Mapa", "Map controls", "Mapa",
  "user_var", "Variable:", "Variable:", "Variable:",
  "user_date", "Data:", "Date:", "Fecha:",
  "user_ts_title", "Sèries temporals", "Time series controls", "Series temporales",
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

# source other data-raw scripts needed
source("data-raw/cv_assets.R")

# internal data for package
usethis::use_data(
  # app_translations
  app_translations,
  # cv json (from cv_assets.R)
  interpolators_geojson,
  # opts
  internal = TRUE, overwrite = TRUE
)
