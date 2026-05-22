# --- 1. DOWNLOAD ÜBER 'SF' ---

 install.packages("sf")

library(tidyverse)
library(sf)

# Der offizielle ArcGIS-Pfad zum Spielplatz-Layer (ID 0)
api_url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/freizeit_natur_sport/spielangebote/MapServer/0/query?where=1=1&outFields=*&f=geojson"

# sf liest das GeoJSON direkt als fertige Tabelle ein!
koeln_spielplaetze_sf <- st_read(api_url)

# Wir machen daraus ein ganz normales, sauberes Data Frame ohne Geo-Ballast
koeln_spielplaetze_final <- as.data.frame(koeln_spielplaetze_sf)