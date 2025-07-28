library(tidyverse)
library(sf)
library(leaflet)
library(openxlsx)

prop_ctrs_path <- "C:\\Users\\CLam\\Puget Sound Regional Council\\GIS - Sharing\\Projects\\Growth\\Centers\\Centers_Criteria_Report"
prop_ctrs_file <- file.path(prop_ctrs_path, "New File Geodatabase.gdb")

# un <- Sys.getenv("USERNAME")
# gis_dir <- file.path("C:/Users",str_to_lower(un),"Puget Sound Regional Council/GIS - Sharing/Projects/Transportation/RTP_2026")
# options(dplyr.summarise.inform = FALSE)

# Inputs ----

wgs84 <- 4326
spn <- 2285

prgcs <- st_read(dsn = prop_ctrs_file, layer = "Proposed_RGCs")|> 
  st_transform(wgs84)

# proposed rgcs with existing ----

# m <- leaflet(options = leafletOptions(zoomControl=FALSE)) |>
#   addProviderTiles(providers$Esri.WorldStreetMap) |>
#   addPolygons(data = prgcs,
#               fillColor = "76787A",
#               weight = 4,
#               opacity = 1.0,
#               color = "#EB4584",
#               dashArray = "4",
#               fillOpacity = 0.0)
# 
# m

# join with centers_information.csv to have matching join name
# df <- read.csv("C:\\Users\\CLam\\github\\centers-monitoring\\data\\centers_information.csv")
# openxlsx::write.xlsx(df, "data/centers_lkup.xlsx")

ctr_name_lkup <- read.xlsx("data/centers_lkup.xlsx") |> 
  filter(center_type == "rgc")
prgcs_01 <- prgcs |> 
  left_join(ctr_name_lkup, by = c("name" = "name_prop_shape"))

saveRDS(prgcs_01, "data/proposed_rgcs.rds")
test <- readRDS("data/proposed_rgcs.rds")

mic_path_01 <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?outFields=*&where=1%3D1"
mic_path_02 <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

mic <- st_read(dsn = mic_path_02)|> 
  st_transform(wgs84)

mic_plot <- leaflet(options = leafletOptions(zoomControl=FALSE)) |>
  addProviderTiles(providers$Esri.WorldStreetMap) |>
  addPolygons(data = mic,
              fillColor = "76787A",
              weight = 4,
              opacity = 1.0,
              color = "#EB4584",
              dashArray = "4",
              fillOpacity = 0.0)

mic_plot

saveRDS(mic, "data/mics.rds")
