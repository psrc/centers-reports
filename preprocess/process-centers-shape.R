# This script exports two rds, one for the proposed rgc shape and another for the mic shape to be used in report-*.Rmd

library(tidyverse)
library(sf)
library(leaflet)
library(openxlsx)

# Inputs ----

wgs84 <- 4326
spn <- 2285
ctr_name_lkup <- read.xlsx("data/centers_lkup.xlsx")

# rgc ----

prop_ctrs_path <- "C:\\Users\\CLam\\Puget Sound Regional Council\\GIS - Sharing\\Projects\\Growth\\Centers\\Centers_Criteria_Report"
prop_ctrs_file <- file.path(prop_ctrs_path, "New File Geodatabase.gdb")

# un <- Sys.getenv("USERNAME")
# gis_dir <- file.path("C:/Users",str_to_lower(un),"Puget Sound Regional Council/GIS - Sharing/Projects/Transportation/RTP_2026")
# options(dplyr.summarise.inform = FALSE)

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

rgc_lkup <- ctr_name_lkup |> 
  filter(center_type == "rgc")

prgcs_01 <- prgcs |> 
  left_join(rgc_lkup, by = c("name" = "name_prop_shape"))

saveRDS(prgcs_01, "data/proposed_rgcs.rds")

# mic ----

mic_path <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

mic <- st_read(dsn = mic_path)|> 
  st_transform(wgs84)

ctr_name_lkup_mic <- read.xlsx("data/centers_lkup.xlsx") |>
  filter(center_type == "mic")

mics <- mic |>
  left_join(ctr_name_lkup_mic, by = c("mic" = "name_prop_shape"))

mic_plot <- leaflet(options = leafletOptions(zoomControl=FALSE)) |>
  addProviderTiles(providers$Esri.WorldStreetMap) |>
  addPolygons(data = mics,
              fillColor = "76787A",
              weight = 4,
              opacity = 1.0,
              color = "#EB4584",
              dashArray = "4",
              fillOpacity = 0.0)

mic_plot

saveRDS(mics, "data/mics.rds")

# test <- readRDS("data/mics.rds")
# test <- readRDS("data/proposed_rgcs.rds")