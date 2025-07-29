library(leaflet)

create_map <- function(center_shape) {
  m <- leaflet(options = leafletOptions(zoomControl=FALSE)) |>
    addProviderTiles(providers$Esri.NatGeoWorldMap) |>
    # addProviderTiles(providers$Esri.WorldStreetMap) |>
    # addProviderTiles(providers$Stadia.StamenTonerLite) |>
    # addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(data = center_shape,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#EB4584",
                dashArray = "4",
                fillOpacity = 0.0)
}