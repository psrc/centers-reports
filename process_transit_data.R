# Libraries -----------------------------------------------------------------
library(tidyverse)
library(tidytransit)
library(psrcelmer)
library(sf)

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

rgc_title <- "Regional Growth Center (4/23/2024)"
mic_title <- "MIC (1/5/2024)"

gtfs_years <- c(2025, 2050)
gtfs_service <- "spring"

transit_ord <- c("All Transit Stops", "Bus", "Bus Rapid Transit", "Commuter Rail", "Ferry", "Light Rail", "Monorail", "Streetcar")
county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")

un <- Sys.getenv("USERNAME")
data_dir <- file.path("C:/Users",str_to_lower(un),"Puget Sound Regional Council", "RTP Data & Analysis - Data", "transit")

# Center Shapefiles -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  st_transform(crs = spn) |>
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) |>
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) |>
  mutate(name = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", name)) |>
  select(geography = "name") |>
  rename(geometry="Shape") |>
  mutate(geography_type = rgc_title)

rgc_names <- rgc_shape |> 
  select("geography") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

mic_shape <- st_read_elmergeo(layer_name = "micen") |> 
  st_transform(crs = spn) |>
  select(geography = "mic") |>
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) |>
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) |>
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) |>
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) |>
  select("geography") |>
  rename(geometry="Shape") |>
  mutate(geography_type = mic_title)

mic_names <- mic_shape |> 
  select("geography") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

rgc_shape <- bind_rows(rgc_shape, mic_shape)
center_names <- rgc_shape |> 
  select("geography") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

# Functions ---------------------------------------------------------------
transit_stops_by_mode <- function(year, service_change) {
  
  hct_file <- file.path(data_dir,"hct_ids.csv")
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- file.path(data_dir,"gtfs",tolower(service_change),paste0(as.character(year),".zip"))
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes"))
  
  # Load Stops
  print(str_glue("Getting the {service_change} {year} stops into a tibble." ))
  stops <- as_tibble(gtfs$stops) |> 
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("stop_id", "stop_name", "stop_lat", "stop_lon")
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & agency_id == "29" ~ "Community Transit",
      is.na(agency_name) & str_detect(route_id, "communitytransit") ~ "Community Transit",
      is.na(agency_name) & agency_id %in% c("97", "7") ~ "Everett Transit",
      is.na(agency_name) & agency_id == "1" ~ "King County Metro",
      is.na(agency_name) & agency_id %in% c("4","20") ~ "Kitsap Transit",
      is.na(agency_name) & agency_id == "23" ~ "City of Seattle",
      is.na(agency_name) & agency_id %in% c("2", "3") ~ "Pierce Transit",
      is.na(agency_name) & agency_id == "51" ~ "Amtrak",
      is.na(agency_name) & agency_id == "95" ~ "Washington State Ferries",
      is.na(agency_name) & agency_id == "96" ~ "Seattle Center Monorail",
      is.na(agency_name) & agency_id == "19" ~ "Intercity Transit",
      is.na(agency_name) & agency_id %in% c("6","40") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Trips are used to get route id onto stop times
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("trip_id", "route_id")
  
  trips <- left_join(trips, routes, by=c("route_id"))
  
  # Clean Up Stop Times to get routes and mode by stops served
  print(str_glue("Getting the {service_change} {year} stop times into a tibble to add route information." ))
  stoptimes <- as_tibble(gtfs$stop_times) |>
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("trip_id", "stop_id")
  
  # Get Mode and agency from trips to stops
  print(str_glue("Getting unique stop list by modes for the {service_change} {year}." ))
  stops_by_mode <- left_join(stoptimes, trips, by=c("trip_id")) |>
    select("stop_id", "type_code", "type_name", "agency_name") |>
    distinct()
  
  stops_by_mode <- left_join(stops_by_mode, stops, by=c("stop_id")) |>
    mutate(date=year)
  
  print(str_glue("All Done."))
  
  return(stops_by_mode)
  
}

# GTFS Data ---------------------------------------------------------------
transit_stops <- NULL
  for(y in gtfs_years) {
    s <- transit_stops_by_mode(year = y, service_change = gtfs_service) |> drop_na()
    if(is.null(transit_stops)) {transit_stops <- s} else {transit_stops <- bind_rows(transit_stops, s)}
    rm(s)
}

transit_stop_lyr <- transit_stops |> 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs=wgs84) |> 
  st_transform(spn) |>
  mutate(type_name = case_when(
    type_name == "ST Express" ~ "Bus",
    type_name == "BRT" ~ "Bus Rapid Transit",
    type_name %in% c("Monorail", "Streetcar", "Light Rail") ~ type_name,
    type_name %in% c("Passenger Ferry", "Auto Ferry") ~ "Ferry",
    type_name == "Commuter Rail" ~ type_name,
    type_name == "Bus" ~ type_name))
    
transit_modes <- transit_stop_lyr |> st_drop_geometry() |> select(mode = "type_name") |> distinct() |> arrange(mode)

# Stops by Center ---------------------------------------------------------

center_stop_data <- NULL
for (y in gtfs_years) {
  
  ts_lyr <- transit_stop_lyr |> filter(date == y)
  stop_year <- paste0("stops_", y)
  
  interim_stop_data <- NULL
  # Summarize by Center Boundary
  for (rgc in center_names) {
    print(str_glue("Summarzing stops for {rgc}."))
    g_type <- rgc_shape |> st_drop_geometry() |> filter(geography == rgc) |> select("geography_type") |> distinct() |> pull()
    
    # Stops that intersect Centers
    s_lyr <- rgc_shape |> filter(geography == rgc)
    
    j_lyr <- st_intersection(ts_lyr, s_lyr) |> 
      st_drop_geometry() |>
      select("geography", "geography_type", mode = "type_name") |>
      mutate(!!stop_year := 1) |>
      group_by(mode) |>
      summarise(!!stop_year := round(sum(.data[[stop_year]]),0)) |>
      as_tibble()
    
    s <- left_join(transit_modes, j_lyr, by=c("mode")) |> mutate(geography = rgc) |> mutate(!!stop_year := replace_na(.data[[stop_year]], 0))
    
    total <- s |> group_by(geography) |> summarise(!!stop_year := sum(.data[[stop_year]])) |> as_tibble() |> mutate(mode = "All Transit Stops")
    
    s <- bind_rows(s, total) |> mutate(mode = factor(mode, levels = transit_ord)) |> arrange(mode) |> mutate(geography_type = g_type)
    
    if (is.null(interim_stop_data)) {interim_stop_data <- s} else {interim_stop_data <- bind_rows(interim_stop_data, s)}
    rm(j_lyr, s_lyr, s, total)
  }
  
  if (is.null(center_stop_data)) {center_stop_data <- interim_stop_data} else {center_stop_data <- left_join(center_stop_data, interim_stop_data, by=c("mode", "geography", "geography_type"))}
  rm(interim_stop_data)
}

# Summarize for All Centers
center_stop_data <- center_stop_data |> select("geography", "geography_type", "mode", "stops_2025", "stops_2050")
ord <- unique(c(county_order, rgc_names, mic_names))
center_stop_data <- center_stop_data |>
  mutate(geography = factor(geography, levels = ord)) |> 
  arrange(geography, mode)

saveRDS(center_stop_data, "outputs/transit_stop_data.rds")
