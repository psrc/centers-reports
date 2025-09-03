# This script generates a table with the modes that responsible for meeting criteria

library(tidyverse)
library(openxlsx)

# center_info <- read.xlsx("data/all-data.xlsx")
center_info <- read.xlsx("data/centers_lkup.xlsx")

transit_stops <- readRDS('data/transit_stop_data.rds') |> 
  mutate(geography = as.character(geography)) |> 
  filter(mode != "Monorail") # it’s not explicitly mentioned in our HCT definition

# criteria detailed table ----

mode_hierarchy <- c("Light Rail", "Streetcar", "Monorail", "Commuter Rail", "Ferry", "Bus Rapid Transit", "Bus")
ci <- center_info |> 
  select(center_name = name_monitoring) #center_name_og = name_prop_shape, , center_type

ts <- transit_stops |> 
  filter(stops_2025 != 0 | stops_2050 != 0) |> 
  filter(mode != "All Transit Stops") |> 
  mutate(mode = factor(mode, levels = mode_hierarchy)) |> 
  arrange(geography, mode) |> 
  mutate(mode = str_squish(as.character(mode))) |> 
  select(geography, geography_type, mode) |> 
  group_by(geography, geography_type) |> 
  summarise(all_modes = list(mode)) # all_modes is list in list

df <- ts |>
  left_join(ci, by = c("geography" = "center_name")) |>
  filter(str_detect(geography_type, "^M.*")) 


## existing modes and planned modes ----

### planned modes by geography

ts_planned <- transit_stops |> 
  filter(stops_2025 == 0 & stops_2050 != 0) |> 
  filter(mode != "All Transit Stops")|> 
  mutate(mode = factor(mode, levels = mode_hierarchy)) |> 
  arrange(geography, mode) |> 
  mutate(mode = str_squish(as.character(mode))) |> 
  select(geography, geography_type, mode) |> 
  group_by(geography, geography_type) |> 
  summarise(planned_modes = list(mode))|> 
  left_join(ci, by = c("geography" = "center_name")) |>
  filter(str_detect(geography_type, "^M.*"))

### existing modes by geography

ts_existing <- transit_stops |> 
  filter(stops_2025 > 0) |> 
  filter(mode != "All Transit Stops")|> 
  mutate(mode = factor(mode, levels = mode_hierarchy)) |> 
  arrange(geography, mode) |> 
  mutate(mode = str_squish(as.character(mode))) |> 
  select(geography, geography_type, mode) |> 
  group_by(geography, geography_type) |> 
  summarise(existing_modes = list(mode))|> 
  left_join(ci, by = c("geography" = "center_name")) |>
  filter(str_detect(geography_type, "^M.*"))

### join all dfs

df_join <- df |>
  left_join(ts_existing, by = c("geography", "geography_type")) |> 
  left_join(ts_planned, by = c("geography", "geography_type"))

### convert vectors to comma separated strings


### identify modes that are responsible for meeting criteria 

dd2 <- df_join |>
  mutate(existing_modes_clean = str_flatten_comma(existing_modes[[1]], ", and ")) |>
  mutate(planned_modes_clean = ifelse(!is.null(planned_modes), str_flatten_comma(planned_modes[[1]], ", and "), NULL)) |>
  rename(center_name = geography) |>
  select(-geography_type)

glimpse(dd2)

openxlsx::write.xlsx(dd2, "data/transit-service-status-mic.xlsx")

