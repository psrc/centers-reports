# This script generates a table with the modes that responsible for meeting criteria

library(tidyverse)

center_info <- read.csv("C:\\Users\\CLam\\github\\centers-monitoring\\data\\centers_information.csv")
transit_stops <- readRDS('data/transit_stop_data.rds')

# criteria detailed table ----

ci <- center_info |> 
  select(name, center_type)

ts <- transit_stops |> 
  filter(stops_2025 != 0 | stops_2050 != 0) |> 
  filter(mode != "All Transit Stops") |> 
  mutate(mode = str_squish(as.character(mode))) |> 
  select(geography, geography_type, mode) |> 
  group_by(geography, geography_type) |> 
  summarise(all_modes = list(mode)) # all_modes is list in list

df <- ts |> 
  left_join(ci, by = c("geography" = "name")) |> 
  filter(!center_type %in% c("Growth", "Employment", NA))

## existing modes and planned modes ----

### planned modes by geography

ts_planned <- transit_stops |> 
  filter(stops_2025 == 0 & stops_2050 != 0) |> 
  filter(mode != "All Transit Stops")|> 
  mutate(mode = str_squish(as.character(mode))) |> 
  select(geography, geography_type, mode) |> 
  group_by(geography, geography_type) |> 
  summarise(planned_modes = list(mode))|> 
  left_join(ci, by = c("geography" = "name")) |> 
  filter(!center_type %in% c("Growth", "Employment", NA))

### existing modes by geography

ts_existing <- transit_stops |> 
  filter(stops_2025 > 0) |> 
  filter(mode != "All Transit Stops")|> 
  mutate(mode = str_squish(as.character(mode))) |> 
  select(geography, geography_type, mode) |> 
  group_by(geography, geography_type) |> 
  summarise(existing_modes = list(mode))|> 
  left_join(ci, by = c("geography" = "name")) |> 
  filter(!center_type %in% c("Growth", "Employment", NA))

### join all dfs

df_join <- df |>
  left_join(ts_existing, by = c("geography", "geography_type", "center_type")) |> 
  left_join(ts_planned, by = c("geography", "geography_type", "center_type")) |> 
  relocate(center_type, .after = geography_type)

### convert vectors to comma separated strings

d <- df_join |> 
  mutate(all_modes_text = map_chr(all_modes, ~str_c(.x, collapse = ", "))) |> 
  mutate(existing_modes_text = map_chr(existing_modes, ~str_c(.x, collapse = ", "))) |> 
  mutate(planned_modes_text = map_chr(planned_modes, ~str_c(.x, collapse = ", ")))

### identify modes that are responsible for meeting criteria 

# •	Urban centers need bus or BRT access (existing or planned, so the 2025 and 2050 data fields)
# •	Metro centers need light rail, commuter rail, ferry, or BRT  (same things-existing or planned)

urban_req <- c("Bus", "Bus Rapid Transit")
metro_req <- c("Light Rail", "Commuter Rail", "Ferry", "Bus Rapid Transit")

dd <- d |> 
  mutate(criteria_success = case_when(
    center_type == "Urban" ~ map(all_modes, ~intersect(.x, urban_req)),
    center_type == "Metro" ~ map(all_modes, ~intersect(.x, metro_req))
  )) |> 
  mutate(criteria_success_text = map_chr(criteria_success, ~str_c(.x, collapse = ", ")))

dd2 <- dd |> 
  select(geography:center_type, ends_with("text")) |> 
  rename_with(~gsub("_text", "", .x))

openxlsx::write.xlsx(dd2, "T:\\60day-TEMP\\christy\\transit-service-status.xlsx")
