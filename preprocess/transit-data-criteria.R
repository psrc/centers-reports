# This script generates a table with the modes that responsible for meeting criteria

library(tidyverse)
library(openxlsx)
library(stringi)

center_info <- read.xlsx("data/all-data.xlsx")
transit_stops <- readRDS('data/transit_stop_data.rds') |> 
  mutate(geography = as.character(geography)) |> 
  mutate(geography = ifelse(geography == "Kirkland Greater Downtown", "Greater Downtown Kirkland", geography)) |> 
  filter(mode != "Monorail") # it’s not explicitly mentioned in our HCT definition

# criteria detailed table ----

mode_hierarchy <- c("Light Rail", "Streetcar", "Monorail", "Commuter Rail", "Ferry", "Bus Rapid Transit", "Bus")
ci <- center_info |> 
  select(center_name, center_type)

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
  filter(str_detect(geography_type, "^Regional.*")) |> 
  relocate(center_type, .after = geography_type)

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
  filter(str_detect(geography_type, "^Regional.*"))

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
  filter(str_detect(geography_type, "^Regional.*"))

### join all dfs

df_join <- df |>
  left_join(ts_existing, by = c("geography", "geography_type", "center_type")) |> 
  left_join(ts_planned, by = c("geography", "geography_type", "center_type"))

### convert vectors to comma separated strings


### identify modes that are responsible for meeting criteria 

# •	Urban centers need bus or BRT access (existing or planned, so the 2025 and 2050 data fields)
# •	Metro centers need light rail, commuter rail, ferry, or BRT  (same things-existing or planned)

urban_req <- c("Bus Rapid Transit", "Bus")
metro_req <- c("Light Rail", "Commuter Rail", "Ferry", "Bus Rapid Transit")

dd <- df_join |> 
  mutate(criteria_success = case_when(
    center_type == "Urban" ~ map(all_modes, ~intersect(.x, urban_req)),
    center_type == "Metro" ~ map(all_modes, ~intersect(.x, metro_req))
  )) 

dd2 <- dd |> 
  mutate(existing_modes_clean = str_flatten_comma(existing_modes[[1]], ", and ")) |> 
  mutate(planned_modes_clean = ifelse(!is.null(planned_modes), str_flatten_comma(planned_modes[[1]], ", and "), NULL)) |> 
  mutate(criteria_success_clean = str_flatten_comma(criteria_success[[1]], ", and ")) |> 
  rename(center_name = geography) |> 
  select(-geography_type)

# has_plans <- quote(paste("Has existing", get("existing_modes_clean"), "and is planning for", get("planned_modes_clean"), "to serve the", get("geography"), "RGC.\nMeets criteria with", get("criteria_success_clean")))
# no_plans <- quote(paste("Has existing", get("existing_modes_clean"), "to serve the", get("geography"), "RGC.\nMeets criteria with", get("criteria_success_clean")))
# 
# dd3 <- dd2 |> 
#   mutate(status = case_when(
#     planned_modes != "NULL" ~ rlang::eval_tidy(has_plans),
#     planned_modes == "NULL" ~ rlang::eval_tidy(no_plans)
#   )) |> 
#   select(geography:center_type, ends_with('clean'), status) |> 
#   rename_with(~gsub("_clean", "", .x), ends_with("clean")) |> 
#   rename(transit_service = status)

glimpse(dd2)

openxlsx::write.xlsx(dd2, "data/transit-service-status.xlsx")

