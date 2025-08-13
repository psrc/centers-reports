# This script will create two excel files: a main dataframe with all content and a dataframe specifically for image paths (icons in flextable)

library(openxlsx)
library(tidyverse)

## main df ----

cols <- c("b", "c", "d", "f", "i", "j", "p", "q", "r", "s", "u", "w", "x", "z") # + ab, ac

colnums <- c(which(letters %in% cols), 28, 29)

df <- read.xlsx("data/2025 Criteria Report Data RGC for Christy8.8.25.xlsx", 
          startRow = 2,
          cols = colnums) |> 
  filter(!is.na(center_name)) |> 
  mutate(center_name = str_trim(center_name))

df2 <- read.xlsx("data/2025 Criteria Report Data RGC for Christy8.8.25.xlsx",
                 sheet = 2,
                 startRow = 2,
                 cols = 1:4)|> 
  mutate(center_name = str_trim(center_name))

df3 <- read.xlsx("data/transit-service-status.xlsx") |> 
  select(center_name, ends_with('clean'))|> 
  mutate(center_name = str_trim(center_name))

df[df == "n/a"] <- NA

df <- df |> 
  left_join(df2, by = "center_name")

df <- df |> 
  mutate(dens_existing_icon = urban_dens,
         dens_planned_icon = urban_dens_planned,
         size_icon = size_urban) |> 
  mutate(dens_existing_icon = ifelse(is.na(dens_existing_icon), metro_dens, urban_dens),
         dens_planned_icon = ifelse(is.na(dens_planned_icon), metro_dens_planned, urban_dens_planned),
         size_icon = ifelse(is.na(size_urban), size_metro, size_urban)) |> 
  select(-urban_dens, -urban_dens_planned, -size_urban, -metro_dens, -metro_dens_planned, -size_metro)

df <- df |> 
  mutate(dens_existing_icon = case_when(dens_existing_icon == "Okay" ~ "meets the criteria",
                                   dens_existing_icon == "Does Not Meet Criteria" ~ "does not meet the criteria",
                                   dens_existing_icon == "Needs Improvement" ~ "needs improvement")) |> 
  mutate(dens_planned_icon = case_when(dens_planned_icon == "Okay" ~ "meets the criteria",
                                  dens_planned_icon == "Does Not Meet Criteria" ~ "does not meet the criteria",
                                  dens_planned_icon == "Needs Improvement" ~ "needs improvement")) #|> 
  # mutate(size_icon = case_when(size_icon == "Okay" ~ "meets",
  #                              size_icon == "Does Not Meet Criteria" ~ "does not meet",
  #                              size_icon == "Needs Improvement" ~ "needs improvement"))

# test <- df |>
  # select(center_name, size_urban, size_metro, size)
#   select(center_name, urban_dens_planned, metro_dens_planned, dens_planned)
#   select(center_name, urban_dens, metro_dens, dens_existing)

df_text <- df |> 
  mutate(density_acre = paste0("The ", center_name_clean,"'s current density is ", round(exist_au, 0), " people per acre, which ", dens_existing_icon, "."),
         planned_target_density = paste0("The ", center_name_clean, "'s planned density is ", round(planned_au, 0), " people per acre, which ", dens_planned_icon, ". Reaching this density shows whether the center is effectively managing and concentrating growth within its boundaries."),
         mix_of_uses = paste0(round(as.numeric(str_extract(mix_of_uses_num, "\\d+"), 0)), "% is planned to be residential."),
         size = paste0("The ", center_name_clean, " is currently ", round(size_ac, 0), " acres.")
  )

df_text <- df_text |> 
  left_join(df3, by = "center_name")

df_text <- df_text |> 
    mutate(transit_service = case_when(
      planned_modes_clean != "" ~ paste0("The center has existing ", str_to_lower(existing_modes_clean), " service and is planning for ", str_to_lower(planned_modes_clean), " to serve the community.", "\n\n", center_type,": ", str_to_lower(criteria_success_clean)),
      planned_modes_clean == "" ~ paste0("The center has existing ", str_to_lower(existing_modes_clean), " service to serve the community.", "\n\n", center_type, ": ", str_to_lower(criteria_success_clean))
    )) |> 
  mutate(subarea_plan = str_squish(subarea_plan))


openxlsx::write.xlsx(df_text, "data/all-data.xlsx")

## icons ----

df_icons <- df_text |> 
  select(center_name, mix_of_uses_icon, dens_existing_icon, dens_planned_icon, size_icon, criteria_success_clean)

df_icons[df_icons == "Insufficient data"] <- NA

df_icons2 <- df_icons |> 
  mutate(across(ends_with("icon"), ~str_to_lower(.x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(.x == "okay" ~ "meets the criteria", .default = .x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(.x == "does not meet criteria" ~ "does not meet the criteria", .default = .x))) |> 
  mutate(transit_service = ifelse(is.null(criteria_success_clean) | is.na(criteria_success_clean), "does not meet the criteria", "meets the criteria")) |> 
  rename(mix_of_uses = mix_of_uses_icon, 
         density_acre = dens_existing_icon, 
         planned_target_density = dens_planned_icon,
         size = size_icon) |> 
  select(-criteria_success_clean)

# openxlsx::write.xlsx(df_icons2, "data/all-data-icons.xlsx")
