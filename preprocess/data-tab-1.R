library(openxlsx)
library(tidyverse)

cols <- c("b", "c", "d", "f", "i", "j", "p", "q", "r", "s", "u", "w", "x", "z") # + ab, ac

colnums <- c(which(letters %in% cols), 28, 29)

df <- read.xlsx("data/2025 Criteria Report Data RGC for Christy8.8.25.xlsx", 
          startRow = 2,
          cols = colnums) |> 
  filter(!is.na(center_name))

df[df == "n/a"] <- NA

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

# size
# transit_service
# market_targets
# subarea_plan
