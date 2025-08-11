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
  mutate(dens_existing = urban_dens,
         dens_planned = urban_dens_planned,
         size = size_urban) |> 
  mutate(dens_existing = ifelse(is.na(dens_existing), metro_dens, urban_dens),
         dens_planned = ifelse(is.na(dens_planned), metro_dens_planned, urban_dens_planned),
         size = ifelse(is.na(size_urban), size_metro, size_urban)) |> 
  select(-urban_dens, -urban_dens_planned, -size_urban, -metro_dens, -metro_dens_planned, -size_metro)

# test <- df |>
  # select(center_name, size_urban, size_metro, size)
#   select(center_name, urban_dens_planned, metro_dens_planned, dens_planned)
#   select(center_name, urban_dens, metro_dens, dens_existing)
