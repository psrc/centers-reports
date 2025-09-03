# This script will create two excel files for MIC: a main dataframe with all content and a dataframe specifically for image paths (icons in flextable)

library(openxlsx)
library(tidyverse)
library(scales)

cols <- c("a", "b", "c", "d", "e", "g", "h", "i", "j", "v")

colnums <- which(letters %in% cols)

df <- read.xlsx("data/2025 Criteria Report Data MIC for Christy 8.27.25.xlsx", 
                startRow = 2,
                cols = colnums,
                rows = 1:12) |> 
  filter(!is.na(center_name)) |> 
  mutate(center_name = str_trim(center_name))

df2 <- read.xlsx("data/2025 Criteria Report Data MIC for Christy 8.27.25.xlsx",
                 sheet = 2,
                 startRow = 2,
                 cols = 1:10)|> 
  mutate(center_name = str_trim(center_name))

df3 <- read.xlsx("data/transit-service-status-mic.xlsx") |> 
  select(center_name, ends_with('clean'))|> 
  mutate(center_name = str_trim(center_name))

df4 <- read.xlsx("data/2025 Criteria Report Data MIC for Christy 8.27.25.xlsx", 
                 sheet = "Transit",
                 startRow = 2,
                 cols = c(1,3),
                 rows = 1:11) 

# df[df == "n/a"] <- NA

df <- df |> 
  left_join(df2, by = "center_name")

# munge ----

df <- df |> 
  mutate(center_type = paste("Industrial", center_type_cat, "Center")) |> 
  mutate(across(ends_with("icon"), ~str_to_lower(.x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(.x == "okay" ~ "meets the criteria", .default = .x))) |> 
  mutate(across(ends_with("icon"), ~ case_when((.x == "does not meet criteria" | str_detect(.x, "does not meet c.*")) ~ "does not meet the criteria", .default = .x)))

df <- df |> 
  mutate(indus_emp_icon = case_when(indus_emp > .5 ~ "meets the criteria",
                                    indus_emp < .5 ~ "does not meet the criteria")) |> 
  mutate(size_icon = case_when(size_ac >= 2000 ~ "meets the criteria",
                               size_ac < 2000 ~ "does not meet the criteria")) |> 
  mutate(planned_jobs_num = as.numeric(planned_jobs_num))

# concat text ----

df_text <- df |> 
  mutate(existing_jobs = paste0("The ", center_name_clean, " currently has ", prettyNum(exist_jobs_num, big.mark = ","), " jobs which ", exist_jobs_icon, ".")) |> 
  mutate(planned_jobs = paste0("The ", center_name_clean, " is planning for ", prettyNum(planned_jobs_num, big.mark = ","), " jobs which ", planned_jobs_icon, ".")) |> 
  mutate(industrial_employment = paste0(percent(indus_emp), " of jobs are classified as industrial jobs which ", indus_emp_icon, ".")) |> 
  mutate(size = paste0("The ", center_name_clean, " is currently ", prettyNum(round(size_ac, 0), big.mark = ","), " acres which ", size_icon, "."))

## transit service ----

df_text <- df_text |> 
  left_join(df3, by = "center_name") |> 
  left_join(df4, by = "center_name")

ts_dnm <- "Outside of a transit service district; to meet criteria, the jurisdiction needs to provide documentation of transportation demand management strategies and policies to reduce commute impacts."

df_text <- df_text |> 
  mutate(transit_service = case_when(
    planned_modes_clean != "" ~ paste0("The ", center_name_clean, " is within a transit service district with ", str_to_lower(existing_modes_clean), " service and is also planning for ", str_to_lower(planned_modes_clean), " service."),
    planned_modes_clean == "" ~ paste0("The ", center_name_clean, " is within a transit service district with ", str_to_lower(existing_modes_clean), " service."),
    is.na(planned_modes_clean) ~ ts_dnm
    )) |>
  mutate(transit_service = ifelse(existing_modes_clean == "" & planned_modes_clean != "", paste0("The ", center_name_clean, " is within a transit service district that is planning for ", str_to_lower(planned_modes_clean), " service."), transit_service)) |> 
  rename(ind_ret_strategies = ind_ret) |> 
  mutate(subarea_plan = str_squish(subarea_plan)) |> 
  mutate(core_ind_uses = str_c(core_ind_uses, "."),
         ind_ret_strategies = str_c(ind_ret_strategies, "."))

openxlsx::write.xlsx(df_text, "data/all-data-mic.xlsx")

## icons ----

df_icons <- df_text |> 
  select(center_name, ends_with("icon"))

df_icons2 <- df_icons |> 
  mutate(across(ends_with("icon"), ~ str_to_lower(.x))) |> 
  mutate(across(ends_with("icon"), ~ case_when((.x == "okay" | .x == "green flag") ~ "meets the criteria", .default = .x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(.x == "yellow flag" ~ "needs improvement", .default = .x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(((.x == "does not meet criteria" | str_detect(.x, "does not meet c.*")) | .x == "red flag") ~ "does not meet the criteria", .default = .x))) |> 
  mutate(transit_service_icon = ifelse(is.na(transit_service_icon) & center_name == "Sumner-Pacific", "does not meet the criteria", transit_service_icon))

df_icons3 <- df_icons2 |> 
  rename_with(~str_replace_all(., "_icon", ""), ends_with("icon")) |> 
  rename(existing_jobs = exist_jobs,
         market_targets = market_target,
         ind_ret_strategies = ind_ret,
         core_ind_uses = core_ind_use,
         industrial_employment = indus_emp,
         )

openxlsx::write.xlsx(df_icons3, "data/all-data-icons-mic.xlsx")
