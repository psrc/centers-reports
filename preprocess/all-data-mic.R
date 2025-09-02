# This script will create two excel files for MIC: a main dataframe with all content and a dataframe specifically for image paths (icons in flextable)

library(openxlsx)
library(tidyverse)

## main df ----

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

# df[df == "n/a"] <- NA

df <- df |> 
  left_join(df2, by = "center_name")

# munge ----

df <- df |> 
  mutate(center_type = paste("Industrial", center_type_cat, "Center")) |> 
  mutate(across(ends_with("icon"), ~str_to_lower(.x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(.x == "okay" ~ "meets the criteria", .default = .x))) |> 
  mutate(across(ends_with("icon"), ~ case_when(.x == "does not meet criteria" ~ "does not meet the criteria", .default = .x)))

df <- df |> 
  mutate(indus_emp_icon = case_when(indus_emp > .5 ~ "meets the criteria",
                                    indus_emp < .5 ~ "does not meet the criteria")) |> 
  mutate(size_icon = case_when(size_ac >= 2000 ~ "meets the criteria",
                               size_ac < 2000 ~ "does not meet the criteria")) |> 
  mutate(planned_jobs_num = as.numeric(planned_jobs_num))

# concat text ----
## existing_jobs, planned_jobs, industrial_employment, size, transit_service, core_ind_ises, ind_ret_strategies
## market_targets, subarea_plan

df_text <- df |> 
  mutate(existing_jobs = paste0("The ", center_name_clean, " currently has ", prettyNum(exist_jobs_num, big.mark = ","), " jobs which ", exist_jobs_icon, ".")) |> 
  mutate(planned_jobs = paste0("The ", center_name_clean, " is planning for ", prettyNum(planned_jobs_num, big.mark = ","), " jobs which ", planned_jobs_icon, ".")) |> 
  mutate(industrial_employment = paste0(percent(indus_emp), " of jobs are classified as industrial jobs, which ", indus_emp_icon)) |> 
  mutate(size = paste0("The ", center_name_clean, " is currently ", prettyNum(round(size_ac, 0), big.mark = ","), " acres, which ", size_icon))
