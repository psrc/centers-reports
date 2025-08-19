library(tidyverse)
library(rmarkdown)
library(openxlsx)

# create vector of centers
centers <- read.xlsx("data/all-data.xlsx") |>
  mutate(center_name = str_trim(center_name)) |> 
  # filter(center_name %in% c("Ballard-Interbay", "Kent MIC")) |>
  distinct(center_name) |>
  pull(center_name) |>
  as.character()

centers <- read.xlsx("data/centers_lkup.xlsx") |>
  mutate(center_name = str_trim(name_monitoring)) |> 
  filter(center_type == "mic") |> 
  # filter(center_name %in% c("Ballard-Interbay", "Kent MIC")) |>
  distinct(center_name) |>
  pull(center_name) |>
  as.character()

filenames <- str_replace_all(centers, "/", "-")

# using rmarkdown ----

# out_dir <- "outputs"
out_dir <- paste0("T:\\60day-TEMP\\christy\\centers-reports\\", paste0("outputs_", Sys.Date()))

if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

reports <- tibble(
  input = "report-mic.Rmd",
  output_file = str_glue(out_dir, "/{filenames}.docx"),
  params = map2(centers, filenames, ~list(center = .x, filename = .y))
)

pwalk(reports, ~rmarkdown::render(input = ..1, output_file = ..2, params = ..3))
