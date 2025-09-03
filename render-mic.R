library(tidyverse)
library(rmarkdown)
library(openxlsx)

# create vector of centers
centers <- read.xlsx("data/all-data-mic.xlsx") |>
  mutate(center_name = str_trim(center_name)) |>
  distinct(center_name) |>
  pull(center_name) |>
  as.character()

centers_name_clean <- read.xlsx("data/all-data-mic.xlsx") |>
  mutate(center_name_clean = str_trim(center_name_clean)) |>
  distinct(center_name_clean) |>
  pull(center_name_clean) |>
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
  params = pmap(list(centers, filenames, centers_name_clean), function(x, y, z) list(center = x, filename = y, center_name_clean = z))
)

pwalk(reports, ~rmarkdown::render(input = ..1, output_file = ..2, params = ..3))
