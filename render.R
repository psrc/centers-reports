library(tidyverse)
library(rmarkdown)
library(quarto)
library(openxlsx)

# create vector of centers
centers <- read.xlsx("data/all-data.xlsx") |>
  # filter(center_name %in% c("SeaTac", "Renton", "Bellevue Downtown")) |>
  distinct(center_name) |>
  pull(center_name) |>
  as.character()

filenames <- str_replace_all(centers, "Seattle First Hill/Capitol Hill", "Seattle First Hill-Capitol Hill")

# using quarto ----

# reports <- tibble(
#   input = "report.qmd",
#   output_file = str_glue("{centers}.docx"),
#   execute_params = map(centers, ~list(center = .))
# )
# 
# pwalk(reports, quarto_render)


# using rmarkdown ----

out_dir <- paste0("T:\\60day-TEMP\\christy\\centers-reports\\", paste0("outputs_", Sys.Date()))

if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

reports <- tibble(
  input = "report-rgc.Rmd",
  output_file = str_glue(out_dir, "/{filenames}.docx"),
  params = map2(centers, filenames, ~list(center = .x, filename = .y))
)

pwalk(reports, ~rmarkdown::render(input = ..1, output_file = ..2, params = ..3))
