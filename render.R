library(tidyverse)
library(rmarkdown)
library(quarto)
library(openxlsx)

# create vector of centers
centers <- read.xlsx("data/all-data.xlsx") |>
  filter(center_name %in% c("SeaTac", "Renton", "Bellevue Downtown")) |>
  distinct(center_name) |>
  pull(center_name) |>
  as.character()

# using quarto ----

# reports <- tibble(
#   input = "report.qmd",
#   output_file = str_glue("{centers}.docx"),
#   execute_params = map(centers, ~list(center = .))
# )
# 
# pwalk(reports, quarto_render)


# using rmarkdown ----

reports <- tibble(
  input = "report-rgc.Rmd",
  output_file = str_glue("outputs/{centers}.docx"),
  params = map(centers, ~list(center = .))
)

pwalk(reports, ~rmarkdown::render(input = ..1, output_file = ..2, params = ..3))
