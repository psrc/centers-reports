library(tidyverse)
library(rmarkdown)
library(quarto)

# create vector of centers
centers <- read.csv("C:\\Users\\CLam\\github\\centers-monitoring\\data\\centers_information.csv") |>
  filter(name %in% c("SeaTac", "Renton", "Bellevue Downtown")) |>
  # head(2) |>
  distinct(name) |>
  pull(name) |>
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
  input = "report.Rmd",
  output_file = str_glue("{centers}.docx"),
  params = map(centers, ~list(center = .))
)

pwalk(reports, ~rmarkdown::render(input = ..1, output_file = ..2, params = ..3))
