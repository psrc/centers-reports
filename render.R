library(quarto)
library(tidyverse)

# create vector of centers
centers <- read.csv("C:\\Users\\CLam\\github\\centers-monitoring\\data\\centers_information.csv") |> 
  head(2) |> 
  distinct(name) |> 
  pull(name) |> 
  as.character()

reports <- tibble(
  input = "report.qmd",
  output_file = str_glue("{centers}.docx"),
  execute_params = map(centers, ~list(center = .))
)

pwalk(reports, quarto_render)