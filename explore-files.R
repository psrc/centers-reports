library(tidyverse)
library(openxlsx)

dir <- "C:\\Users\\CLam\\github\\centers-monitoring\\data"

rds_files <- list.files(path = dir, pattern = "\\.rds", full.names = TRUE)
files <- list.files(path = dir, pattern = "\\.x|\\.c", full.names = TRUE)

# exclude rds files that end in shape or lyr

dfs <- list()

for(f in files) {
  nm <- basename(f)
  file_nm <- str_extract(nm, ".*(?=\\.)")
  ext <- str_extract(nm, "(?<=\\.).*")
  read_func <- switch(ext, "xlsx" = "read.xlsx", "csv" = "read.csv", "rds" = "readRDS")
  
  df <- do.call(read_func, list(f))
  dfs[[file_nm]] <- df
}

# 2 centers_information
# transit_stop_data.rds Transit access

names(dfs)
glimpse(dfs[[2]])
tsd <- readRDS("C:\\Users\\CLam\\github\\centers-monitoring\\data\\transit_stop_data.rds")

# https://rfortherestofus.com/2024/06/parameterized-reporting-quarto
# https://nrennie.rbind.io/blog/parameterized-plots-reports-r-quarto/#styling-documents
# https://quarto.org/docs/output-formats/ms-word-templates.html
# https://jadeyryan.quarto.pub/rladies-abuja-quarto-params/
# 22:43 - Parameterized reports

# table Styling
# https://www.ardata.fr/en/flextable-gallery/2020-12-01-creating-theme-function/