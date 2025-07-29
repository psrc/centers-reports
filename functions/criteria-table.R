
create_table <- function(data_table) {
  std_border <- fp_border(color = "lightgray")
  
  cft <- data_table |>
    flextable() |> 
    font(fontname = "Poppins", part = "all") |> 
    bold(part = "header") |>
    vline(j = c('Criteria', 'Icon'), border = std_border, part = "all") |>
    hline(border = std_border, part = "body") |> 
    width(j=1, width = 2.5) |> # total width = 7.5in
    width(j=2, width = .5) |>
    width(j=3, width = 4.5) |> 
    align(align = "left", part = "all") |> 
    bg(bg = "lightgray", part = "header") |> 
    merge_at(i=1, j = 2:3, part = "header") |> 
    set_header_labels(Icon = "Status") |> 
    align(i = 1, j = 2, align = "center", part = "header") |>
    colformat_image(j = 2, width = .2, height = 0.2) |> 
    align(j = 2, align = "center", part = "body")
  
  cft
}