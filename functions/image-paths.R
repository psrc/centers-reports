# update main df with image paths to 'icon' column

add_image_paths <- function(data_path, center = params$center) {
  df <- read.xlsx(data_path) |> 
    filter(center_name == center) 
  
  img_dir <- "./images/status_icons"
  
  df <- df |> 
    pivot_longer(cols = colnames(df)[!(colnames(df) %in% "center_name")],
                 names_to = "join_field",
                 values_to = "icon_status") 
  
  df <- df |> 
    mutate(icon = case_when(icon_status == "meets the criteria" ~ file.path(img_dir, "circle-check-solid-2.png"),
                            icon_status == "does not meet the criteria" ~ file.path(img_dir,"do-not-enter-solid.png"),
                            icon_status == "needs improvement" ~ file.path(img_dir,"triangle-exclamation-solid-2.png"),
                            .default = file.path(img_dir,"hyphen-solid.png"))) 
  
  df |> 
    select(join_field, icon)
}