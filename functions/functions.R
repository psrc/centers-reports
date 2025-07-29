# update main df with image paths to 'icon' column

add_image_paths <- function(data_path, center) {
  data_df <- read.xlsx(data_path) |>
    filter(name == center)

  data_df2 <- data_df |>
    pivot_longer(cols = setdiff(colnames(data_df), "name"),
                 names_to = "join_field",
                 values_to = "value")

  if(data_df$density >= 18) {
    den_img_path <- "circle-check-solid-2.png"
  } else {
    den_img_path <- "do-not-enter-solid.png"
  }

  if(data_df$size >= 200 & data_df$size <= 600) {
    size_img_path <- "circle-check-solid-2.png"
  } else {
    size_img_path <- "do-not-enter-solid.png"
  }

  if(data_df$planned_target_dens >= 45) {
    ptden_img_path <- "circle-check-solid-2.png"
  } else {
    ptden_img_path <- "do-not-enter-solid.png"
  }

  if(data_df$mix_of_uses >= 15) {
    mix_img_path <- "circle-check-solid-2.png"
  } else {
    mix_img_path <- "do-not-enter-solid.png"
  }

  if(is.na(data_df$market_targets)) {
    market_tgt_path <- "hyphen-solid.png"
  }

  if(is.na(data_df$subarea_plan)) {
    subarea_path <- "hyphen-solid.png"
  }

  img_dir <- "./images/status_icons"
  data_df2[data_df2$join_field == 'density_acre', "icon"] <- file.path(img_dir, den_img_path)
  data_df2[data_df2$join_field == 'size', "icon"] <- file.path(img_dir, size_img_path)
  data_df2[data_df2$join_field == 'planned_target_density', "icon"] <- file.path(img_dir, ptden_img_path)
  data_df2[data_df2$join_field == 'mix_of_uses', "icon"] <- file.path(img_dir, mix_img_path)
  data_df2[data_df2$join_field == 'market_targets', "icon"] <- file.path(img_dir, market_tgt_path)
  data_df2[data_df2$join_field == 'subarea_plan', "icon"] <- file.path(img_dir, subarea_path)


  data_df2 <- data_df2 |>
    select(-value, -name)
  
  return(data_df2)
}