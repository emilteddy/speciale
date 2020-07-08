# EXAMPLE: 
# pretty_plot <- ggplot(aes(x = x, y = y, color = z)) +
# geom_line() +
# epinion_color()

##### Epinion colors and corresponding hex codes ----------------------------------------------------------------------
epi_colors <- c(
  `Dark Blue`          = "#0F283C",
  `Epinion Red`        = "#FF4646",
  `Dark Purple`        = "#641E3C",
  `Clear Blue`         = "#233CA0",
  `Warm Sand`          = "#E8E1D5",
  `Light Deep Blue`    = "#68838B",
  `Light Purple`       = "#BA7384",
  `Light Blue`         = "#A7C7D7",
  `Green`              = "#004337",
  `Light Green`        = "#73A89A",
  `Gold`               = "#C18022",
  `Light Gold`         = "#EBC882"
)

##### A function that extracts the by the name of it  ---------------------------------------------------------------
epi_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (epi_colors)
  
  epi_colors[cols]
}

##### Combines colors into palettes ---------------------------------------------------------------------------------
epi_palettes <- list(
  `red`    = epi_cols("Epinion Red"),
  `main`    = epi_cols("Epinion Red", "Dark Purple", "Warm Sand", "Clear Blue", "Dark Blue"),
  `extra`   = epi_cols("Green", "Light Deep Blue", "Light Purple", "Light Blue"),
  `extra2`  = epi_cols("Light Green", "Gold", "Light Gold"),
  `full`    = epi_cols("Epinion Red", "Dark Blue", "Dark Purple", "Clear Blue", "Green",
                       "Warm Sand", "Light Deep Blue", "Light Purple", "Light Blue",
                       "Light Green", "Gold", "Light Gold")
)

##### A function to scale the individual colors -------------------------------------------------------------------
epi_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- epi_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

# EKSEMPEL: epi_pal("ekstra2")(10)

##### COLOR --------------------------------------------------------------------------------------------------------
epinion_color <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- epi_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("epi_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

##### FILL -------------------------------------------------------------------------------------------------------
epinion_fill <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- epi_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("epi_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}