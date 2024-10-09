#' Function to create a categorical raster based on an input raster
#'
#' @param in_rast A `SpatRaster` that is to be divided into categories
#'
#' @return A `SpatRaster` with categorical data
#' @export
make_elsa_categorical_raster <- function(in_rast) {
  # Create a tibble with the categories
  elsa_categories <- tibble(
    value = seq(1, nlyr(in_rast), 1),
    action = names(in_rast)
  )

  r_in <- in_rast
  r_in[r_in == 0] <- NA

  # Convert to categorical raster for mapping
  elsa_categorical_raster <- prioritizr::category_layer(r_in)

  unique_vals <- unique(elsa_categories$action)

  # Ensure the unique values exist in the global palette
  if (!all(unique_vals %in% pal.elsa$category)) {
    stop("Some categories in the raster do not have corresponding colors in `pal.elsa`.")
  }

  raster_attributes <- pal.elsa %>%
    filter(category %in% unique_vals) %>%
    arrange(factor(category, levels = unique_vals)) %>% # Arrange to match raster order
    mutate(category_lower = tolower(category)) %>% # Convert category to lowercase for matching
    left_join(ELSA_text, by = c("category_lower" = "var")) %>%
    left_join(elsa_categories, by = c("category" = "action")) %>%
    select(value, colour, category, label = !!sym(language)) %>%
    data.frame()

  levels(elsa_categorical_raster) <- raster_attributes

  activeCat(elsa_categorical_raster) <- 2 # Set to label category, e.g., term in the required language

  return(elsa_categorical_raster)
}
