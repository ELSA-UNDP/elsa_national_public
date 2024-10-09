#' Function to create an interactive map with an elsa prioritisation
#'
#' @param multi_theme A logical. Whether separate prioritizations have been run per theme
#' @param elsa_hm A `SpatRaster` A list of `SpatRaster` that contains information of the normalised summed input feature data (all features) for the country, indicating the relative contribution of every planning unit to the maximum possible net benefit in the country.
#' @param heatm_lst A list of `SpatRaster` that contains information of the normalised summed input feature data (all features) PER THEME for the country, indicating the relative contribution of every planning unit to the maximum possible net benefit in the country.
#' @param rast The output of a spatial prioritisation as a `SpatRaster`
#' @param theme_tbl A `tbl_df` containing information on the themes, the names of the data in each theme, and the type of data layers
#'
#' @return A `leaflet` map 
#' @export
fun_leaflet_elsa_1 <- function(multi_theme = NULL,
                               elsa_hm = NULL,
                               heatm_lst = NULL,
                               rast = NULL,
                               theme_tbl = NULL) {
  # Create default leaflet basemap ####
  map <- get_leaflet_basemap()

  # Add ELSA features ####
  if (multi_theme) {
    theme_group <- c(
      glue(
        "ELSA {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
      ),
      glue(
        "{theme_tbl$theme} {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
      )
    )

    for (n in 1:nlyr(rast)) {
      # Get raster attributes ####
      raster_attributes <- as_tibble(terra::cats(rast[[n]])[[1]]) %>%
        filter(value %in% unique(terra::values(rast[[n]])))

      map <- map %>%
        addRasterImage(
          rast[[n]],
          colors = raster_attributes$colour,
          opacity = 0.8,
          group = theme_group[n],
          options = tileOptions(zIndex = 1000) # Ensure raster is always on top
        ) %>%
        addLegend(
          "topright",
          colors = raster_attributes$colour,
          values = raster_attributes$category,
          title = theme_group[n],
          labels = raster_attributes$label,
          group = theme_group[n]
        )
    }
  } else if (!multi_theme) {
    # Get raster attributes ####
    raster_attributes <- as_tibble(terra::cats(rast)[[1]]) %>%
      filter(value %in% unique(terra::values(rast)))

    map <- map %>%
      addRasterImage(
        rast,
        colors = raster_attributes$colour,
        opacity = 0.8,
        group = glue(
          "ELSA {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
        ),
        options = tileOptions(zIndex = 1000) # Ensure raster is always on top
      ) %>%
      addLegend(
        "topright",
        colors = raster_attributes$colour,
        values = raster_attributes$category,
        title = glue(
          "ELSA {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
        ),
        labels = raster_attributes$label
      )
  }

  # Heat maps ####
  if (terra::minmax(elsa_hm)[2] == 0) {
    # For when an heatmap only has one input layer?
    map <- map %>%
      addRasterImage(
        elsa_hm,
        colors = pal.hm[1],
        opacity = 0.8,
        group = "ELSA HM",
        options = tileOptions(zIndex = 1000) # Ensure raster is always on top
      ) %>%
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM",
        group = "ELSA HM"
      )
  } else {
    map <- map %>%
      addRasterImage(
        elsa_hm,
        colors = pal.hm,
        opacity = 0.8,
        group = "ELSA HM",
        options = tileOptions(zIndex = 1000) # Ensure raster is always on top
      ) %>%
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM",
        group = "ELSA HM"
      )
  }

  # Add theme layers ####
  for (ii in 1:nrow(theme_tbl)) {
    name <- glue("{theme_tbl$theme[ii]} HM")
    if (terra::minmax(heatm_lst[[ii]])[2] == 0) {
      map <- map %>%
        addRasterImage(
          heatm_lst[[ii]],
          colors = pal.hm[1],
          opacity = 0.8,
          group = name,
          options = tileOptions(zIndex = 1000) # Ensure raster is always on top
        ) %>%
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
          values = terra::values(heatm[[ii]]),
          title = name,
          group = name
        )
    } else {
      map <- map %>%
        addRasterImage(
          heatm_lst[[ii]],
          colors = pal.hm,
          opacity = 0.8,
          group = name,
          options = tileOptions(zIndex = 1000)
        ) %>%
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
          values = terra::values(heatm_lst[[ii]]),
          title = name,
          group = name
        )
    }
  }

  # Add protected areas layer ####
  map <- map %>%
    addRasterImage(
      PA,
      colors = "#005a32",
      opacity = 0.8,
      group = ELSA_text %>% filter(var == "protected_areas") %>% pull(language),
      options = tileOptions(zIndex = 1000)
    ) %>%
    addLegend(
      "topright",
      colors = "#005a32",
      labels = ELSA_text %>% filter(var == "protected_areas") %>% pull(language),
      group = ELSA_text %>% filter(var == "protected_areas") %>% pull(language)
    )

  # Add restoration areas layer if applicable ####
  if (restorelock) {
    map <- map %>%
      addRasterImage(
        Rest,
        colors = "#fcae16",
        opacity = 0.8,
        group = ELSA_text %>% filter(var == "restore_areas") %>% pull(language),
        options = tileOptions(zIndex = 1000)
      ) %>%
      addLegend(
        "topright",
        colors = "#fcae16",
        labels = ELSA_text %>% filter(var == "restore_areas") %>% pull(language),
        group = ELSA_text %>% filter(var == "restore_areas") %>% pull(language)
      )
  }

  # Add layers control ####
  overlay_groups <- if (multi_theme) {
    c(
      glue(
        "ELSA {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
      ),
      glue(
        "{theme_tbl$theme} {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
      ),
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      ELSA_text %>% filter(var == "protected_areas") %>% pull(language)
    )
  } else if (!multi_theme) {
    c(
      glue(
        "ELSA {ELSA_text %>% dplyr::filter(var == 'action') %>% dplyr::pull(language)}"
      ),
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      ELSA_text %>% filter(var == "protected_areas") %>% pull(language)
    )
  }

  if (restorelock) {
    overlay_groups <- c(
      overlay_groups,
      ELSA_text %>% filter(var == "restore_areas") %>% pull(language)
    )
  }

  map <- map %>%
    addLayersControl(
      overlayGroups = overlay_groups,
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    hideGroup(c(
      overlay_groups[-1],
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      ELSA_text %>% filter(var == "protected_areas") %>% pull(language)
    ))

  if (restorelock) {
    map <- map %>%
      hideGroup(ELSA_text %>% filter(var == "restore_areas") %>% pull(language))
  }

  return(map)
}
