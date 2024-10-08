fun_leaflet_input <- function(layers, labels) {
  # Create default leaflet basemap ####
  map <- get_leaflet_basemap()

  # Add input layers with color palettes
  for (ii in 1:length(labels)) {
    name <- labels[ii]
    map <- map %>%
      addRasterImage(spatSample(layers[[ii]], 100000, method = "regular", as.raster = TRUE), # Downsample to render faster
        colors = pal.in,
        group = name,
        opacity = 0.8,
        options = tileOptions(zIndex = 800) # Ensure raster is always on top
      ) %>%
      addLegend(
        position = "topright",
        pal = colorNumeric(pal.in, na.color = NA, domain = NULL),
        values = values(layers[[ii]]),
        title = name,
        group = name,
        opacity = 0.8
      )
  }

  # Add base layers with default color ####
  map <- map %>%
    addRasterImage(
      pu1[[1]],
      group = ELSA_text %>% filter(var == "protect_zone") %>% pull(language),
      opacity = 0.8,
      colors = pal.zone,
      options = tileOptions(zIndex = 800)
    ) %>%
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[1]]),
      labels = ELSA_text %>% filter(var == "protect_zone") %>% pull(language),
      group = ELSA_text %>% filter(var == "protect_zone") %>% pull(language),
      opacity = 0.8
    ) %>%
    addRasterImage(
      pu1[[2]],
      group = ELSA_text %>% filter(var == "restore_zone") %>% pull(language),
      opacity = 0.8,
      colors = pal.zone,
      options = tileOptions(zIndex = 800)
    ) %>%
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[2]]),
      labels = ELSA_text %>% filter(var == "restore_zone") %>% pull(language),
      group = ELSA_text %>% filter(var == "restore_zone") %>% pull(language),
      opacity = 0.8
    ) %>%
    addRasterImage(
      pu1[[3]],
      group = ELSA_text %>% filter(var == "manage_zone") %>% pull(language),
      opacity = 0.8,
      colors = pal.zone,
      options = tileOptions(zIndex = 800)
    ) %>%
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[3]]),
      labels = ELSA_text %>% filter(var == "manage_zone") %>% pull(language),
      group = ELSA_text %>% filter(var == "manage_zone") %>% pull(language),
      opacity = 0.8
    )

  # Add layers control
  overlay_groups <- c(
    labels,
    ELSA_text %>% filter(var == "protect_zone") %>% pull(language),
    ELSA_text %>% filter(var == "restore_zone") %>% pull(language),
    ELSA_text %>% filter(var == "manage_zone") %>% pull(language)
  )

  map <- map %>%
    addLayersControl(
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    hideGroup(labels[-1]) %>%
    hideGroup(ELSA_text %>% filter(var == "protect_zone") %>% pull(language)) %>%
    hideGroup(ELSA_text %>% filter(var == "restore_zone") %>% pull(language)) %>%
    hideGroup(ELSA_text %>% filter(var == "manage_zone") %>% pull(language))

  return(map)
}
