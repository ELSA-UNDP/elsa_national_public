library(leaflet)
library(dplyr)
library(glue)

fun_leaflet_elsa_0 <- function(elsa_hm = NULL,
                               heatm_lst = NULL,
                               theme_tbl = NULL) {
  # Create default leaflet basemap ####
  map <- get_leaflet_basemap()
  
  # Add the main feature layer
  if (terra::minmax(elsa_hm)[2] == 0) {
    map <- map |>
      addRasterImage(elsa_hm,
                     colors = pal.hm[1],
                     opacity = 0.8,
                     group = "ELSA HM",
                     options = tileOptions(zIndex = 800)
                     ) |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM"
      )
  } else {
    map <- map |>
      addRasterImage(elsa_hm,
                     colors = pal.hm,
                     opacity = 0.8,
                     group = "ELSA HM",
                     options = tileOptions(zIndex = 800)) |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM"
      )
  }
  
  # Add theme layers
  for (ii in 1:nrow(theme_tbl)) {
    name <- glue("{theme_tbl$theme[ii]} HM")
    if (terra::minmax(heatm_lst[[ii]])[2] == 0) {
      map <- map |>
        addRasterImage(heatm_lst[[ii]],
                       colors = pal.hm[1],
                       opacity = 0.8,
                       group = name,
                       options = tileOptions(zIndex = 800)
                       ) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
          values = terra::values(heatm_lst[[ii]]),
          title = name,
          group = name
        )
    } else {
      map <- map |>
        addRasterImage(heatm_lst[[ii]],
                       colors = pal.hm,
                       opacity = 0.8,
                       group = name,
                       options = tileOptions(zIndex = 800)
                       ) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
          values = terra::values(heatm_lst[[ii]]),
          title = name,
          group = name,
          bins = c(0, 0.25, 0.5, 0.75, 1)
        )
    }
  }
  
  # Add protected areas layer
  map <- map |>
    addRasterImage(
      PA,
      colors = "#005a32",
      opacity = 0.8,
      group = ELSA_text |> filter(var == "protected_areas") |> pull(language),
      options = tileOptions(zIndex = 800)
    ) |>
    addLegend(
      "topright",
      colors = "#005a32",
      labels = ELSA_text |> filter(var == "protected_areas") |> pull(language),
      group = ELSA_text |> filter(var == "protected_areas") |> pull(language)
    )
  
  # Add restoration areas layer if applicable
  if (restorelock) {
    map <- map |>
      addRasterImage(
        Rest,
        colors = "#fcae16",
        opacity = 0.8,
        group = ELSA_text |> filter(var == "restore_areas") |> pull(language),
        options = tileOptions(zIndex = 800)
      ) |>
      addLegend(
        "topright",
        colors = "#fcae16",
        labels = ELSA_text |> filter(var == "restore_areas") |> pull(language),
        group = ELSA_text |> filter(var == "restore_areas") |> pull(language)
      )
  }
  
  # Add layers control
  overlay_groups <- c(
    "ELSA HM",
    glue("{theme_tbl$theme} HM"),
    ELSA_text |> filter(var == "protected_areas") |> pull(language)
  )
  if (restorelock) {
    overlay_groups <- c(overlay_groups,
                        ELSA_text |> filter(var == "restore_areas") |> pull(language))
  }
  
  map <- map |>
    addLayersControl(
      overlayGroups = overlay_groups,
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) |>
    hideGroup(c(
      glue("{theme_tbl$theme} HM"),
      ELSA_text |> filter(var == "protected_areas") |> pull(language)
    ))
  
  if (restorelock) {
    map <- map |>
      hideGroup(ELSA_text |> filter(var == "restore_areas") |> pull(language))
  }
  
  return(map)
}
