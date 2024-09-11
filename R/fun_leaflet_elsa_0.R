library(leaflet)
library(dplyr)
library(glue)

fun_leaflet_elsa_0 <- function(feat_temp = NULL,
                               heatm = NULL,
                               theme_tbl = NULL) {
  # Create an empty leaflet map
  map <- leaflet() |>
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    addTiles(
      "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl07p7r84000b15mw1ctxlqwo/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2xvZmJ5eHNkMDlqNjJxdWhjYjVlcG5sMSJ9.ATqw6HfibevC5ov5y6VTOQ",
      group = "UNBL",
      attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Geodata'
    ) |>
    addTiles(
      "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl5vpldzt000614qc8qnjutdy/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2xvZmJ5eHNkMDlqNjJxdWhjYjVlcG5sMSJ9.ATqw6HfibevC5ov5y6VTOQ",
      group = "UNBL Dark",
      attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Geodata'
    )
  
  # Add the main feature layer
  if (terra::minmax(feat_temp)[2] == 0) {
    map <- map |>
      addRasterImage(feat_temp,
                     colors = pal.hm[1],
                     opacity = 0.8,
                     group = "ELSA HM") |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
        values = terra::values(feat_temp),
        title = "ELSA HM"
      )
  } else {
    map <- map |>
      addRasterImage(feat_temp,
                     colors = pal.hm,
                     opacity = 0.8,
                     group = "ELSA HM") |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
        values = terra::values(feat_temp),
        title = "ELSA HM"
      )
  }
  
  # Add theme layers
  for (ii in 1:nrow(theme_tbl)) {
    name <- glue("{theme_tbl$theme[ii]} HM")
    if (terra::minmax(heatm[[ii]])[2] == 0) {
      map <- map |>
        addRasterImage(heatm[[ii]],
                       colors = pal.hm[1],
                       opacity = 0.8,
                       group = name) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
          values = terra::values(heatm[[ii]]),
          title = name,
          group = name
        )
    } else {
      map <- map |>
        addRasterImage(heatm[[ii]],
                       colors = pal.hm,
                       opacity = 0.8,
                       group = name) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
          values = terra::values(heatm[[ii]]),
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
      group = ELSA_text |> filter(var == "protected_areas") |> pull(language)
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
        group = ELSA_text |> filter(var == "restore_areas") |> pull(language)
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
