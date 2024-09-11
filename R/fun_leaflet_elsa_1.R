library(leaflet)
library(dplyr)
library(glue)
library(terra)

fun_leaflet_elsa_1 <- function(feat_temp = NULL,
                               heatm = NULL,
                               rast = NULL,
                               theme_tbl = NULL) {
  # Determine the palette to use based on the max value of rast
  if (terra::global(rast, max, na.rm = TRUE)$max == 4) {
    tb <- get_palette(palette = pal.elsa,
                      in_rast = terra::ifel(rast %in% 1:4, rast, NA))
  } else {
    tb <- get_palette(palette = pal.elsa,
                      in_rast = terra::ifel(rast %in% 1:3, rast, NA))
  }
  
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
  
  # Add the main feature layer based on the max value of rast
  if (terra::global(rast, max, na.rm = TRUE)$max == 4) {
    map <- map |>
      addRasterImage(
        terra::ifel(rast %in% 1:4, rast, NA),
        colors = tb |> dplyr::pull(colour),
        opacity = 0.8,
        group = glue(
          "ELSA {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
        )
      ) |>
      addLegend(
        "topright",
        colors = tb |> dplyr::pull(colour),
        values = tb |> dplyr::pull(breaks),
        title = glue(
          "ELSA {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
        ),
        labels = tb |> dplyr::pull(label)
      )
  } else {
    map <- map |>
      addRasterImage(
        terra::ifel(rast %in% 1:3, rast, NA),
        colors = tb |> dplyr::pull(colour),
        opacity = 0.8,
        group = glue(
          "ELSA {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
        )
      ) |>
      addLegend(
        "topright",
        colors = tb |> dplyr::pull(colour),
        values = tb |> dplyr::pull(breaks),
        title = glue(
          "ELSA {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
        ),
        labels = tb |> dplyr::pull(label)
      )
  }
  
  # Heat maps
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
        title = "ELSA HM",
        group = "ELSA HM"
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
        title = "ELSA HM",
        group = "ELSA HM"
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
          pal = colorNumeric(pal.hm[1], domain = NULL),
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
          pal = colorNumeric(pal.hm, domain = NULL),
          values = terra::values(heatm[[ii]]),
          title = name,
          group = name
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
    glue(
      "ELSA {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
    ),
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
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      ELSA_text |> filter(var == "protected_areas") |> pull(language)
    ))
  
  if (restorelock) {
    map <- map |>
      hideGroup(ELSA_text |> filter(var == "restore_areas") |> pull(language))
  }
  
  return(map)
}
