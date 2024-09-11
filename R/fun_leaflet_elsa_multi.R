library(leaflet)
library(dplyr)
library(glue)
library(terra)

fun_leaflet_elsa_multi <- function(feat_temp = NULL,
                                   heatm = NULL,
                                   rast = NULL,
                                   rast_lst = NULL,
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
  
  # Add each theme's raster layer
  for (ii in 1:nrow(theme_tbl)) {
    if (length(pal.elsa) == 4) {
      tb <- get_palette(
        palette = pal.elsa,
        in_rast = terra::ifel(rast_lst[[ii]] %in% 1:4, rast_lst[[ii]], NA)
      )
    } else {
      tb <- get_palette(
        palette = pal.elsa,
        in_rast = terra::ifel(rast_lst[[ii]] %in% 1:3, rast_lst[[ii]], NA)
      )
    }
    
    theme_group <- glue(
      "{theme_tbl$theme[ii]} {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
    )
    
    map <- map |>
      addRasterImage(
        terra::ifel(rast_lst[[ii]] %in% 1:4, rast_lst[[ii]], NA),
        colors = tb |> dplyr::pull(colour),
        opacity = 0.8,
        group = theme_group
      ) |>
      addLegend(
        "topright",
        colors = tb |> pull(colour),
        labels = tb |> pull(label),
        title = theme_group,
        group = theme_group,
        values = tb |> pull(breaks)
      )
  }
  
  # Add the main feature layer based on the max value of rast
  if (terra::global(rast, max, na.rm = TRUE)$max == 4) {
    tb <- get_palette(palette = pal.elsa,
                      in_rast = terra::ifel(rast %in% 1:4, rast, NA))
  } else {
    tb <- get_palette(palette = pal.elsa,
                      in_rast = terra::ifel(rast %in% 1:3, rast, NA))
  }
  
  action_group <- glue("ELSA {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}")
  
  map <- map |>
    addRasterImage(
      terra::ifel(rast %in% 1:4, rast, NA),
      colors = tb |> pull(colour),
      opacity = 0.8,
      group = action_group
    ) |>
    addLegend(
      "topright",
      colors = tb |> pull(colour),
      labels = tb |> pull(label),
      title = action_group,
      group = action_group,
      values = tb |> pull(breaks)
    )
  
  # Heat maps
  heatmap_group <- "ELSA HM"
  map <- map |>
    addRasterImage(feat_temp,
                   colors = pal.hm,
                   opacity = 0.8,
                   group = heatmap_group) |>
    addLegend(
      "topright",
      pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
      values = terra::values(feat_temp),
      title = heatmap_group,
      group = heatmap_group
    )
  
  # Add theme heat map layers
  for (ii in 1:nrow(theme_tbl)) {
    name <- glue("{theme_tbl$theme[ii]} HM")
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
        group = name
      )
  }
  
  # Add protected areas layer
  protected_group <- ELSA_text |> filter(var == "protected_areas") |> pull(language)
  map <- map |>
    addRasterImage(PA,
                   colors = "#005a32",
                   opacity = 0.8,
                   group = protected_group) |>
    addLegend(
      "topright",
      colors = "#005a32",
      labels = protected_group,
      group = protected_group
    )
  
  # Add restoration areas layer if applicable
  if (restorelock) {
    restore_group <- ELSA_text |> filter(var == "restore_areas") |> pull(language)
    map <- map |>
      addRasterImage(
        Rest,
        colors = "#fcae16",
        opacity = 0.8,
        group = restore_group
      ) |>
      addLegend(
        "topright",
        colors = "#fcae16",
        labels = restore_group,
        group = restore_group
      )
  }
  
  # Add layers control
  overlay_groups <- c(
    action_group,
    glue(
      "{theme_tbl$theme} {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
    ),
    heatmap_group,
    glue("{theme_tbl$theme} HM"),
    protected_group
  )
  if (restorelock) {
    overlay_groups <- c(overlay_groups, restore_group)
  }
  
  map <- map |>
    addLayersControl(
      overlayGroups = overlay_groups,
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) |>
    hideGroup(c(
      glue(
        "{theme_tbl$theme} {ELSA_text |> dplyr::filter(var == 'action') |> dplyr::pull(language)}"
      ),
      heatmap_group,
      glue("{theme_tbl$theme} HM"),
      protected_group
    ))
  if (restorelock) {
    map <- map |>
      hideGroup(restore_group)
  }
  
  return(map)
}
