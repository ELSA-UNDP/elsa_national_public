library(leaflet)
library(dplyr)

fun_leaflet_input <- function(layers, labels) {
  # Create an empty leaflet map
  map <- leaflet()  |>
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
  
  # Add input layers with color palettes
  for (ii in 1:length(labels)) {
    name <- labels[ii]
    map <- map  |>
      addRasterImage(layers[[ii]],
                     colors = pal.in,
                     group = name,
                     opacity = 0.8) |>
      addLegend(
        position = "topright",
        pal = colorNumeric(pal.in, na.color = NA, domain = NULL),
        values = values(layers[[ii]]),
        title = name,
        group = name,
        opacity = 0.8
      )
  }
  
  # Add base layers with default color
  map <- map  |>
    addRasterImage(
      pu1[[1]],
      group = ELSA_text  |> filter(var == "protect_zone")  |> pull(language),
      opacity = 0.8,
      colors = pal.zone
    ) |>
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[1]]),
      labels = ELSA_text  |>  filter(var == "protect_zone") |>  pull(language),
      group = ELSA_text |>  filter(var == "protect_zone") |>  pull(language),
      opacity = 0.8
    ) |>
    addRasterImage(
      pu1[[2]],
      group = ELSA_text  |>  filter(var == "restore_zone")  |>  pull(language),
      opacity = 0.8,
      colors = pal.zone
    ) |>
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[2]]),
      labels = ELSA_text |>  filter(var == "restore_zone") |>  pull(language),
      group = ELSA_text |>  filter(var == "restore_zone") |>  pull(language),
      opacity = 0.8
    ) |>
    addRasterImage(
      pu1[[3]],
      group = ELSA_text |>  filter(var == "manage_zone")  |>  pull(language),
      opacity = 0.8,
      colors = pal.zone
    ) |>
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[3]]),
      labels = ELSA_text  |>  filter(var == "manage_zone")  |>  pull(language),
      group = ELSA_text |>  filter(var == "manage_zone") |>  pull(language),
      opacity = 0.8
    )
  
  if (urb_green) {
    map <- map |>
      addRasterImage(
        pu1[[4]],
        group = ELSA_text  |> filter(var == "green_zone")  |> pull(language),
        opacity = 0.8,
        colors = pal.zone
      )  |>
      addLegend(
        position = "topright",
        colors = pal.zone,
        values = values(pu1[[4]]),
        labels = ELSA_text  |> filter(var == "green_zone")  |> pull(language),
        group = ELSA_text  |> filter(var == "green_zone")  |> pull(language),
        opacity = 0.8
      )
  }
  
  # Add layers control
  overlay_groups <- c(
    labels,
    ELSA_text  |> filter(var == "protect_zone")  |> pull(language),
    ELSA_text  |> filter(var == "restore_zone")  |> pull(language),
    ELSA_text  |> filter(var == "manage_zone")  |> pull(language)
  )
  
  if (urb_green) {
    overlay_groups <- c(overlay_groups,
                        ELSA_text  |> filter(var == "green_zone")  |> pull(language))
  }
  
  map <- map  |>
    addLayersControl(
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    )  |>
    hideGroup(labels[-1])  |>
    hideGroup(ELSA_text  |> filter(var == "protect_zone")  |> pull(language))  |>
    hideGroup(ELSA_text  |> filter(var == "restore_zone")  |> pull(language))  |>
    hideGroup(ELSA_text  |> filter(var == "manage_zone")  |> pull(language))
  
  if (urb_green) {
    map <- map  |>
      hideGroup(ELSA_text  |> filter(var == "green_zone")  |> pull(language))
  }
  
  return(map)
}
