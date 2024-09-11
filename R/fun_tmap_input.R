fun_tmap_input <- function(layers = NULL,
                           layer_names = NULL) {
  opts <- tmap::tmap_options(show.warnings = FALSE,
                             show.messages = FALSE)
  
  outl <- tmap_mode(mode = "view")
  
  for (ii in 1:terra::nlyr(layers)) {
    name <- layer_names[ii]
    outl <- outl +
      tm_shape(layers[[ii]],
               name = name,
               raster.downsample = TRUE) +
      tm_raster(
        style = "cont",
        alpha = 0.8,
        palette = pal.in,
        title = name
      )
  }
  
  outl <- outl +
    tm_shape(
      pu1[[1]],
      name = ELSA_text %>%
        dplyr::filter(var == "protect_zone") %>%
        dplyr::pull(language),
      raster.downsample = FALSE
    ) +
    tm_raster(
      alpha = 0.8,
      style = "cat",
      n = 1,
      breaks = c(1),
      palette = c("#6afdfa"),
      title = ELSA_text %>%
        dplyr::filter(var == "protect_zone") %>%
        dplyr::pull(language),
      drop.levels = TRUE,
      labels = ""
    )
  
  outl <- outl +
    tm_shape(
      pu1[[2]],
      name = ELSA_text %>%
        dplyr::filter(var == "restore_zone") %>%
        dplyr::pull(language),
      raster.downsample = FALSE
    ) +
    tm_raster(
      alpha = 0.8,
      style = "cat",
      n = 1,
      breaks = c(1),
      palette = c("#6afdfa"),
      title = ELSA_text %>%
        dplyr::filter(var == "restore_zone") %>%
        dplyr::pull(language),
      drop.levels = TRUE,
      labels = ""
    )
  
  outl <- outl +
    tm_shape(
      pu1[[3]],
      name = ELSA_text %>%
        dplyr::filter(var == "manage_zone") %>%
        dplyr::pull(language),
      raster.downsample = FALSE
    ) +
    tm_raster(
      alpha = 0.8,
      style = "cat",
      n = 1,
      breaks = c(1),
      palette = c("#6afdfa"),
      title = ELSA_text %>%
        dplyr::filter(var == "manage_zone") %>%
        dplyr::pull(language),
      drop.levels = TRUE,
      labels = ""
    )
  
  if (urb_green) {
    outl <- outl +
      tm_shape(
        pu1[[4]],
        name = ELSA_text %>%
          dplyr::filter(var == "green_zone") %>%
          dplyr::pull(language),
        raster.downsample = FALSE
      ) +
      tm_raster(
        alpha = 0.8,
        style = "cat",
        n = 1,
        breaks = c(1),
        palette = c("#6afdfa"),
        title = ELSA_text %>%
          dplyr::filter(var == "green_zone") %>%
          dplyr::pull(language),
        drop.levels = TRUE,
        labels = ""
      )
  }
  
  outl <- tmap_leaflet(outl,
                       in.shiny = TRUE) %>%
    addTiles(
      "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl07p7r84000b15mw1ctxlqwo/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2xvZmJ5eHNkMDlqNjJxdWhjYjVlcG5sMSJ9.ATqw6HfibevC5ov5y6VTOQ",
      group = "UNBL",
      attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Geodata'
    ) %>%
    addTiles(
      "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl5vpldzt000614qc8qnjutdy/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2xvZmJ5eHNkMDlqNjJxdWhjYjVlcG5sMSJ9.ATqw6HfibevC5ov5y6VTOQ",
      group = "UNBL Dark",
      attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Geodata'
    ) %>%
    addProviderTiles("Esri.WorldImagery",
                     group = "Satellite") %>%
    addLayersControl(
      overlayGroups = c(
        layer_names,
        ELSA_text %>%
          dplyr::filter(var == "protect_zone") %>%
          dplyr::pull(language),
        ELSA_text %>%
          dplyr::filter(var == "restore_zone") %>%
          dplyr::pull(language),
        ELSA_text %>%
          dplyr::filter(var == "manage_zone") %>%
          dplyr::pull(language),
        if (urb_green) {
          ELSA_text %>%
            dplyr::filter(var == "green_zone") %>%
            dplyr::pull(language)
        }
      ),
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      position = "topleft"
    ) %>%
    leaflet::hideGroup(
      c(
        layer_names[-1],
        ELSA_text %>%
          dplyr::filter(var == "protect_zone") %>%
          dplyr::pull(language),
        ELSA_text %>%
          dplyr::filter(var == "restore_zone") %>%
          dplyr::pull(language),
        ELSA_text %>%
          dplyr::filter(var == "manage_zone") %>%
          dplyr::pull(language),
        if (urb_green) {
          ELSA_text %>%
            dplyr::filter(var == "green_zone") %>%
            dplyr::pull(language)
        }
      )
    )
  
  outl
}
