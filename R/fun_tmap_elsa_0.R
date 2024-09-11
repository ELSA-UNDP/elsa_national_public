fun_tmap_elsa_0 <- function(feat_temp = NULL,
                            heatm = NULL,
                            theme_tbl = NULL) {
  opts <- tmap::tmap_options(show.warnings = FALSE,
                             show.messages = FALSE)
  
  outl <- tmap_mode(mode = "view")
  
  
  if (minmax(feat_temp)[2] == 0) {
    # Allow for possible 0 values if user sets all weights to 0 or a theme's weights are all 0
    
    outl <- outl +
      tm_shape(feat_temp,
               name = "ELSA HM",
               raster.downsample = TRUE) +
      tm_raster(
        alpha = 0.8,
        style = "cont",
        palette = pal.hm[1],
        title = "ELSA HM"
      )
  } else {
    outl <- outl +
      tm_shape(feat_temp,
               name = "ELSA HM",
               raster.downsample = TRUE) +
      tm_raster(
        alpha = 0.8,
        style = "cont",
        palette = pal.hm,
        title = "ELSA HM"
      )
  }
  
  for (ii in 1:nrow(theme_tbl)) {
    # Allow for possible 0 values if user sets all weights to 0 or a theme's weights are all 0
    
    name <- glue("{theme_tbl$theme[ii]} HM")
    
    if (minmax(heatm[[ii]])[2] == 0) {
      outl <- outl +
        tm_shape(heatm[[ii]],
                 name = name,
                 raster.downsample = TRUE) +
        tm_raster(
          alpha = 0.8,
          style = "cont",
          palette = pal.hm[1],
          title = name
        )
    } else {
      outl <- outl +
        tm_shape(heatm[[ii]],
                 name = name,
                 raster.downsample = TRUE) +
        tm_raster(
          alpha = 0.8,
          style = "cont",
          palette = pal.hm,
          title = name,
          breaks = c(0, 0.25, 0.5, 0.75, 1)
        )
    }
  }
  
  outl <- outl +
    tm_shape(
      PA,
      name = ELSA_text %>%
        dplyr::filter(var == "protected_areas") %>%
        dplyr::pull(language),
      raster.downsample = FALSE
    ) +
    tm_raster(
      alpha = 0.8,
      style = "cat",
      n = 1,
      breaks = c(1),
      palette = c("#005a32"),
      title = ELSA_text %>%
        dplyr::filter(var == "protected_areas") %>%
        dplyr::pull(language),
      drop.levels = TRUE,
      labels = ""
    )
  
  if (restorelock) {
    outl <- outl +
      tm_shape(
        Rest,
        name = ELSA_text %>%
          dplyr::filter(var == "restore_areas") %>%
          dplyr::pull(language),
        raster.downsample = FALSE
      ) +
      tm_raster(
        alpha = 0.8,
        style = "cat",
        n = 1,
        breaks = c(1),
        palette = c("#fcae16"),
        title = ELSA_text %>%
          dplyr::filter(var == "restore_areas") %>%
          dplyr::pull(language),
        drop.levels = TRUE,
        labels = ""
      )
  }
  
  if (restorelock) {
    outl <- tmap_leaflet(outl,
                         in.shiny = TRUE) %>%
      addTiles(
        "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl07p7r84000b15mw1ctxlqwo/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2tjeWY0cTU2MDk4aTMwbW9ndGhoaXQ1ZyJ9.R7w4SMGYo_tW4yPDWq0I7w",
        group = "UNBL",
        attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Cartographic'
      ) %>%
      addTiles(
        "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl5vpldzt000614qc8qnjutdy/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2tjeWY0cTU2MDk4aTMwbW9ndGhoaXQ1ZyJ9.R7w4SMGYo_tW4yPDWq0I7w",
        group = "UNBL Dark",
        attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Cartographic'
      ) %>%
      addProviderTiles("Esri.WorldImagery",
                       group = "Satellite") %>%
      addLayersControl(
        overlayGroups = c(
          "ELSA HM",
          glue::glue("{theme_tbl$theme} HM"),
          ELSA_text %>%
            dplyr::filter(var == "protected_areas") %>%
            dplyr::pull(language),
          ELSA_text %>%
            dplyr::filter(var == "restore_areas") %>%
            dplyr::pull(language)
        ),
        baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
        position = "topleft"
      ) %>%
      leaflet::hideGroup(
        c(
          glue::glue("{theme_tbl$theme} HM"),
          ELSA_text %>%
            dplyr::filter(var == "protected_areas") %>%
            dplyr::pull(language),
          ELSA_text %>%
            dplyr::filter(var == "restore_areas") %>%
            dplyr::pull(language)
        )
      )
  } else {
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
          "ELSA HM",
          glue::glue("{theme_tbl$theme} HM"),
          ELSA_text %>%
            dplyr::filter(var == "protected_areas") %>%
            dplyr::pull(language)
        ),
        baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
        position = "topleft"
      ) %>%
      leaflet::hideGroup(c(
        glue::glue("{theme_tbl$theme} HM"),
        ELSA_text %>%
          dplyr::filter(var == "protected_areas") %>%
          dplyr::pull(language)
      ))
  }
  
  outl
}
