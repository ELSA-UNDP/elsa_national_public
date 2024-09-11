fun_tmap_elsa_multi <- function(feat_temp = NULL,
                                heatm = NULL,
                                rast = NULL,
                                rast.lst = NULL,
                                theme_tbl = NULL) {
  opts <- tmap::tmap_options(show.warnings = FALSE,
                             show.messages = FALSE)
  
  outl <- tmap_mode(mode = "view")
  
  for (ii in 1:nrow(theme_tbl)) {
    if (length(pal.elsa) == 4) {
      tb <-
        get_palette(
          palette = pal.elsa,
          in_rast = terra::ifel(rast.lst[[ii]] %in% 1:4, rast.lst[[ii]], NA)
        )
    } else {
      tb <-
        get_palette(
          palette = pal.elsa,
          in_rast = terra::ifel(rast.lst[[ii]] %in% 1:3, rast.lst[[ii]], NA)
        )
    }
    
    if (global(rast, max, na.rm = TRUE)$max == 4) {
      outl <- outl +
        tm_shape(
          terra::ifel(rast.lst[[ii]] %in% 1:4, rast.lst[[ii]], NA),
          name = glue(
            "{theme_tbl$theme[ii]} {ELSA_text %>%
                    dplyr::filter(var == 'action') %>%
                    dplyr::pull(language)}"
          ),
          raster.downsample = FALSE,
          legend.size.is.portrait = TRUE
        ) +
        tm_raster(
          alpha = 0.8,
          style = "cat",
          palette = tb %>% pull(colour),
          title = glue::glue(
            "{theme_tbl$theme[ii]} {ELSA_text %>%
                           dplyr::filter(var == 'action') %>%
                           dplyr::pull(language)}"
          ),
          labels = tb %>% dplyr::pull(label),
          breaks = tb %>% dplyr::pull(breaks),
          drop.levels = TRUE
        )
    } else {
      outl <- outl +
        tm_shape(
          terra::ifel(rast.lst[[ii]] %in% 1:3, rast.lst[[ii]], NA),
          name = glue(
            "{theme_tbl$theme[ii]} {ELSA_text %>%
                    dplyr::filter(var == 'action') %>%
                    dplyr::pull(language)}"
          ),
          raster.downsample = FALSE,
          legend.size.is.portrait = TRUE
        ) +
        tm_raster(
          alpha = 0.8,
          style = "cat",
          palette = tb %>% pull(colour),
          title = glue::glue(
            "{theme_tbl$theme[ii]} {ELSA_text %>%
                           dplyr::filter(var == 'action') %>%
                           dplyr::pull(language)}"
          ),
          labels = tb %>% dplyr::pull(label),
          breaks = tb %>% dplyr::pull(breaks),
          drop.levels = TRUE
        )
      
    }
  }
  
  if (global(rast, max, na.rm = TRUE)$max == 4) {
    outl <- outl +
      tm_shape(
        terra::ifel(rast %in% 1:4, rast, NA),
        name = glue::glue(
          "ELSA {ELSA_text %>%
                        dplyr::filter(var == 'action') %>%
                        dplyr::pull(language)}"
        ),
        raster.downsample = FALSE,
        legend.size.is.portrait = TRUE
      ) +
      tm_raster(
        alpha = 0.8,
        style = "cat",
        palette = tb %>% dplyr::pull(colour),
        title = glue::glue(
          "ELSA {ELSA_text %>%
                         dplyr::filter(var == 'action') %>%
                         dplyr::pull(language)}"
        ),
        labels = tb %>% dplyr::pull(label),
        breaks = tb %>% dplyr::pull(breaks),
        drop.levels = TRUE
      )
  } else {
    outl <- outl +
      tm_shape(
        terra::ifel(rast %in% 1:3, rast, NA),
        name = glue::glue(
          "ELSA {ELSA_text %>%
                        dplyr::filter(var == 'action') %>%
                        dplyr::pull(language)}"
        ),
        raster.downsample = FALSE,
        legend.size.is.portrait = TRUE
      ) +
      tm_raster(
        alpha = 0.8,
        style = "cat",
        palette = tb %>% dplyr::pull(colour),
        title = glue::glue(
          "ELSA {ELSA_text %>%
                         dplyr::filter(var == 'action') %>%
                         dplyr::pull(language)}"
        ),
        labels = tb %>% dplyr::pull(label),
        breaks = tb %>% dplyr::pull(breaks),
        drop.levels = TRUE
      )
  }
  
  # Heat maps ####
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
    name <- glue::glue("{theme_tbl$theme[ii]} HM")
    
    if (minmax(heatm[[ii]])[2] == 0) {
      # Allow for possible 0 values if user sets all weights to 0 or a theme's weights are all 0
      
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
          title = name
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
          glue::glue(
            "ELSA {ELSA_text %>%
                   dplyr::filter(var == 'action') %>%
                   dplyr::pull(language)}"
          ),
          glue::glue(
            "{theme_tbl$theme} {ELSA_text %>%
                   dplyr::filter(var == 'action') %>%
                   dplyr::pull(language)}"
          ),
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
          glue::glue(
            "{theme_tbl$theme} {ELSA_text %>%
                 dplyr::filter(var == 'action') %>%
                 dplyr::pull(language)}"
          ),
          "ELSA HM",
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
          glue::glue(
            "ELSA {ELSA_text %>%
                   dplyr::filter(var == 'action') %>%
                   dplyr::pull(language)}"
          ),
          glue::glue(
            "{theme_tbl$theme} {ELSA_text %>%
                   dplyr::filter(var == 'action') %>%
                   dplyr::pull(language)}"
          ),
          "ELSA HM",
          glue::glue("{theme_tbl$theme} HM"),
          ELSA_text %>%
            dplyr::filter(var == "protected_areas") %>%
            dplyr::pull(language)
        ),
        baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
        position = "topleft"
      ) %>%
      leaflet::hideGroup(
        c(
          glue::glue(
            "{theme_tbl$theme} {ELSA_text %>%
                 dplyr::filter(var == 'action') %>%
                 dplyr::pull(language)}"
          ),
          "ELSA HM",
          glue::glue("{theme_tbl$theme} HM"),
          ELSA_text %>%
            dplyr::filter(var == "protected_areas") %>%
            dplyr::pull(language)
        )
      )
    
  }
  
  outl
}
