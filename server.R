#' ELSA Shiny App Server
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session) {
  # # Authentication module ####
  # auth <- callModule(
  #   module = auth_server,
  #   id = "auth",
  #   check_credentials = check_credentials(credentials)
  # )

  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })

  session$onSessionEnded(function() {
    stopApp()
  })

  values <- reactiveValues(hot_wgt = wgts, hot_imp = impacts)

  calc <- reactive({
    # Load initial values ####
    df1 <- values[["hot_wgt"]]
    df3 <- values[["hot_imp"]]

    list(wgts = df1, impacts = df3)
  })

  ## Edit Weights ####
  output$hot_wgt <- renderRHandsontable({
    if (!is.null(input$hot_wgt)) {
      DF <- hot_to_r(input$hot_wgt)
      values[["hot_wgt"]] <- DF
    } else if (!is.null(values[["hot_wgt"]])) {
      DF <- values[["hot_wgt"]]
    }

    # Prevent rhandson from adding rows when user drags values
    if (nrow(DF) > nrow(wgts)) {
      DF <- DF[1:nrow(wgts), ]
      values[["hot_wgt"]] <- DF
    }

    # setHot(DF)
    rhandsontable(
      DF[, c("name", "theme", "weight", "policy", "feature")],
      readOnly = TRUE,
      colHeaders = c(
        ELSA_text %>%
          dplyr::filter(var %in% c("data", "theme", "weight", "policy")) %>%
          dplyr::slice(1, 3, 4, 2) %>%
          dplyr::pull(language),
        "feature"
      )
    ) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(
        ELSA_text %>%
          filter(var %in% c("weight")) %>%
          pull(language),
        readOnly = FALSE
      ) %>%
      hot_col(col = "feature", colWidths = 0.1) %>% # Small width hides column
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  # Gurobi reactive ####
  my.data <- shiny::reactive({
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.
    if (input$mrun == 0) {
      return(NULL)
    }

    return(isolate({
      weights.temp <- calc()$wgts
      # Adjust UI weights with pre-calibrated weights
      weights.temp$weight <- weights.temp$weight * wgta

      impacts.temp <- impacts

      progress <- Progress$new(session)

      progress$set(
        message = ELSA_text %>%
          dplyr::filter(var == "setup") %>%
          dplyr::pull(language),
        detail = ELSA_text %>%
          dplyr::filter(var == "be_patient") %>%
          dplyr::pull(language),
        value = 0.01
      )

      pu_temp <- pu_all[["area"]][[input$protected]]

      prob.ta <-
        prioritizr::problem(pu_temp, zns, run_checks = FALSE) %>%
        prioritizr::add_gurobi_solver(gap = 0.05, threads = 8)


      if (input$protected == "avail") {
        target <- get_min_lockin_target(c(PA0, PA0, PA0), input, pu)

        prob.ta <- prob.ta %>%
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          ))
      }

      if (input$protected == "locked") {
        target <- get_min_lockin_target(c(PA, PA0, PA0), input, pu)

        prob.ta <- prob.ta %>%
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          )) %>%
          prioritizr::add_locked_in_constraints(c(PA, PA0, PA0))
      }

      if (input$protected == "restore") {
        target <- get_min_lockin_target(c(PA0, Rest, PA0), input, pu)

        prob.ta <- prob.ta %>%
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          )) %>%
          prioritizr::add_locked_in_constraints(c(PA0, Rest, PA0))
      }

      if (input$protected == "pa_restore") {
        target <- get_min_lockin_target(c(PA, Rest, PA0), input, pu)

        prob.ta <- prob.ta %>%
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          )) %>%
          prioritizr::add_locked_in_constraints(c(PA, Rest, PA0))
      }

      #### Boundary Penalty Factor ####
      if (input$blm > 0) {
        prob.ta <- prob.ta %>%
          prioritizr::add_boundary_penalties(penalty = input$blm / 10000)
      }

      if (input$multipri == TRUE) {
        progress$set(
          message = ELSA_text %>% dplyr::filter(var == "calc") %>% dplyr::pull(language),
          detail = paste(
            ELSA_text %>% dplyr::filter(var == "calc") %>% dplyr::pull(language),
            sprintf("1/%s", (1 + nrow(theme_tbl)))
          ),
          value = round(1 / (2 + nrow(theme_tbl)), 1)
        )
      } else {
        progress$set(
          message = ELSA_text %>% dplyr::filter(var == "calc") %>% dplyr::pull(language),
          detail = ELSA_text %>% dplyr::filter(var == "run") %>% dplyr::pull(language),
          value = 0.5
        )
      }

      # All
      # Add weights to conservation problem ####
      prob.all <- prob.ta %>%
        prioritizr::add_feature_weights(as.matrix(matrix(
          rep(weights.temp$weight, 3),
          ncol = 3,
          nrow = terra::nlyr(feat_stack)
        )))

      # Solve conservation problem ####
      elsa_result <- solve(prob.all, force = TRUE)
      #browser()
      
      # get feature representation
      # feat_rep <-
      #   prioritizr::eval_feature_representation_summary(prob.all, elsa_result) %>%
      #   dplyr::filter(summary != "overall") %>%
      #   dplyr::rename(zone = summary)
      # 
      # tmp <-
      #   impacts.temp[, c("feature", "Protect", "Restore", "Manage")] %>%
      #   tidyr::pivot_longer(-feature, names_to = "zone", values_to = "impact")
      # 
      # feat_rep <-
      #   dplyr::left_join(feat_rep, tmp, by = c("feature" = "feature", "zone" = "zone"))
      # 
      # feat_rep$relative_held <-
      #   feat_rep$relative_held * feat_rep$impact
      
      # First get overall info from prioritizr
      overall_rep <- prioritizr::eval_feature_representation_summary(prob.all, elsa_result) %>%
        dplyr::rename(
          zone = summary,
          relative_held_overall = relative_held,
          total_amount_overall = total_amount
        ) %>%
        filter(zone == "overall") %>%
        dplyr::select(-"zone", -"absolute_held")
      
      # Calculate relative values for each zone and pivot them into new columns
      relative_rep <- prioritizr::eval_feature_representation_summary(prob.all, elsa_result) %>%
        dplyr::rename(zone = summary) %>%
        dplyr::filter(zone != "overall") %>%
        dplyr::select(zone, feature, absolute_held) %>%
        dplyr::left_join(overall_rep, by = "feature") %>%
        dplyr::mutate(relative_held_zone = absolute_held / total_amount_overall) %>%
        dplyr::select("zone", "feature", "relative_held_zone") %>%
        tidyr::pivot_wider(names_from = zone, values_from = relative_held_zone)
      
      # Combine the overall totals with the relative values
      feat_rep <- overall_rep %>%
        dplyr::left_join(relative_rep, by = "feature")

      elsa_result_multi <- feat_rep.lst <- list()

      if (input$multipri == TRUE) {
        for (ii in 1:nrow(theme_tbl)) {
          progress$set(
            message = ELSA_text %>% dplyr::filter(var == "calc") %>% dplyr::pull(language),
            detail = paste(
              ELSA_text %>% dplyr::filter(var == "calc") %>% dplyr::pull(language),
              sprintf("%s/%s", 1 + ii, (1 + nrow(
                theme_tbl
              )))
            ),
            value = round((1 + ii) / (2 + nrow(
              theme_tbl
            )), 1)
          )

          wgt.tmp <- weights.temp

          wgt.tmp$weight[names(feat_stack) %nin% theme_tbl$names[[ii]]] <-
            0

          prob.tmp <- prob.ta %>%
            prioritizr::add_feature_weights(as.matrix(matrix(
              rep(wgt.tmp$weight, 3),
              ncol = 3,
              nrow = terra::nlyr(feat_stack)
            )))

          elsa_result_multi[[ii]] <- solve(prob.tmp, force = TRUE)

          # feat_rep.lst[[ii]] <-
          #   prioritizr::eval_feature_representation_summary(prob.tmp, elsa_result_multi[[ii]]) %>%
          #   dplyr::filter(summary != "overall") %>%
          #   dplyr::rename(zone = summary)
          # 
          # feat_rep.lst[[ii]]$relative_held <-
          #   feat_rep.lst[[ii]]$relative_held * feat_rep$impact
          
          overall_rep <- prioritizr::eval_feature_representation_summary(prob.tmp, elsa_result_multi[[ii]]) %>%
            dplyr::rename(
              zone = summary,
              relative_held_overall = relative_held,
              total_amount_overall = total_amount
            ) %>%
            filter(zone == "overall") %>%
            dplyr::select(-"zone", -"absolute_held")
          
          # Calculate relative values for each zone and pivot them into new columns
          relative_rep <- prioritizr::eval_feature_representation_summary(prob.tmp, elsa_result_multi[[ii]]) %>%
            dplyr::rename(zone = summary) %>%
            dplyr::filter(zone != "overall") %>%
            dplyr::select(zone, feature, absolute_held) %>%
            dplyr::left_join(overall_rep, by = "feature") %>%
            dplyr::mutate(relative_held_zone = absolute_held / total_amount_overall) %>%
            dplyr::select("zone", "feature", "relative_held_zone") %>%
            tidyr::pivot_wider(names_from = zone, values_from = relative_held_zone)
          
          # Combine the overall totals with the relative values
          feat_rep.lst[[ii]] <- overall_rep %>%
            dplyr::left_join(relative_rep, by = "feature") %>%
            dplyr::select(-"total_amount_overall")

          rm(wgt.tmp, prob.tmp)
        }
      }

      #### Calculate representation summaries ####
      progress$set(
        message = ELSA_text
        %>% dplyr::filter(var == "post") %>%
          dplyr::pull(language),
        detail = ELSA_text %>%
          dplyr::filter(var == "post_help") %>%
          dplyr::pull(language),
        value = 0.9
      )
      browser()

      rh_rep <- feat_rep %>%
        dplyr::group_by(feature) %>%
        dplyr::summarise(ELSA = round(sum(relative_held_overall, na.rm = T) * 100, 0)) # what is this meant to do?

      if (input$multipri == TRUE) {
        #rh.lst <- list()
        
        feature_rep_tabl_comb <- purrr::reduce(seq_along(feat_rep.lst), function(x, i) {
          # Subset first data frame (i == 1) to columns 1:3, the rest to columns 1 and 3
          df <- feat_rep.lst[[i]][, c(1, 2)]
          colnames(df)[2] <- themes[[i]]
          
          # Join the data frames
          dplyr::left_join(x, df, by = setNames(names(x)[1], names(df)[1]))
        }, .init = feat_rep.lst[[1]][, 1])  %>%
          tibble::add_column(
            Name = feat_df$label,
            Theme = feat_df$theme,
            .before = 1
          )  %>%
          dplyr::select(-c(feature))%>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 100, 1))) 
        

        # feat_rep_tabl <-
        #   feat_rep[feat_rep$zone == "Protect", c("feature")] %>%
        #   dplyr::left_join(rh_rep, by = "feature")

        # for (ii in 1:nrow(theme_tbl)) {
        #   rh.lst[[ii]] <- feat_rep.lst[[ii]] %>%
        #     dplyr::group_by(feature) %>%
        #     dplyr::summarise(
        #       !!stringr::str_glue(
        #         theme_tbl$theme[[ii]],
        #         " {ELSA_text %>% filter(var == 'action') %>% pull(language)}"
        #       ) := round(sum(relative_held, na.rm = T) * 100, 0)
        #     )
        #   feat_rep_tabl <- feat_rep_tabl %>%
        #     dplyr::left_join(rh.lst[[ii]], by = "feature")
        # }

        feat_rep_tabl <- feat_rep_tabl %>%
          dplyr::rowwise() %>%
          dplyr::mutate(elsa_tradeoff = round(ELSA / max(c_across(3:5)) * 100, 0)) %>% # ELSA Trade-off relative to other scenarios
          # tibble::add_column(
          #   Name = feat_df$label,
          #   Theme = feat_df$theme,
          #   .before = 1
          # ) %>%
          dplyr::rename(
            "{ELSA_text %>% filter(var == 'elsa_tradeoff') %>% pull(language)}" := elsa_tradeoff
          )
      } else { # don't we need this anyway if we want to show ELSA without multipri as well?
        feature_rep_tabl <- feature_rep %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 100, 1))) %>%
          #dplyr::rename_with(~ as_tibble(cats(elsa_raster)[[1]])$label, .cols = -c(1:3)) %>%
          dplyr::select(3:6) %>%
          tibble::add_column(
            Name = feat_df$label,
            Theme = feat_df$theme,
            .before = 1
          ) %>%
          dplyr::rename(
            "{ELSA_text %>% filter(var == 'data') %>% pull(language)}" := Name,
            "{ELSA_text %>% filter(var == 'theme') %>% pull(language)}" := Theme,
           "{ELSA_text %>% filter(var == 'overall') %>% pull(language)}" := relative_held_overall,
          )
        
        # feat_rep_tabl <-
        #   feat_rep[feat_rep$zone == "Protect", c("feature")] %>%
        #   dplyr::left_join(rh_rep, by = "feature") %>%
        #   tibble::add_column(
        #     Name = feat_df$label,
        #     Theme = feat_df$theme,
        #     .before = 1
        #   ) %>%
        #   dplyr::select(-c(feature))
      }

      # feat_rep_tabl <- feat_rep_tabl %>%
      #   dplyr::rename(
      #     "{ELSA_text %>% filter(var == 'data') %>% pull(language)}" := Name,
      #     "{ELSA_text %>% filter(var == 'theme') %>% pull(language)}" := Theme
      #   )

      rlist <- list(
        sel.fr = feat_rep,
        res.fr = feat_rep,
        elsa_result = elsa_result,
        elsa_result_multi = elsa_result_multi,
        feat_rep = feat_rep,
        feat_rep.lst = feat_rep.lst,
        feat_rep_tabl = feat_rep_tabl
      )

      progress$set(value = 1)

      progress$close()

      return(rlist)
    }))
  })

  observe({
    my.data()
  })

  output$InMap <- renderLeaflet({
    # Leaflet input datasets map ####
    progress <- Progress$new(session)

    progress$set(
      message = ELSA_text %>%
        filter(var == "gen_map") %>%
        pull(language),
      detail = ELSA_text %>%
        filter(var == "be_patient") %>%
        pull(language),
      value = 0.5
    )

    outl <- fun_leaflet_input(layers = feat_stack, labels = feat_df$label)

    progress$set(value = 1)

    progress$close()

    outl
  })

  output$cadMap <- renderLeaflet({
    if (input$mrun == 0) {
      progress <- Progress$new(session)

      progress$set(
        message = ELSA_text %>%
          filter(var == "gen_map") %>%
          pull(language),
        detail = ELSA_text %>%
          filter(var == "be_patient") %>%
          pull(language),
        value = 0.5
      )

      weights.temp <- calc()$wgts

      heatm <- list()

      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          heatm[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]],
              sum,
              na.rm = TRUE
            )
        } else {
          heatm[[ii]] <-
            theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }

        heatm[[ii]] <-
          heatm[[ii]] / terra::global(heatm[[ii]], max, na.rm = TRUE)$max * pu
      }

      feat_temp <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)

      feat_temp <-
        feat_temp / terra::global(feat_temp, max, na.rm = TRUE)$max * pu

      # No Run Maps ####
      outl <- fun_leaflet_elsa_0(
        feat_temp = feat_temp,
        heatm = heatm,
        theme_tbl = theme_tbl
      )

      progress$set(value = 1)
      progress$close()

      outl
    } else {
      progress <- Progress$new(session)
      progress$set(
        message = ELSA_text %>%
          dplyr::filter(var == "gen_map") %>%
          dplyr::pull(language),
        detail = ELSA_text %>%
          dplyr::filter(var == "be_patient") %>%
          dplyr::pull(language),
        value = 0.5
      )

      weights.temp <- calc()$wgts

      heatm <- list()

      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          heatm[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]],
              sum,
              na.rm = TRUE
            )
        } else {
          heatm[[ii]] <- theme_tbl$layers[[ii]] *
            weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }

        heatm[[ii]] <-
          heatm[[ii]] / terra::global(heatm[[ii]], max, na.rm = TRUE)$max * pu
      }

      feat_temp <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)

      feat_temp <-
        terra::ifel(
          max(feat_temp) > 0,
          feat_temp / terra::global(feat_temp, max, na.rm = TRUE)$max * pu,
          feat_temp
        )

      elsa_result <- my.data()$elsa_result

      elsa_result[is.na(elsa_result)] <- 0

      # Convert to categorical raster for mapping
      elsa_result <- prioritizr::category_layer((elsa_result * pu))

      if (input$multipri == TRUE) {
        elsa_result_multi <- my.data()$elsa_result_multi

        for (ii in 1:nrow(theme_tbl))
        {
          elsa_result_multi[[ii]][is.na(elsa_result_multi[[ii]])] <- 0

          # Convert to categorical raster for mapping
          elsa_result_multi[[ii]] <-
            prioritizr::category_layer((elsa_result_multi[[ii]] * pu))
        }
      }

      # Prioritisation Maps ####
      if (input$multipri == TRUE) {
        # Multi scenario maps ####
        outl <- fun_leaflet_elsa_multi(
          feat_temp = feat_temp,
          heatm = heatm,
          rast = elsa_result,
          rast_lst = elsa_result_multi,
          theme_tbl = theme_tbl
        )
      } else {
        # Single Scenario Maps ####
        outl <- fun_leaflet_elsa_1(
          feat_temp = feat_temp,
          heatm = heatm,
          rast = elsa_result,
          theme_tbl = theme_tbl
        )
      }

      progress$set(value = 1)
      progress$close()

      outl
    }

    # End individual run attribute table ####
  })

  #### Summary Table + Download Results raster ####
  output$summary <- DT::renderDataTable(my.data()$feat_rep_tabl,
    options = list(
      dom = "tipr",
      autoWidth = TRUE,
      pageLength = 9
    )
  )

  output$downloadSHP <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_layers_{Sys.Date()}.zip")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      # Delete geotiffs before prepping new tifs ####
      list.files(pattern = "*\\.(tif|dbf)$") %>%
        file.remove()

      files <- NULL

      progress <- Progress$new(session)
      progress$set(
        message = ELSA_text %>%
          dplyr::filter(var == "prep_raster") %>%
          dplyr::pull(language),
        detail = ELSA_text %>%
          dplyr::filter(var == "be_patient") %>%
          dplyr::pull(language),
        value = 0.5
      )

      weights.temp <- calc()$wgts

      #### Create/Write Rasters ####
      ##### ELSA Rasters ####
      elsa_result <- my.data()$elsa_result

      elsa_result[is.na(elsa_result)] <- 0

      # Convert to categorical raster
      elsa_result <-
        prioritizr::category_layer((elsa_result * pu)) %>%
        terra::classify(cbind(0, NA)) %>%
        terra::classify(cbind(NA, 255))

      NAflag(elsa_result) <- 255

      if (terra::global(elsa_result[[1]], max, na.rm = TRUE)$max == 4) {
        cls <- data.frame(
          value = 1:4,
          action = ELSA_text %>%
            filter(var %in% c(
              "protect", "restore", "manage", "green"
            )) %>%
            slice(3, 4, 2, 1) %>%
            pull(language)
        )
      } else {
        cls <- data.frame(
          value = 1:3,
          action = ELSA_text %>%
            filter(var %in% c("protect", "restore", "manage")) %>%
            slice(2, 3, 1) %>%
            pull(language)
        )
      }

      levels(elsa_result) <- cls

      names(elsa_result) <- "ELSA action"

      elsa_result %>%
        terra::writeRaster(
          glue::glue("ELSA_{Sys.Date()}.tif"),
          gdal = c(
            "COMPRESS=DEFLATE",
            "NUM_THREADS=ALL_CPUS",
            "TILED=YES",
            "OVERVIEWS=NONE"
          ),
          overwrite = TRUE,
          datatype = "INT1U",
          NAflag = 255
        )

      # Write out RAT
      foreign::write.dbf(cls, file = glue::glue("ELSA_{Sys.Date()}.tif.vat.dbf"))

      ##### Heatmaps ####
      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)

      elsa_hm <-
        terra::ifel(
          max(elsa_hm) > 0,
          elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu,
          elsa_hm
        )

      names(elsa_hm) <- "ELSA heatmap"

      elsa_hm %>%
        terra::classify(cbind(NA, -9999)) %>%
        terra::writeRaster(
          glue::glue("ELSA_HM_{Sys.Date()}.tif"),
          gdal = c(
            "COMPRESS=DEFLATE",
            "NUM_THREADS=ALL_CPUS",
            "TILED=YES",
            "OVERVIEWS=NONE"
          ),
          overwrite = TRUE,
          datatype = "FLT4S",
          NAflag = -9999
        )

      if (input$multipri == TRUE) {
        elsa_result_multi <- my.data()$elsa_result_multi

        for (ii in 1:nrow(theme_tbl))
        {
          elsa_result_multi[[ii]][is.na(elsa_result_multi[[ii]])] <- 0

          # Convert to categorical raster
          elsa_result_multi[[ii]] <-
            prioritizr::category_layer((elsa_result_multi[[ii]] * pu)) %>%
            terra::classify(cbind(0, NA)) %>%
            terra::classify(cbind(NA, 255))

          NAflag(elsa_result_multi[[ii]]) <- 255

          if (terra::global(elsa_result_multi[[ii]], max, na.rm = TRUE)$max == 4) {
            cls <- data.frame(
              value = 1:4,
              action = ELSA_text %>%
                filter(var %in% c(
                  "protect", "restore", "manage", "green"
                )) %>%
                slice(3, 4, 2, 1) %>%
                pull(language)
            )
          } else {
            cls <- data.frame(
              value = 1:3,
              action = ELSA_text %>%
                filter(var %in% c("protect", "restore", "manage")) %>%
                slice(2, 3, 1) %>%
                pull(language)
            )
          }

          levels(elsa_result_multi[[ii]]) <- cls

          names(elsa_result_multi[[ii]]) <-
            glue::glue("{theme_tbl$theme[ii]} action")

          elsa_result_multi[[ii]] %>%
            terra::writeRaster(
              glue::glue("{theme_tbl$theme[ii]}_action_{Sys.Date()}.tif") %>% gsub("/", "-", .) %>% gsub(" ", "_", .),
              gdal = c(
                "COMPRESS=DEFLATE",
                "TILED=YES",
                "NUM_THREADS=ALL_CPUS",
                "OVERVIEWS=NONE"
              ),
              overwrite = TRUE,
              datatype = "INT1U",
              NAflag = 255
            )

          # Write out RAT
          foreign::write.dbf(
            cls,
            file = glue::glue(
              "{theme_tbl$theme[ii]}_action_{Sys.Date()}.tif.vat.dbf"
            ) %>% gsub("/", "-", .) %>% gsub(" ", "_", .)
          )
        }
      }

      theme_hm <- list()

      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          theme_hm[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]], sum, na.rm = TRUE)
        } else {
          theme_hm[[ii]] <-
            theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }

        theme_hm[[ii]] <-
          terra::ifel(
            max(theme_hm[[ii]]) > 0,
            theme_hm[[ii]] / terra::global(theme_hm[[ii]], max, na.rm = TRUE)$max * pu,
            theme_hm[[ii]]
          )

        names(theme_hm[[ii]]) <-
          glue::glue("{theme_tbl$theme[ii]} heatmap")

        theme_hm[[ii]] %>%
          terra::classify(cbind(NA, -9999)) %>%
          terra::writeRaster(
            glue::glue("{theme_tbl$theme[ii]}_HM_{Sys.Date()}.tif") %>% gsub("/", "-", .) %>% gsub(" ", "_", .),
            gdal = c(
              "COMPRESS=DEFLATE",
              "NUM_THREADS=ALL_CPUS",
              "TILED=YES",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            datatype = "FLT4S",
            NAflag = -9999
          )
      }

      files <- list.files(pattern = "*\\.(tif|dbf)$")

      files <- files[!grepl("spat", files)]

      progress$set(value = 1)

      progress$close()

      # Create the zip file ####
      zip::zip(file, files)
    }
  )

  output$download_ssoln_csv <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_summary_results_{Sys.Date()}.csv")
    },
    content = function(file) {
      readr::write_csv(my.data()$feat_rep_tabl, file)
    }
  )

  output$download_ssoln_xlsx <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_summary_results_{Sys.Date()}.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(my.data()$feat_rep_tabl, file)
    }
  )

  output$download_params_csv <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_model_parameters_{Sys.Date()}.csv")
    },
    content = function(file) {
      if (terra::global(my.data()$elsa_result[[1]], max, na.rm = TRUE)$max == 4) {
        tidyr::pivot_longer(
          data = tibble(
            `Multi-theme prioritisation` = input$multipri,
            `Protected areas lock-in` = input$protected,
            `Protect budget` = input$zone_1_target,
            `Restore budget` = input$zone_2_target,
            `Manage budget` = input$zone_3_target,
            `Urban-greening budget` = input$zone_4_target,
            `Boundary Penalty Factor` = input$blm
          ),
          everything(),
          names_to = "Parameter",
          values_to = "Value",
          values_transform = list(Value = as.character)
        ) %>%
          readr::write_csv(file, col_names = TRUE)
      } else {
        tidyr::pivot_longer(
          data = tibble(
            `Multi-theme prioritisation` = input$multipri,
            `Protected areas lock-in` = input$protected,
            `Protect budget` = input$zone_1_target,
            `Restore budget` = input$zone_2_target,
            `Manage budget` = input$zone_3_target,
            `Boundary Penalty Factor` = input$blm,
          ),
          everything(),
          names_to = "Parameter",
          values_to = "Value",
          values_transform = list(Value = as.character)
        ) %>%
          readr::write_csv(file, col_names = TRUE)
      }
    }
  )
}
