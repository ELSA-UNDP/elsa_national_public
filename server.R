server <- function(input, output, session) {
  # Helper function to enforce min values in the UI
  reset_and_enforce_min <- function(input_id, temp_reset_value, min_value) {
    # Reset the input to temporary reset value to clear any previous state
    updateNumericInput(session, input_id, min = 0, value = temp_reset_value)
    # Set the actual min value after the reset
    updateNumericInput(session,
                       input_id,
                       min = min_value,
                       value = max(input[[input_id]], min_value))
  }
  
  # Monitor the lock-in option and update the min budget values dynamically
  observeEvent(input$protected, {
    selected_value <- input$protected
    
    # Reset and enforce min values based on the selected protection
    reset_and_enforce_min("zone_1_target",
                          temp_reset_value = -1,
                          min_value = 0)
    reset_and_enforce_min("zone_2_target",
                          temp_reset_value = -1,
                          min_value = 0)
    reset_and_enforce_min("zone_3_target",
                          temp_reset_value = -1,
                          min_value = 0)
    
    # Dynamically set min values based on input$protected
    isolate({
      if (selected_value == prot_lst[1]) {
        reset_and_enforce_min("zone_1_target",
                              temp_reset_value = 0,
                              min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target",
                              temp_reset_value = 0,
                              min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target",
                              temp_reset_value = 0,
                              min_value = 0)
      } else if (selected_value == prot_lst[2]) {
        reset_and_enforce_min("zone_1_target",
                              temp_reset_value = 0,
                              min_value = paoecm_lockin_min_budget)
        reset_and_enforce_min("zone_2_target",
                              temp_reset_value = 0,
                              min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target",
                              temp_reset_value = 0,
                              min_value = 0)
      } else if (selected_value == prot_lst[3]) {
        reset_and_enforce_min("zone_1_target",
                              temp_reset_value = 0,
                              min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target",
                              temp_reset_value = 0,
                              min_value = restore_snap_lockin_min_budget)
        reset_and_enforce_min("zone_3_target",
                              temp_reset_value = 0,
                              min_value = 0)
      } else if (selected_value == prot_lst[4]) {
        reset_and_enforce_min("zone_1_target",
                              temp_reset_value = 0,
                              min_value = paoecm_lockin_min_budget)
        reset_and_enforce_min("zone_2_target",
                              temp_reset_value = 0,
                              min_value = restore_snap_lockin_min_budget)
        reset_and_enforce_min("zone_3_target",
                              temp_reset_value = 0,
                              min_value = 0)
      } else if (selected_value == prot_lst[5]) {
        reset_and_enforce_min("zone_1_target",
                              temp_reset_value = 0,
                              min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target",
                              temp_reset_value = 0,
                              min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target",
                              temp_reset_value = 0,
                              min_value = 0)
      } else {
        reset_and_enforce_min("zone_1_target",
                              temp_reset_value = 0,
                              min_value = default_protect_min_budget)
        reset_and_enforce_min("zone_2_target",
                              temp_reset_value = 0,
                              min_value = default_restore_min_budget)
        reset_and_enforce_min("zone_3_target",
                              temp_reset_value = 0,
                              min_value = 0)
      }
    })
  })
  
  # Monitor the individual target inputs and dynamically enforce minimum values based on input$protected
  observeEvent(input$zone_1_target, {
    selected_value <- input$protected
    if (selected_value == prot_lst[1] ||
        selected_value == prot_lst[3] || selected_value == prot_lst[5]) {
      reset_and_enforce_min(
        "zone_1_target",
        temp_reset_value = input$zone_1_target,
        min_value = default_protect_min_budget
      )
    } else if (selected_value == prot_lst[2] ||
               selected_value == prot_lst[4]) {
      reset_and_enforce_min(
        "zone_1_target",
        temp_reset_value = input$zone_1_target,
        min_value = paoecm_lockin_min_budget
      )
    }
  })
  
  observeEvent(input$zone_2_target, {
    selected_value <- input$protected
    if (selected_value == prot_lst[1] ||
        selected_value == prot_lst[2] || selected_value == prot_lst[5]) {
      reset_and_enforce_min(
        "zone_2_target",
        temp_reset_value = input$zone_2_target,
        min_value = default_restore_min_budget
      )
    } else if (selected_value == prot_lst[3] ||
               selected_value == prot_lst[4]) {
      reset_and_enforce_min(
        "zone_2_target",
        temp_reset_value = input$zone_2_target,
        min_value = restore_snap_lockin_min_budget
      )
    }
  })
  
  # No observeEvent() required for zone 3 budget
  
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
        ELSA_text  |>
          dplyr::filter(var %in% c("data", "theme", "weight", "policy"))  |>
          dplyr::slice(1, 3, 4, 2) |>
          dplyr::pull(language),
        "feature"
      )
    )  |>
      hot_table(highlightCol = TRUE, highlightRow = TRUE) |>
      hot_col(ELSA_text |>
                filter(var %in% c("weight")) |>
                pull(language), readOnly = FALSE) |>
      hot_col(col = "feature", colWidths = 0.1)  |> # Small width hides column
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  #### Reactive Block ####
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
        message = ELSA_text |> dplyr::filter(var == "setup") |> dplyr::pull(language),
        detail = ELSA_text |> dplyr::filter(var == "be_patient") |> dplyr::pull(language),
        value = 0.01
      )
      
      pu_temp <- pu_all[["area"]][[input$protected]]
      
      prob.ta <-
        prioritizr::problem(pu_temp, zns, run_checks = FALSE) |>
        prioritizr::add_default_solver(gap = 0.05, threads = 4) #add_gurobi_solver(gap = 0.05, threads = 8)
      
      
      if (input$protected == "avail") {
        target <- get_min_lockin_target(c(PA0, PA0, PA0), input, pu)
        
        prob.ta <- prob.ta |>
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          ))
      }
      
      if (input$protected == "locked") {
        target <- get_min_lockin_target(c(PA, PA0, PA0), input, pu)
        
        prob.ta <- prob.ta |>
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          )) |>
          prioritizr::add_locked_in_constraints(c(PA, PA0, PA0))
      }
      
      if (input$protected == "restore") {
        target <- get_min_lockin_target(c(PA0, Rest, PA0), input, pu)
        
        prob.ta <- prob.ta |>
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          )) |>
          prioritizr::add_locked_in_constraints(c(PA0, Rest, PA0))
      }
      
      if (input$protected == "pa_restore") {
        target <- get_min_lockin_target(c(PA, Rest, PA0), input, pu)
        
        prob.ta <- prob.ta |>
          prioritizr::add_max_utility_objective(c(
            count_tar(pu0, target[1]),
            count_tar(pu0, target[2]),
            count_tar(pu0, target[3])
          )) |>
          prioritizr::add_locked_in_constraints(c(PA, Rest, PA0))
      }
      
      #### Boundary Penalty Factor ####
      if (input$blm > 0) {
        prob.ta <- prob.ta |>
          prioritizr::add_boundary_penalties(penalty = input$blm / 10000)
      }
      
      if (input$multipri == TRUE) {
        progress$set(
          message = ELSA_text |> dplyr::filter(var == "calc") |>  dplyr::pull(language),
          detail = paste(
            ELSA_text  |> dplyr::filter(var == "calc")  |> dplyr::pull(language),
            sprintf("1/%s", (1 + nrow(theme_tbl)))
          ),
          value = round(1 / (2 + nrow(theme_tbl)), 1)
        )
      } else {
        progress$set(
          message = ELSA_text |> dplyr::filter(var == "calc") |>  dplyr::pull(language),
          detail = ELSA_text |> dplyr::filter(var == "run") |>  dplyr::pull(language),
          value = 0.5
        )
      }
      
      #### Add weights to conservation problem ####
      prob.all <- prob.ta |>
        prioritizr::add_feature_weights(as.matrix(matrix(
          rep(weights.temp$weight, 3),
          ncol = 3,
          nrow = terra::nlyr(feat_stack)
        )))
      
      #### Solve conservation problem ####
      elsa_result <- solve.ConservationProblem(prob.all, force = TRUE)
      
      #### Convert to categorical raster only one time ####
      elsa_raster <- make_elsa_categorical_raster(elsa_result)
      
      #### Get feature representation ####
      
      # First get overall info from prioritizr
      overall_rep <- prioritizr::eval_feature_representation_summary(prob.all, elsa_result) |>
        dplyr::rename(zone = summary) |> 
        filter(zone == "overall") |> 
        dplyr::select("feature", "absolute_held") |> 
        dplyr::left_join(overall_raw_df, by = "feature") |> 
        dplyr::mutate(relative_held_overall = absolute_held / total_amount)
      
      # Calculate relative values for each zone and pivot them into new columns
      relative_rep <- prioritizr::eval_feature_representation_summary(prob.all, elsa_result)  |> 
        dplyr::rename(zone = summary)  |>
        filter(zone != "overall")  |>
        dplyr::select("feature", "absolute_held", "zone")  |>
        dplyr::left_join(overall_raw_df, by = "feature")  |>
        dplyr::mutate(relative_held_zone = absolute_held / total_amount)  |>
        dplyr::select("zone", "feature", "relative_held_zone")  |>
        tidyr::pivot_wider(names_from = zone, values_from = relative_held_zone)
      
      # Combine the overall totals with the relative values
      feat_rep <- overall_rep  |>
        dplyr::select("feature", "relative_held_overall")  |>
        dplyr::left_join(relative_rep, by = "feature")  |>
        dplyr::rename(
          "{ELSA_text |>  filter(var == 'protect')  |>  pull(language)}" := Protect,
          "{ELSA_text |>  filter(var == 'restore') |>  pull(language)}" := Restore,
          "{ELSA_text |>  filter(var == 'manage')  |>  pull(language)}" := Manage
        )
      
      elsa_result_multi <- feat_rep.lst <- list()
      
      if (input$multipri == TRUE) {
        for (ii in 1:nrow(theme_tbl)) {
          progress$set(
            message = ELSA_text |>  dplyr::filter(var == "calc")  |>  dplyr::pull(language),
            detail = paste(
              ELSA_text |>  dplyr::filter(var == "calc") |>  dplyr::pull(language),
              sprintf("%s/%s", 1 + ii, (1 + nrow(
                theme_tbl
              )))
            ),
            value = round((1 + ii) / (2 + nrow(
              theme_tbl
            )), 1)
          )
          
          wgt.tmp <- weights.temp
          
          wgt.tmp$weight[names(feat_stack) %nin% theme_tbl$names[[ii]]] <- 0
          
          prob.tmp <- prob.ta |>
            prioritizr::add_feature_weights(as.matrix(matrix(
              rep(wgt.tmp$weight, 3),
              ncol = 3,
              nrow = terra::nlyr(feat_stack)
            )))
          
          #### ELSA Result ####
          elsa_result_multi[[ii]] <- solve.ConservationProblem(prob.tmp, force = TRUE)
          
          #### Convert to categorical raster and add to existing ELSA raster - so makes a stacked raster
          elsa_raster <- c(elsa_raster,
                           make_elsa_categorical_raster(elsa_result_multi[[ii]]))
          
          # First get overall info from prioritizr
          overall_rep <- prioritizr::eval_feature_representation_summary(prob.tmp, elsa_result_multi[[ii]])  |>
            dplyr::rename(zone = summary)  |>
            filter(zone == "overall")  |>
            dplyr::select("feature", "absolute_held")  |>
            dplyr::left_join(overall_raw_df, by = "feature")  |>
            dplyr::mutate(relative_held_overall = absolute_held / total_amount)
          
          # Calculate relative values for each zone and pivot them into new columns
          relative_rep <- prioritizr::eval_feature_representation_summary(prob.tmp, elsa_result_multi[[ii]])  |>
            dplyr::rename(zone = summary)  |>
            filter(zone != "overall")  |>
            dplyr::select("feature", "absolute_held", "zone")  |>
            dplyr::left_join(overall_raw_df, by = "feature")  |>
            dplyr::mutate(relative_held_zone = absolute_held / total_amount)  |>
            dplyr::select("zone", "feature", "relative_held_zone")  |>
            tidyr::pivot_wider(names_from = zone, values_from = relative_held_zone)
          
          # Combine the overall totals with the relative values
          feat_rep.lst[[ii]] <- overall_rep  |>
            dplyr::select("feature", "relative_held_overall")  |>
            dplyr::left_join(relative_rep, by = "feature")  |>
            dplyr::rename(
              "{ELSA_text |>  filter(var == 'protect')  |>  pull(language)}" := Protect,
              "{ELSA_text |>  filter(var == 'restore') |>  pull(language)}" := Restore,
              "{ELSA_text |>  filter(var == 'manage')  |>  pull(language)}" := Manage
            )

          rm(wgt.tmp, prob.tmp)
        }
      }

      #### Calculate representation summaries ####
      progress$set(
        message = ELSA_text |> dplyr::filter(var == "post") |> dplyr::pull(language),
        detail = ELSA_text |> dplyr::filter(var == "post_help") |> dplyr::pull(language),
        value = 0.9
      )

      if (input$multipri == TRUE) {
        feature_rep_tabl_multi <- purrr::reduce(seq_along(feat_rep.lst), function(x, i) {
          df <- feat_rep.lst[[i]][, c(1, 2)]
          colnames(df)[2] <- themes[[i]]
          # Join the data frames
          dplyr::left_join(x, df, by = setNames(names(x)[1], names(df)[1]))
        }, .init = feat_rep[, c(1, 2)])  |>
          tibble::add_column(
            Name = feat_df$label,
            Theme = feat_df$theme,
            .before = 1
          ) |>
          dplyr::rename(ELSA = "relative_held_overall") |>
          dplyr::select(-"feature") |>
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 100, 1))) |>
          dplyr::rowwise() |>
          dplyr::mutate(elsa_tradeoff = round(ELSA / max(c_across(4:6)) * 100, 1)) |>
          dplyr::rename(
            "{ELSA_text  |> filter(var == 'elsa_tradeoff')  |> pull(language)}" := elsa_tradeoff
          )
      } 
      
      feature_rep_tabl <- feat_rep  |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 100, 1)))  |>
        dplyr::select(2:5)  |>
        tibble::add_column(Name = feat_df$label,
                           Theme = feat_df$theme,
                           .before = 1) |>
        dplyr::rename(
          "{ELSA_text  |> filter(var == 'data') |> pull(language)}" := Name,
          "{ELSA_text |> filter(var == 'theme')  |> pull(language)}" := Theme,
          "{ELSA_text |> filter(var == 'overall')  |> pull(language)}" := relative_held_overall
        )
      
    
      
      rlist <- list(
        elsa_raster = elsa_raster,
        feature_rep_tabl = feature_rep_tabl
      )
      
      if (input$multipri) {
        rlist$feature_rep_tabl_multi <- feature_rep_tabl_multi
      }
      
      progress$set(value = 1)
      
      progress$close()
      
      return(rlist)
    }))
  })
  
  observe({
    my.data()
  })
  
  #### Leaflet Inputs Map ####
  output$InMap <- renderLeaflet({
    progress <- Progress$new(session)
    
    progress$set(
      message = ELSA_text |> filter(var == "gen_map") |> pull(language),
      detail = ELSA_text |> filter(var == "be_patient") |> pull(language),
      value = 0.5
    )
    
    outl <- fun_leaflet_input(layers = feat_stack, labels = feat_df$label)
    
    progress$set(value = 1)
    
    progress$close()
    
    outl
  })
  
  #### Leaflet Results Map ####
  output$cadMap <- renderLeaflet({
    if (input$mrun == 0) {
      progress <- Progress$new(session)
      
      progress$set(
        message = ELSA_text |> filter(var == "gen_map") |> pull(language),
        detail = ELSA_text |> filter(var == "be_patient") |> pull(language),
        value = 0.5
      )
      
      weights.temp <- calc()$wgts
      
      heatm_lst <- list()
      
      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          heatm_lst[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]], sum, na.rm = TRUE)
        } else {
          heatm_lst[[ii]] <- theme_tbl$layers[[ii]] *
            weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }
        
        heatm_lst[[ii]] <-
          heatm_lst[[ii]] / terra::global(heatm_lst[[ii]], max, na.rm = TRUE)$max * pu
      }
      
      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)
      
      elsa_hm <-
        elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu
      
      ##### No Run Maps ####
      outl <- fun_leaflet_elsa_0(elsa_hm = elsa_hm,
                                 heatm_lst = heatm_lst,
                                 theme_tbl = theme_tbl)
      
      progress$set(value = 1)
      progress$close()
      
      outl
    } else {
      progress <- Progress$new(session)
      progress$set(
        message = ELSA_text |> dplyr::filter(var == "gen_map") |> dplyr::pull(language),
        detail = ELSA_text |> dplyr::filter(var == "be_patient") |> dplyr::pull(language),
        value = 0.5
      )
      
      weights.temp <- calc()$wgts
      
      heatm_lst <- list()
      
      for (ii in 1:nrow(theme_tbl))
      {
        if (terra::nlyr(theme_tbl$layers[[ii]]) > 1) {
          heatm_lst[[ii]] <-
            terra::app(theme_tbl$layers[[ii]] * weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]], sum, na.rm = TRUE)
        } else {
          heatm_lst[[ii]] <- theme_tbl$layers[[ii]] *
            weights.temp$weight[weights.temp$feature %in% theme_tbl$names[[ii]]]
        }
        
        heatm_lst[[ii]] <-
          heatm_lst[[ii]] / terra::global(heatm_lst[[ii]], max, na.rm = TRUE)$max * pu
      }
      
      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)
      
      elsa_hm <-
        elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu
      
      ##### Prioritisation Maps ####
      
      outl <- fun_leaflet_elsa_1(
        multi_theme = input$multipri,
        elsa_hm = elsa_hm,
        heatm_lst = heatm_lst,
        rast = my.data()$elsa_raster,
        theme_tbl = theme_tbl
      )
      
      progress$set(value = 1)
      progress$close()
      
      outl
    }
    
    # End individual run attribute table ####
  })
  
  #### Summary Table + Download Results raster ####
  output$summary <- DT::renderDataTable(
    if (!input$multipri) {
      my.data()$feature_rep_tabl
    }
    else {
      my.data()$feature_rep_tabl_multi
    }, 
    options = list(
    dom = "tipr",
    autoWidth = TRUE,
    pageLength = 9
  ))
  
  # Representation figure - uses the ELSA output only, not theme specific outputs.
  output$gg_repStacked <- shiny::renderPlot(
    elsar_plot_repStacked(my.data()$feature_rep_tabl, input)
    )

  output$downloadSHP <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_layers_{Sys.Date()}.zip")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      #### Delete geotifs before prepping new tifs ####
      list.files(pattern = "*\\.(tif|xml)$")  |>
        file.remove()
      
      files <- NULL
      
      progress <- Progress$new(session)
      progress$set(
        message = ELSA_text |> dplyr::filter(var == "prep_raster") |> dplyr::pull(language),
        detail = ELSA_text |> dplyr::filter(var == "be_patient") |> dplyr::pull(language),
        value = 0.5
      )
      
      weights.temp <- calc()$wgts
      
      if (input$multipri == FALSE) {
#### Create/Write Rasters ####
        elsa_raster <- my.data()$elsa_raster
        
        activeCat(elsa_raster) <- 3 # Set active category to the action label in the language used
        
        names(elsa_raster) <- glue("ELSA {ELSA_text  |> filter(var == 'action')  |> pull(language)}")
        
        elsa_raster  |>
          terra::writeRaster(
            glue::glue("ELSA_{Sys.Date()}.tif"),
            gdal = c(
              "COMPRESS=DEFLATE",
              "NUM_THREADS=ALL_CPUS",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            datatype = "INT1U",
            filetype = "COG",
            NAflag = 255
          )
      } else if (input$multipri == TRUE) {
        elsa_raster <- my.data()$elsa_raster
        
        for (i in 1:nlyr(elsa_raster)) {
          # Set active category for each layer to label
          activeCat(elsa_raster[[i]]) <- 3
        }
        
        layer_names <- c("ELSA", theme_tbl$theme)
        
        elsa_raster |>
          terra::writeRaster(
            c(glue::glue("{layer_names[1]}_{Sys.Date()}.tif"),
              glue::glue("ELSA_{gsub(' ', '_', layer_names[-1])}_{Sys.Date()}.tif")),
            gdal = c(
              "COMPRESS=DEFLATE",
              "NUM_THREADS=ALL_CPUS",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            filetype = "COG",
            datatype = "INT1U",
            NAflag = 255
          )
      }
      
      #### Heatmaps ####
      elsa_hm <-
        terra::app(feat_stack * weights.temp$weight, sum, na.rm = TRUE)
      
      elsa_hm <-
        terra::ifel(
          max(elsa_hm) > 0,
          elsa_hm / terra::global(elsa_hm, max, na.rm = TRUE)$max * pu,
          elsa_hm
        )
      
      names(elsa_hm) <- "ELSA heatmap"
      
      elsa_hm  |>
        terra::classify(cbind(NA, -9999))  |>
        terra::writeRaster(
          glue::glue("ELSA_HM_{Sys.Date()}.tif"),
          gdal = c(
            "COMPRESS=DEFLATE",
            "NUM_THREADS=ALL_CPUS",
            "OVERVIEWS=NONE"
          ),
          overwrite = TRUE,
          datatype = "FLT4S",
          filetype = "COG",
          NAflag = -9999
        )
      
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
        
        theme_hm[[ii]]  |>
          terra::classify(cbind(NA, -9999))  |>
          terra::writeRaster(
            glue::glue("{theme_tbl$theme[ii]}_HM_{Sys.Date()}.tif")  |> gsub("/", "-", .)  |> gsub(" ", "_", .),
            gdal = c(
              "COMPRESS=DEFLATE",
              "NUM_THREADS=ALL_CPUS",
              "OVERVIEWS=NONE"
            ),
            overwrite = TRUE,
            datatype = "FLT4S",
            filetype = "COG",
            NAflag = -9999
          )
      }
      
      files <- list.files(pattern = "*\\.(tif|xml)$")
      
      files <- files[!grepl("spat", files)]
      
      progress$set(value = 1)
      
      progress$close()
      
      # Create the zip file ####
      zip::zip(file, files)
    }
  )
  
  output$download_ssoln_xlsx <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_summary_results_{Sys.Date()}.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(my.data()$feature_rep_tabl, file)
    }
  )
  
  output$download_params_csv <- downloadHandler(
    filename = function() {
      glue::glue("ELSA_model_parameters_{Sys.Date()}.csv")
    },
    content = function(file) {
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
      )  |>
        readr::write_csv(file, col_names = TRUE)
    }
  )
}
