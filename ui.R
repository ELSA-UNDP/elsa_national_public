ui <- dashboardPage(
  #### Header ####
  dashboardHeader(
    title = glue::glue(
      "{ELSA_text %>%
                    filter(var == 'title') %>%
                    pull(language)} {country}"
    ),
    titleWidth = 650
  ),
  
  #### Sideboard ####
  dashboardSidebar(disable = TRUE),
  
  #### Body ####
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$style(
      # Needed to overwrite dataTable CSS for paginators
      HTML(
        ".dataTables_wrapper .dataTables_paginate .paginate_button {
           box-sizing:border-box;
           display:inline-block;
           min-width:1.5em;
           padding:.5em 1em;
           margin-left:2px;
           text-align:center;
           text-decoration:none !important;
           cursor:pointer;
           *cursor:hand;
           color:#ffffff90 !important;
           border:1px solid transparent;
           border-radius:2px !important;
           background: #343A40 !important
           }"
      )
    ),
    tags$style(
      HTML(
        ".dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
           .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover,
           .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:active {
           cursor:default;
           color:#ffffff90 !important;
           border: none !important;
           background: #343A40 !important;
           box-shadow:none
           }"
      )
    ),
    tags$style(
      HTML(
        ".dataTables_wrapper .dataTables_paginate .paginate_button.current,
           .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
           color:#ffffff !important;
           border:1px solid #83E8F0 !important;
           background-color: #343A40 !important;
           }"
      )
    ),
    
    # Boxes need to be put in a row (or column)
    fluidRow(column(
      width = 3,
      box(
        title = ELSA_text %>%
          filter(var == "subtitle") %>%
          pull(language),
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "in",
        width = NULL,
        height = "100%",
        actionButton("mrun",
                     HTML(
                       "<h4>",
                       ELSA_text %>%
                         filter(var == "button1") %>%
                         pull(language),
                       "</h4>"
                     )),
        helpText("================================="),
        helpText(
          HTML(
            "<h4><strong>",
            ELSA_text %>%
              filter(var == "global_par") %>%
              pull(language),
            ":</strong></h4>"
          )
        ),
        checkboxInput(
          "multipri",
          ELSA_text %>%
            filter(var == "multipr") %>%
            pull(language),
          FALSE
        ),
        selectInput(
          "protected",
          ELSA_text %>%
            filter(var == "protect_lock") %>%
            pull(language),
          prot_lst
        ),
        # Update if other areas need to be locked in
        tags$hr(),
        helpText(HTML(
          ELSA_text %>%
            filter(var == "help_value") %>%
            pull(language)
        )),
        numericInput(
          "zone_1_target",
          ELSA_text %>%
            filter(var == "tar_prot") %>%
            pull(language),
          protect_budget,
          min = 0,
          max = 100,
          step = 0.1
        ),
        numericInput(
          "zone_2_target",
          ELSA_text %>%
            filter(var == "tar_rest") %>%
            pull(language),
          restore_budget,
          min = 0,
          max = 100,
          step = 0.11
        ),
        numericInput(
          "zone_3_target",
          ELSA_text %>%
            filter(var == "tar_man") %>%
            pull(language),
          manage_budget,
          min = 0,
          max = 100,
          step = 0.1
        ),
        tags$hr(),
        helpText(HTML(
          ELSA_text %>%
            filter(var == "blm_help") %>%
            pull(language)
        )),
        numericInput(
          "blm",
          ELSA_text %>%
            filter(var == "blm") %>%
            pull(language),
          0,
          min = 0,
          max = 1e10,
          step = 0.1
        )
      )
    ),
    column(
      width = 9,
      tabBox(
        title = "",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "out",
        width = NULL,
        height = "100%",
        tabPanel(
          ELSA_text %>%
            filter(var == "tab_wgt") %>%
            pull(language),
          id = "rhweights",
          helpText(ELSA_text %>%
                     filter(var == "wgt_help") %>%
                     pull(language)),
          rHandsontableOutput("hot_wgt")
        ),
        tabPanel(
          ELSA_text %>%
            filter(var == "tab_input") %>%
            pull(language),
          leafletOutput("InMap", height = 700)
        ),
        tabPanel(
          ELSA_text %>%
            filter(var == "tab_res") %>%
            pull(language),
          helpText(HTML(
            "<h4>",
            ELSA_text %>%
              filter(var == "res_tab") %>%
              pull(language),
            "</h4>"
          )),
          helpText(HTML(
            ELSA_text %>%
              filter(var == "res_tab_help")
            %>% pull(language)
          )),
          DT::dataTableOutput("summary"),
          helpText(HTML("<br>")),
          shiny::plotOutput("gg_repStacked", width = "60%"), 
          helpText(HTML("<br>")),
          helpText(HTML(
            "<h4>",
            ELSA_text %>%
              filter(var == "res_down") %>%
              pull(language),
            "</h4>"
          )),
          downloadButton(
            "downloadSHP",
            label = glue("{ELSA_text %>% filter(var == 'd_load') %>% pull(language)}")
          ),
          helpText(HTML("<br>")),
          helpText(HTML(
            "<h4>",
            ELSA_text %>%
              filter(var == "res_tab_down") %>%
              pull(language),
            "</h4>"
          )),
          downloadButton(
            "download_ssoln_xlsx",
            label = glue(
              "{ELSA_text %>% filter(var == 'd_load') %>% pull(language)} {ELSA_text %>%  filter(var == 'summary') %>% pull(language)} (Excel)"
            )
          ),
          downloadButton(
            "download_params_csv",
            label = glue(
              "{ELSA_text %>% filter(var == 'd_load') %>% pull(language)} {ELSA_text %>%  filter(var == 'param') %>% pull(language)} (CSV)"
            )
          ),
          helpText(HTML("<br>"))
        ),
        tabPanel(
          ELSA_text
          %>% filter(var == "tab_map") %>%
            pull(language),
          leafletOutput("cadMap", height = 700)
        )
      )
    ))
  )
)
