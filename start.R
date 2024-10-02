list.of.packages <- c(
  "shiny",
  "shinydashboard",
  "shinyIncubator",
  "prioritizr",
  "here",
  "readxl",
  "writexl",
  "plyr",
  "dplyr",
  "tibble",
  "readr",
  "sf",
  "ggplot2",
  "foreign",
  "vegan",
  "terra",
  "leaflet",
  "rhandsontable",
  "googlesheets4",
  "Matrix",
  "tidyr",
  "gurobi",
  "zip",
  "DT",
  "argparser"
)

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages))
  install.packages(new.packages, repos = "https://cran.rstudio.com/")

if (!require(leaflet)) {
  if (!require("devtools"))
    install.packages("devtools")
  devtools::install_github("rstudio/leaflet")
}

if (!require(shinyIncubator)) {
  devtools::install_github("rstudio/shiny-incubator")
}

shiny::runApp(launch.browser = TRUE)
