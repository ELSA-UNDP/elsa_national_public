#' A function to get the minimum budget needed with areas locked-in
#'
#' @param lockin A `SpatRaster` or stack of `SpatRaster` with the lock-in areas for a prioritization
#' @param input The input object from the Shiny application that inlcudes the user set budgets
#' @param pu A planning unit `SpatRaster`
#'
#' @return A list of minimum budgets for the prioritization
#' @export
#'
get_min_lockin_target <- function(lockin, input, pu) {
  min_coverage <- get_coverage(lockin, pu)

  if (!is.null(input$zone_4_target)) {
    targets <- tibble::tibble(
      zone_1_target = ifelse(
        input$zone_1_target < min_coverage[1],
        min_coverage[1],
        input$zone_1_target
      ),
      zone_2_target = ifelse(
        input$zone_2_target < min_coverage[2],
        min_coverage[2],
        input$zone_2_target
      ),
      zone_3_target = ifelse(
        input$zone_3_target < min_coverage[3],
        min_coverage[3],
        input$zone_3_target
      ),
      zone_4_target = ifelse(
        input$zone_4_target < min_coverage[4],
        min_coverage[4],
        input$zone_4_target
      )
    )
  } else {
    targets <- tibble::tibble(
      zone_1_target = ifelse(
        input$zone_1_target < min_coverage[1],
        min_coverage[1],
        input$zone_1_target
      ),
      zone_2_target = ifelse(
        input$zone_2_target < min_coverage[2],
        min_coverage[2],
        input$zone_2_target
      ),
      zone_3_target = ifelse(
        input$zone_3_target < min_coverage[3],
        min_coverage[3],
        input$zone_3_target
      )
    )
  }
}
