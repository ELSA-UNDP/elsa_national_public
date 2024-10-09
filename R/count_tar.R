#' Function to get country specific budget value
#'
#' @param PU A terra `SpatRaster` that can be used to calculate the total number of planning units available
#' @param targets A numeric value containing the percentage value (0-1) of the budget for a specifc country for a specific zone.
#'
#' @return A numeric value of the country-specific budget (number of planning units).
#' @export
count_tar <- function(PU = NULL,
                      target = NULL) {
  tar <-
    round(terra::global(PU, "sum", na.rm = TRUE) / 100 * target, 0)$sum
  
  ifelse(tar == 0, 1e-4, tar)
}
