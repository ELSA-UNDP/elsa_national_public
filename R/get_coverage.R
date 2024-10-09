#' Function to calculate country area coverage. Can be used to get the minimum budget for a specific zone.
#'
#' @param zone_layer A `SpatRaster` that contains information of the planning units covering an area.
#' @param pu_layer A `SpatRaster` with the total number of planning units in an area.
#'
#' @return A vector of areal targets of length 1.
#' @export
get_coverage <- function(zone_layer, pu_layer) {
  terra::global(zone_layer, sum, na.rm = TRUE)$sum / terra::global(pu_layer, sum, na.rm = TRUE)$sum * 100
}
