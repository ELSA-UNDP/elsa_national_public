#' Calculate country area coverage
#'
#' @inheritParams spatRast
#' @inheritParams spatRast
#' @return A vector of areal targets of length 1.
#'
#' @seealso [terra::global()] which this function wraps.
#' @export
#' @examples
#' get_coverage(PA, pu)
#' get_coverage(zone_protect, pu)
#' get_coverage(zone_manage, pu)
get_coverage <- function(zone_layer, pu_layer) {
  terra::global(zone_layer, sum, na.rm = TRUE)$sum / terra::global(pu_layer, sum, na.rm = TRUE)$sum * 100
}
