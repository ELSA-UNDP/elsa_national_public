# get country specific target value
count_tar <- function(PU = NULL,
                      target = NULL) {
  tar <-
    round(terra::global(PU, "sum", na.rm = TRUE) / 100 * target, 0)$sum
  
  ifelse(tar == 0, 1e-4, tar)
}
