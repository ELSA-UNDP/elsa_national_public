get_googlesheet <- function(ss = NULL,
                            sheet = NULL,
                            range = NULL,
                            col_types = NULL) {
  googlesheets4::gs4_deauth()

  googlesheets4::read_sheet(ss, sheet, range, trim_ws = TRUE, col_types = col_types)
}
