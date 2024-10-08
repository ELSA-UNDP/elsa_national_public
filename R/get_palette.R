get_palette <- function(palette = pal.elsa,
                        in_rast = NULL) {
  unique.vals <- terra::unique(in_rast, na.rm = TRUE) %>%
    pull(1)

  if (length(pal.elsa) == 4) {
    breaks <- 1:4

    tb <- tibble::tibble(
      colour = palette,
      breaks = breaks,
      label = ELSA_text %>%
        filter(var %in% c(
          "protect", "restore", "manage", "green"
        )) %>%
        slice(3, 4, 2, 1) %>%
        pull(language)
    )
  } else {
    breaks <- 1:3

    tb <- tibble::tibble(
      colour = palette,
      breaks = breaks,
      label = ELSA_text %>%
        filter(var %in% c("protect", "restore", "manage")) %>%
        slice(2, 3, 1) %>%
        pull(language)
    )
  }

  tb %>%
    filter(breaks %in% unique.vals) %>%
    arrange(breaks)
}
