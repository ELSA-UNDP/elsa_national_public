elsar_plot_repStacked <- function(feature_rep_tabl, input) {

data_long <- feature_rep_tabl %>%
  tidyr::pivot_longer(cols = c("Protect", "Restore", "Manage"), # Pivot the three relevant columns
               names_to = "Category",
               values_to = "Percentage")  %>%
  dplyr::arrange(Data) %>%
  dplyr::mutate(Category = as.factor(.data$Category),
                Theme = as.factor(.data$Theme))

# create legend text based on selected budgets
protect_budget <- paste0("Protect (", input$zone_1_target, "%)")
restore_budget <- paste0("Restore (", input$zone_2_target, "%)")
manage_budget <- paste0("Manage (", input$zone_3_target, "%)")

# create stacked bar plot
gg_rep_stacked <- ggplot2::ggplot(data_long, ggplot2::aes(x = Data, y = Percentage, fill = Category)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::scale_fill_manual(values = c("Protect" = "#550182",
                               "Restore" = "#059674",
                               "Manage" = "#fcba03"),
                    name = "ELSA actions",
                    labels = c(protect_budget, restore_budget, manage_budget),
                    breaks = c("Protect", "Restore", "Manage")) + # Order of legend items
  ggplot2::coord_flip() +
  ggplot2::labs(x = " ",
       y = "Representation (%)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "top",
        legend.background = ggplot2::element_rect(color = "black", linewidth = 0.5),
        legend.text = ggplot2::element_text(size = 12),
        legend.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 12, color = "black"),
        axis.title = ggplot2::element_text(size = 12,  color = "black"),
        panel.border = ggplot2::element_rect(color = "black", linewidth = 1, fill = NA)
      ) #+ # Add boundary box around the plot

return(gg_rep_stacked)

}