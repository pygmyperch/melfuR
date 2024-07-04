#' View Color Palette
#'
#' This function plots the MDB color palette.
#'
#' @importFrom ggplot2 ggplot geom_tile geom_text scale_fill_identity scale_x_continuous theme_void theme element_blank margin labs
#' @export
view_colors <- function(colors) {
  # Convert the named vector to a data frame
  color_df <- data.frame(
    Name = factor(names(colors), levels = rev(names(colors))),
    Color = colors,
    stringsAsFactors = FALSE
  )
  
  # Create the plot
  ggplot(color_df) +
    geom_tile(aes(y = Name, x = 2.05, fill = Color), width = 1.4, height = 0.8) +
    geom_text(aes(y = Name, x = 1.3, label = Name), hjust = 1) +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0), limits = c(1, 3)) +
    theme_void() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(20, 20, 20, 50) # Increase left margin to prevent chopping off
    ) +
    labs(title = "MDB Palette")
}

# Example usage (this line should not be included in the package script)
# view_colors(MDB_cols)
