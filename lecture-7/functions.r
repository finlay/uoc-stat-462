
plot_func <- function(data, label, title = "") {

  data$label <- factor(label)

  x_col <- colnames(data)[1]
  y_col <- colnames(data)[2]
  label <- colnames(data)[3]

  return(
    ggplot(data,
           aes(x = .data[[x_col]],
               y = .data[[y_col]],
               color = .data[[label]])
           ) +
    scale_color_manual(
      values = c("#00AFBB", "#FC4E07", "#e7b800",
                 "#006400", "#9400D3", "#FF8C00","#008080")) +
    geom_point(size = 3.0, alpha = 0.7) +
    labs(
      title = title,
      x = x_col,
      y = y_col,
      color = "Cluster Result"
    ) +
    theme_grey()
  )
}

