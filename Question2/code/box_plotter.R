box_plotter <- function(data, colname, title) {

    library(ggplot2)

    plot <- ggplot(data, aes(x = album,
                             y = .data[[colname]],
                             fill = album)) +
        geom_boxplot() +
        labs(title = title,
             y = colname) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              panel.border = element_rect(color = "black", fill = NA, size = 1))

    return(plot)
}