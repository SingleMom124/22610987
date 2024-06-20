double_bar_plotter <- function(data){

    suppressWarnings({
        suppressMessages({

    library(tidyverse)
    library(ggplot2)

    # First need to convert the data to long format...

    data_long <- data %>%
        pivot_longer(cols = c(Allocations_GDP_Percent, Commitments_GDP_Percent),
                     names_to = "Compare",
                     values_to = "Percent")

    # ... so that it can be plotted as a double bar graph...

    plot <- ggplot(data_long, aes(x = Country,
                          y = Percent,
                          fill = Compare,
                          color = Compare)) +
        geom_bar(stat = "identity",
                 position = "dodge",
                 width = 1,
                 color = "black") +
        labs(title = "Allocations vs Commitments by Country",
             x = "Country",
             y = "Percent") +
        scale_fill_manual(values = c("Allocations_GDP_Percent" = "navy",
                                     "Commitments_GDP_Percent" = "orange"),
                          name = "Category") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              legend.title = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, size = 1))

        })
    })

    return(plot)

}