allocation_proportions <- function(data){

    library(tidyverse)
    library(ggplot2)

    actions <- data %>%
        filter(Commitments_GDP_Percent <= Allocations_GDP_Percent)

    commited <- data.frame(
        Category = c("Followed Through", "Did Not Allocate What Was Commited"),
        Percentage = c((nrow(actions) / nrow(data)) * 100,
                       (((nrow(data) - nrow(actions))) / nrow(data)) * 100))

    plot <- ggplot(commited, aes(x = Category,
                         y = Percentage)) +
        geom_bar(stat = "identity",
                 width = 0.5,
                 fill = c("navy", "orange"),
                 color = "black") +
        labs(title = "Percentage of EU Members That Met Their Commitment",
             x = "",
             y = "Percentage") +
        theme_minimal() +
        geom_text(aes(label = sprintf("%.1f%%", Percentage)),
                  vjust = -0.5,
                  size = 3,
                  color = "black")+
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    return(plot)

}