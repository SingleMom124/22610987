allocation_head <- function(data){

    suppressWarnings({
        suppressMessages({

    library(tidyverse)
    library(huxtable)

    most <- data %>%
        arrange(desc(Allocations_GDP_Percent)) %>%
        head(5)
    most <- data.frame(Countries = most$Country, Allocation = most$Allocations_GDP_Percent, Relation = c("Slavic", "Diplomatic", "Former USSR", "Slavic", "Slavic"))

    library(huxtable)

    suppressWarnings({
    most <- hux(
        "Countries" = most[,1],
        "Allocation" = most[,2],
        "Relation" = most[,3])
    })

    most_plot <- most %>%
        set_all_padding(4) %>%
        set_outer_padding(0) %>%
        set_number_format(2) %>%
        set_bold(row = 1, col = everywhere) %>%
        set_bottom_border(row = 1, col = everywhere) %>%
        set_width(0.5) %>%
        set_caption("Countries With Largest Relative Allocations")

    return(most_plot)

        })
    })

}