all_time_best <- function(data){

    library(tidyverse)
    library(ggplot2)

    suppressWarnings({
        suppressMessages({

    # Filtering out the athletics discipline

    athletics <- olympics_gdp %>%
        filter(Discipline == "Athletics")

    # Summarize the medal counts for all athletes

    all_athletes <- athletics %>%
        group_by(Athlete, Country) %>%
        summarize(Gold = sum(Medal == "Gold"),
                  Silver = sum(Medal == "Silver"),
                  Bronze = sum(Medal == "Bronze"),
                  Total = n()) %>%
        arrange(desc(Total))

    # Summarize the number of athletes by country

    all_countries <- all_athletes %>%
        group_by(Country) %>%
        summarize(Total_Athletes = n()) %>%
        arrange(desc(Total_Athletes))

    # Plot the number of athletes by country

    plot <- ggplot(all_countries, aes(x = reorder(Country, -Total_Athletes),
                                      y = Total_Athletes,
                                      fill = Country)) +
        geom_bar(stat = "identity") +
        labs(title = "Countries of All Athletes in Athletics (All Time)",
             x = "Country",
             y = "Number of Athletes") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              legend.position = "none") +
        scale_fill_viridis_d()

        })
    })

    return(plot)

}