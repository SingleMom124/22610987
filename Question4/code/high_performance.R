high_performance <- function(data){

    library(tidyverse)
    library(ggplot2)

    suppressWarnings({
        suppressMessages({

    # Working out the average number of medals won in a year and joining it to the main data...

    average_medals_per_year <- data %>%
        group_by(Year) %>%
        summarize(
            Average_Gold = mean(Gold),
            Average_Silver = mean(Silver),
            Average_Bronze = mean(Bronze),
            Average_Total = mean(Total))

    joined_data <- inner_join(data, average_medals_per_year, by = "Year")

    # Now filtering out the top 3 countries with the most total medals

    total_medals <- data %>%
        group_by(Country) %>%
        summarize(
            Total_Gold = sum(Gold),
            Total_Silver = sum(Silver),
            Total_Bronze = sum(Bronze),
            Total_Medals = sum(Total))
    total_medals <- na.omit(total_medals)
    top_countries <- total_medals %>%
        arrange(desc(Total_Medals)) %>%
        arrange(Total_Medals) %>%
        top_n(3)

    filtered_data <- joined_data %>%
        filter(Country %in% top_countries$Country)

    # Putting the data into long format

    countries_long <- filtered_data %>%
        select(Country, Gold, Silver, Bronze, Total) %>%
        pivot_longer(cols = c(Gold, Silver, Bronze, Total),
                     names_to = "Medal_Type",
                     values_to = "Count")

    averages_long <- filtered_data %>%
        select(Average_Gold, Average_Silver, Average_Bronze, Average_Total) %>%
        pivot_longer(cols = everything(),
                     names_to = "Medal_Type",
                     values_to = "Count")

    # Can now plot the top 3 highest performing countries against the overal average performance

    plot <- ggplot() +
        geom_bar(data = countries_long,
                 aes(x = Country, y = Count, fill = Medal_Type),
                 stat = "identity", position = position_dodge(width = 0.8),
                 width = 0.4) +
        geom_bar(data = averages_long,
                 aes(x = "Average", y = Count, fill = Medal_Type),
                 stat = "identity", position = position_dodge(width = 0.8),
                 width = 0.4) +
        labs(title = "Total Medals and Averages by Type (All Time)",
             x = "Country / Average",
             y = "Number of Medals",
             fill = "Medal Type") +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

        })
    })

    return(plot)

}