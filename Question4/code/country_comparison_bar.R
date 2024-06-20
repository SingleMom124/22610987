
country_comparison_bar <- function(medal_data, gdp_data, country) {

    library(dplyr)
    library(tidyr)
    library(ggplot2)

    # Filter GDP data for comparison economies

    similar_economies <- similar_gdp(gdp_data, country)
    emerging_economies <- gdp_data %>%
        filter(Country %in% c("South Africa", "Brazil"))

    comparison_econ <- rbind(similar_economies, emerging_economies)

    # Filter medal data according to these comparison countries

    data <- medal_data %>%
        filter(Country %in% comparison_econ$Country)

    # Convert medal data to long format

    data_long <- data %>%
        pivot_longer(cols = c(Gold, Silver, Bronze, Total),
                     names_to = "Medal_Type",
                     values_to = "Count")

    # Plotting the long format medal data

    plot <- ggplot(data_long, aes(x = Country, y = Count, fill = Medal_Type)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) +
        labs(title = "Total Medal Counts by Country",
             x = "Country",
             y = "Medal Count",
             fill = "Medal Type") +
        scale_fill_manual(values = c("Gold" = "gold", "Silver" = "grey", "Bronze" = "brown", "Total" = "navy")) +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    return(plot)
}