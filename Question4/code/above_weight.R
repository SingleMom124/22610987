above_weight <- function(summer_gdp, summer_medals) {

    library(tidyverse)
    library(ggplot2)
    library(ggpubr)

    suppressWarnings({
        suppressMessages({

    # Want to calculate the average number of medals won per year, number of medals per country, ...
    # ... average number of athletes per country per year, and average number of athletes per year.

    # Total athletes per year

    total_athletes_per_year <- summer_gdp %>%
        group_by(Year) %>%
        summarize(Total_Athletes = n_distinct(Athlete))

    # Number of countries per year

    countries_per_year <- summer_gdp %>%
        group_by(Year) %>%
        summarize(Num_Countries = n_distinct(Country))

    # Average athletes per country per year

    average_athletes_per_country_per_year <- total_athletes_per_year %>%
        left_join(countries_per_year, by = "Year") %>%
        mutate(Average_Athletes = Total_Athletes / Num_Countries)

    # Number of athletes per country per year

    athletes_per_country_per_year <- summer_gdp %>%
        group_by(Country, Year) %>%
        summarize(Num_Athletes = n_distinct(Athlete))

    # Total medals won per year

    total_medals_per_year <- summer_medals %>%
        group_by(Year) %>%
        summarize(Total_Medals = sum(Total))

    # Join all data frames together

    data <- summer_medals %>%
        inner_join(athletes_per_country_per_year, by = c("Country", "Year")) %>%
        inner_join(average_athletes_per_country_per_year, by = "Year") %>%
        inner_join(total_medals_per_year, by = "Year") %>%
        mutate(Average_Medals = Total_Medals / Average_Athletes)

    # Now the purpose of this function is to sift out which countries punch above their weight...
    # Define this as winning more medals than the average number won ...
    # ... with less athletes than the average number of athletes competeing per year.

    filter_data <- data %>%
        filter(Average_Athletes > Num_Athletes & Total > Average_Medals)
    country_counts <- filter_data %>%
        group_by(Country) %>%
        summarize(Observations = n())

    # Picking the 3 countries with the most observations for better graphing...
    # It ends up being 4 here is some countries had an identical number of observation.
    # Its late...its good enough

    top_countries <- country_counts %>%
        top_n(3, Observations) %>%
        pull(Country)
    data <- filter(data, Country %in% top_countries)

    # Now plotting

    plot1 <- ggplot(data, aes(x = Year)) +
        geom_line(aes(y = Total, color = Country),
                  size = 1.2) +
        geom_line(aes(y = Average_Medals,
                      color = "Average Medals"),
                  linetype = "dashed",
                  size = 1.2) +
        labs(title = "Total Medals Won Per Year",
             x = "Year",
             y = "Number of Medals",
             color = "Legend") +
        scale_color_manual(values = c("France" = "red",
                                      "Finland" = "orange",
                                      "Hungary" = "blue",
                                      "Italy" = "yellow",
                                      "Average Medals" = "black")) +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
        theme(plot.title = element_text(size = 10))

    plot2 <- ggplot(data, aes(x = Year)) +
        geom_line(aes(y = Num_Athletes, color = Country),
                  size = 1.2) +
        geom_line(aes(y = Average_Athletes,
                      color = "Average Athletes"),
                  linetype = "dashed",
                  size = 1.2) +
        labs(title = "Number of Athletes per Year",
             x = "Year",
             y = "Number of Athletes",
             color = "Legend") +
        scale_color_manual(values = c("France" = "red",
                                      "Finland" = "orange",
                                      "Hungary" = "blue",
                                      "Italy" = "yellow",
                                      "Average Athletes" = "black")) +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    combined_plot <- ggarrange(plot1, plot2,
                               ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom") +
        theme(plot.title = element_text(size = 10))

        })
    })

    return(combined_plot)
}