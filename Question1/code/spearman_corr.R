spearman_corr <- function(data, gender){

    library(tidyverse)

    suppressWarnings({
        suppressMessages({

    data <- data %>%
        group_by(Year, Gender, Name) %>%
        summarize(Count = sum(Count)) %>%
        ungroup()

    # First starting this function off by writing some other functions:

    # A function to Identify the 25 most popular names...

    popular_names <- function(data, year, gender) {

        data %>%
            filter(Year == year & Gender == gender) %>%
            arrange(desc(Count)) %>%
            head(25) %>%
            mutate(Rank = row_number())
    }

    # ... and a function to calculate the Spearman correlation

    spearman_cor <- function(current, future) {

        all_names <- unique(c(current$Name, future$Name)) # Make sure all names are unique so vectors are of equal length

        current_ranks <- match(current$Name, all_names)
        future_ranks <- match(future$Name, all_names)

        cor.test(current_ranks, future_ranks, method = "spearman")

    }

    # Now looping these 2 functions across all years...

    plot_data <- data.frame()

    for (year in unique(data$Year)) {
        if (year < max(unique(data$Year)) - 2) {  # Make sure there are observations for current year and the future year

            current <- popular_names(data, year, gender)
            future <- popular_names(data, year + 3, gender)

            correlation <- spearman_cor(current, future)

            results <- data.frame(Year = year, Gender = gender, Correlation = correlation$estimate)
            plot_data <- rbind(plot_data, results)

        }}

        })
    })

    return(plot_data)

}