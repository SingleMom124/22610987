name_plotter <- function(data, gender){

    suppressWarnings({
        suppressMessages({

    library(dplyr)
    library(ggplot2)
    library(glue)

    if (gender == "M") {
        text <- "Male"
    } else {
        text <- "Female"
    }

    national <- data %>%
        group_by(Year, Gender, Name) %>%
        summarize(Count = sum(Count)) %>%
        ungroup()

    total_count <- data %>%
        filter(Gender == gender) %>%
        group_by(Name) %>%
        summarise(TotalCount = sum(Count))

    top_names <- total_count %>%
        arrange(desc(TotalCount)) %>%
        head(5)

    ts_top_names <- national %>%
        filter(Gender == gender) %>%
        filter(Name %in% top_names$Name)

    plot <- ggplot(ts_top_names, aes(x = Year, y = Count, color = Name)) +
        geom_line(size = 1) +
        labs(title = glue("Top 5 Most {text} Popular Names Over Time"),
             x = "Year",
             y = "Count") +
        theme_minimal()

        })
    })

    return(plot)
}