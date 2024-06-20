team_adjuster <- function(data, team_sports) {

    library(tidyverse)

    suppressWarnings({
        suppressMessages({

    # Sorted the through the Event names names to create a vector of team sports

    team_sports <- c("Artistic G.", "Water polo", "Cricket", "Football", "Rowing",
                     "Rugby", "Tug of War", "Lacrosse", "Hockey", "Polo", "Ice Hockey",
                     "Basketball", "Volleyball", "Synchronized S.", "Baseball", "Softball",
                     "Beach volley.", "Synchronized Swimming", "Water Polo", "Beach Volleyball",
                     "Four", "Pair", "Relay", "Man", "Curling", "Cross Country Skiing",
                     "Bobsleigh", "Nordic Combined")

    # Constructing a regular expression pattern from team_sports vector

    team_pattern <- paste(team_sports, collapse = "|")
    team_pattern <- paste0("\\b(", team_pattern, ")\\b")

    # If an observation has a sport listed in the "team_sport" vector provided or the "Discipline" contains the word "Team", create a "Team_Sport" dummy...
    # ... replacing the athlete name with "Athlete" or "Team" based on this dummy...
    # ... and using the unique function to condense duplicates for a specific discipline into one observation.

    events <- data %>%
        mutate(Team_Sport = ifelse(grepl(team_pattern, Event, ignore.case = TRUE), 1, 0)) %>%
        mutate(Athlete = ifelse(Team_Sport == 1, "Team", "Athlete")) %>%
        distinct(Country, Year, Discipline, Medal, Team_Sport, .keep_all = TRUE)

    # Getting the number of medals per type won by each country per year


    medals_count <- events %>%
        group_by(Country, Year) %>%
        summarize(
            Gold = sum(Medal == "Gold", na.rm = TRUE),
            Silver = sum(Medal == "Silver", na.rm = TRUE),
            Bronze = sum(Medal == "Bronze", na.rm = TRUE),
            Total = n()) %>%
        ungroup()

        })
    })

    return(medals_count)
}

