names_reg <- function(data, credits, movies, gender) {

    suppressWarnings({
        suppressMessages({

    library(dplyr)
    library(plm)
    library(huxtable)
    library(lmtest)

    # Summarize national data

    national <- baby_names %>%
        group_by(Year, Gender, Name) %>%
        summarize(Count = sum(Count)) %>%
        ungroup()

    # Calculate total count by name

    total_count <- national %>%
        filter(Gender == gender) %>%
        group_by(Name) %>%
        summarise(TotalCount = sum(Count))

    # Identify top names

    top_names <- total_count %>%
        arrange(desc(TotalCount)) %>%
        head(5)

    media <- inner_join(credits, movies, by = "id")

    suppressWarnings({
        media$ActorName <- sapply(strsplit(media$name, " "), `[`, 1)
        media$CharacterName <- sapply(strsplit(media$character, " "), `[`, 1)
    })

    media <- media %>%
        mutate(Top_5_actor = ifelse(ActorName %in% top_names$Name, 1, 0))

    colnames(media)[9] <- "Year"
    colnames(media)[20] <- "Name"

    reg_data <- inner_join(media, national, by = c("Year", "Name")) %>%
        filter(Gender == gender)

    model1 <- plm(Count ~ tmdb_popularity + imdb_score + Top_5_actor,
                  data = reg_data,
                  index = c("id"),
                  model = "pooling")
    model2 <- plm(Count ~ tmdb_popularity + imdb_score + Top_5_actor,
                  data = reg_data,
                  index = c("id"),
                  model = "random")
    model3 <- plm(Count ~ tmdb_popularity + imdb_score + Top_5_actor,
                  data = reg_data,
                  index = c("id"),
                  model = "within")

    regs <- huxreg(POLS = model1, RE = model2, FE = model3)

        })
    })

    return(regs)

}