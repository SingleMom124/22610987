similar_gdp <- function(data, country){

    library(tidyverse)

    # Arranging data by GDP

    data <- data %>%
        arrange(`GDP per Capita`)

    # Extracting the GDP of the country of interest

    country_gdp <- data$`GDP per Capita`[data$Country == country]

    # Getting the counties that have a GDP 2 above and 2 below the country of interest

    index <- which(data$Country == country)

    similar_economies <- data[c(index - 1, index, index + 1), ]

    return(similar_economies)

}