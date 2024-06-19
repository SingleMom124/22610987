heat_mapper <- function(data, title){

    library(tidyverse)
    library(ggplot2)

    band_subset <- data %>%
        select(name, duration_ms, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)

    band_cormat <- cor(data[, c("duration_ms", "danceability", "energy", "loudness", "speechiness",
                                "acousticness", "instrumentalness", "liveness", "valence", "tempo")])

    band_cormat <- as.data.frame(as.table(band_cormat))
    colnames(band_cormat) <- c("attribute1", "attribute2", "correlation")

    plot <- ggplot(band_cormat, aes(attribute1, attribute2, fill = correlation)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(correlation, 2)),
                  color = "black",
                  size = 3) +
        scale_fill_gradient2(low = "blue",
                             mid = "white",
                             high = "red",
                             midpoint = 0,
                             name = "Correlation",
                             limits = c(-1, 1),
                             breaks = seq(-1, 1, by = 0.2)) +
        labs(title = title,
             x = "",
             y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1),
              legend.position = "none",
              panel.border = element_rect(color = "black",
                                          fill = NA,
                                          size = 1))

    return(plot)

}