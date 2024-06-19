

band_attributes <- function(data1, data2, data3, band_name1, band_name2, band_name3) {

    library(ggplot2)
    library(ggpubr)
    library(tidyverse)

    # Subset data for each band and combine...

    band1_subset <- data1 %>%
        select(release_date, tempo, energy, valence, danceability, duration_ms) %>%
        mutate(band = band_name1)
    band2_subset <- data2 %>%
        select(release_date, tempo, energy, valence, danceability, duration_ms) %>%
        mutate(band = band_name2)
    band3_subset <- data3 %>%
        select(release_date, tempo, energy, valence, danceability, duration_ms) %>%
        mutate(band = band_name3)

    combined_data <- bind_rows(band1_subset, band2_subset, band3_subset)

    # Convert release date to release year...

    combined_data$release_date <- as.Date(combined_data$release_date)
    combined_data$release_year <- year(combined_data$release_date)

    # Calculate average attributes by band and album...

    attributes <- combined_data %>%
        group_by(band, release_year) %>%
        summarise(avg_tempo = mean(tempo, na.rm = TRUE),
                  avg_energy = mean(energy, na.rm = TRUE),
                  avg_valence = mean(valence, na.rm = TRUE),
                  avg_danceability = mean(danceability, na.rm = TRUE),
                  avg_duration_ms = mean(duration_ms, na.rm = TRUE)) %>%
        ungroup()

    # Plot average attributes over time for both bands...

    plot1 <- ggplot(attributes, aes(x = release_year, y = avg_tempo, color = band)) +
        geom_line(size = 1) +
        labs(title = "",
             x = "", y = "Tempo",
             color = "Band") +
        theme_minimal() +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    plot2 <- ggplot(attributes, aes(x = release_year, y = avg_energy, color = band)) +
        geom_line(size = 1) +
        labs(title = "",
             x = "", y = "Energy",
             color = "Band") +
        theme_minimal()+
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    plot3 <- ggplot(attributes, aes(x = release_year, y = avg_valence, color = band)) +
        geom_line(size = 1) +
        labs(title = "",
             x = "", y = "Valence",
             color = "Band") +
        theme_minimal()+
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    plot4 <- ggplot(attributes, aes(x = release_year, y = avg_danceability, color = band)) +
        geom_line(size = 1) +
        labs(title = "",
             x = "", y = "Danceability",
             color = "Band") +
        theme_minimal()+
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    plot5 <- ggplot(attributes, aes(x = release_year, y = avg_duration_ms, color = band)) +
        geom_line(size = 1) +
        labs(title = "",
             x = "", y = "Duration (ms)",
             color = "Band") +
        theme_minimal()+
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

    combined_plot <- ggarrange(plot1, plot2, plot3, plot4, plot5,
                               ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")

    combined_plot <- annotate_figure(combined_plot,
                                      top = text_grob("Musical Progression of Metallica and Coldplay",
                                                      face = "bold", size = 10))

    return(combined_plot)

}

