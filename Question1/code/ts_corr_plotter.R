ts_corr_plotter <- function(male_data, female_data) {

    suppressWarnings({
        suppressMessages({

    library(ggplot2)
    library(dplyr)
    library(ggpubr)

    # Plot for boys

    corr_plot_boys <- male_data %>%
        ggplot(aes(x = Year, y = Correlation)) +
        geom_line(size = 1, color = "black") +
        theme_bw() +
        labs(title = "Boy Names",
             x = "",
             y = "") +
        theme(plot.title = element_text(size = 8))

    # Plot for girls

    corr_plot_girls <- female_data %>%
        ggplot(aes(x = Year, y = Correlation)) +
        geom_line(size = 1, color = "black") +
        theme_bw() +
        labs(title = "Girl Names",
             x = "",
             y = "Spearman Correlation") +
        theme(plot.title = element_text(size = 8))

    # Calculate average correlation and create plot

    plot_data <- inner_join(male_data, female_data, by = "Year") %>%
        mutate(Mean_Corr = (Correlation.x + Correlation.y) / 2)

    corr_plot <- plot_data %>%
        ggplot(aes(x = Year, y = Mean_Corr)) +
        geom_line(size = 1, color = "black") +
        theme_bw() +
        labs(title = "Both",
             x = "",
             y = "") +
        theme(plot.title = element_text(size = 8))

    arranged_plots <- ggarrange(corr_plot_boys, corr_plot_girls, corr_plot,
                                ncol = 1, nrow = 3)

        })
    })

    return(arranged_plots)
}

