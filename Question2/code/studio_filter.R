studio_filter <- function(data){

    suppressWarnings({
        suppressMessages({

    library(tidyverse)

    # In when the Cold play data is inputed, some columns need to be adapted to match Metallica such as the duration column being in a different metric...

    if ("duration" %in% colnames(data)) {

        data <- data %>%
            mutate(duration_ms = duration * 1000)

    } else {

        message("No mutate needed")

    }

    # ... and the album name column has a different name.


    if ("album_name" %in% colnames(data)) {

        data <- data %>%
            rename(album = album_name)

        } else {

            message("No rename needed")

        }

    # Now filtering out non-studio recordings...

    band_alt_album <- data.frame(alts = data$album[grepl(pattern = "Live|Demo|Edition|Deluxe|Remastered|Motion", data$album)])
    band_alt_song <- data.frame(alts = data$name[grepl(pattern = "Live|Demo|Progress|Deluxe|Remastered|Edit", data$name)])

    band_studio <- data %>%
        filter(!album %in% band_alt_album$alts) %>%
        filter(!name %in% band_alt_song$alts)

        })
    })

    return(band_studio)

}



