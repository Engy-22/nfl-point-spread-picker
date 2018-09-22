# Setup
library(tidyverse)
library(stringr)

# Read OLG point spread data pasted in text file
clean_olg_data <- function (file_type, file_date = Sys.Date()) {
    
    # Make team names consistent between OLG and OddsShark
    format_team_names <- function (x) {
        x <- gsub('new york-g', 'ny giants', x)
        x <- gsub('new york-j', 'ny jets', x)
        x
    }
    
    # Reading
    filename <- paste0(
        'olg-point-spreads/', file_type, '-', file_date, '.txt')
    olg_text <- suppressWarnings(readLines(filename))
    
    # Cleaning
    games <- olg_text %>%
        data_frame() %>%
        filter(grepl('@', `.`)) %>%
        rename_(line = '.') %>%
        separate(line, c('away', 'home'), sep = '@') %>%
        mutate_at(
            vars(fav_away_spread = away, fav_home_spread = home),
            funs(
                as.numeric(
                    gsub(
                        '\\(|\\)',
                        '',
                        str_extract(
                            gsub('\\(P\\)', '(0)', .),
                            '\\(.*\\)'
                        )
                    )
                )
            )
        ) %>% 
        mutate(home_spread = ifelse(is.na(fav_home_spread),
                                    -fav_away_spread, fav_home_spread)) %>%
        mutate_at(
            vars(away, home),
            funs(format_team_names(tolower(trimws(gsub('\\(.*\\)', '', .)))))
        ) %>% 
        distinct(away, home, .keep_all = TRUE) %>%  # selects latest spread
        select(away, home, home_spread)
    
    # Outputting
    games
}
