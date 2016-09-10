#### Downloading OLG Point Spreads ####
# Go to https://www.proline.ca/#pointspread and roughly copy all the games
# and paste them into a text file titled olg_point_spreads_YYYY-MM-DD.txt and
# save them in your working directory

#### Setting Working Directory ####
setwd('/Users/david.rubinger/Documents/point_spread_analysis')

#### Loading Packages ####
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(googlesheets)

#### Defining Functions ####
# Make team names consistent between OLG and OddsShark
ReformatNames <- function (x) {
    x <- gsub('new york-g', 'ny giants', x)
    x <- gsub('new york-j', 'ny jets', x)
    x
}

# Calculate mode, from # http://www.tutorialspoint.com/r/r_mean_median_mode.htm
Mode <- function (x) {  
    uniq <- unique(x)
    uniq[which.max(tabulate(match(x, uniq)))]
}

#### Data Munging ####
# OLG point spread data
filename <- paste0('olg_point_spreads_', Sys.Date(), '.txt')
olg_page <- readLines(filename)
olg_games <- olg_page %>%
    data.frame() %>%
    setNames('game') %>%
    filter(grepl('[[:alpha:]]@', olg_page)) %>%
    separate(game, c('away', 'home'), sep = '@') %>%
    mutate(fav_away_spread = str_extract(away, '\\(.*\\)'),
           fav_home_spread = str_extract(home, '\\(.*\\)')) %>%
    mutate_each(funs(as.numeric(gsub('\\(|\\)| ', '', .))),
                fav_away_spread, fav_home_spread) %>%
    mutate_each(funs(tolower(trimws(gsub('\\(.*\\)', '', .)))), away, home) %>%
    mutate_each(funs(ReformatNames), away, home) %>%
    mutate(olg_home_spread = ifelse(
        is.na(fav_home_spread), -fav_away_spread, fav_home_spread)) %>%
    select(-fav_away_spread, -fav_home_spread)

# OddsShark point spread data
os_page <- read_html('http://www.oddsshark.com/nfl/odds')
os_teams <- data.frame(
    away = os_page %>% html_nodes('.op-team-top') %>% html_text(),
    home = os_page %>% html_nodes('.op-team-bottom') %>% html_text(),
    stringsAsFactors = FALSE)
os_spread <- os_page %>%
    html_nodes('.border-bottom') %>%
    html_text() %>%
    matrix(ncol = 36, byrow = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    select(seq(3, ncol(.), by = 2)) %>%
    mutate_each(funs(as.numeric(gsub('Ev|\\+', '0', .)))) %>%
    transmute(os_home_spread = -apply(., 1, Mode))
os_games <- os_teams %>%
    bind_cols(os_spread) %>%
    filter(!is.na(os_home_spread)) %>%
    mutate_each(funs(tolower), home, away)

# Joining the two
games <- inner_join(os_games, olg_games, c('away', 'home')) %>%
    mutate(date = Sys.Date()) %>%
    select(date, home, away, olg_home_spread, os_home_spread)

# Outputting
google_sheets <- gs_key('1w-j9itmaUZQacCUmdirAFWZ2DpftqD40e0ECiGmxogY',
                        visibility = 'private')
gs_add_row(google_sheets, ws = 'spreads', input = games)
