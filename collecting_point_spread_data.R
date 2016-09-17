#### Downloading OLG Point Spreads ####
# Go to https://www.proline.ca/#pointspread and roughly copy all the games
# and paste them into a text file titled olg_point_spreads_YYYY-MM-DD.txt and
# save them in your working directory

#### Setting Working Directory ####
setwd('/Users/david.rubinger/Documents/point-spread-analysis')

#### Loading Packages ####
library(rvest)
library(XML)
library(googlesheets)
library(dplyr)

#### Defining Functions ####
# Make team names consistent between OLG and OddsShark
format_team_names <- function (x) {
    x <- gsub('new york-g', 'ny giants', x)
    x <- gsub('new york-j', 'ny jets', x)
    x
}

# Calculate mode, from # http://www.tutorialspoint.com/r/r_mean_median_mode.htm
Mode <- function (x) {  
    uniq <- unique(x)
    uniq[which.max(tabulate(match(x, uniq)))]
}

# Read OLG point spread data pasted in text file
clean_olg_data <- function (file_type, file_date = Sys.Date()) {
    
    # Loading packages
    suppressWarnings(library(tidyr))
    suppressWarnings(library(stringr))
    
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
        mutate_each(funs(as.numeric(
            gsub('\\(|\\)', '', str_extract(gsub('\\(P\\)', '(0)', .),
                                            '\\(.*\\)')))),
            fav_away_spread = away, fav_home_spread = home) %>%
        mutate(home_spread = ifelse(is.na(fav_home_spread),
                                    -fav_away_spread, fav_home_spread)) %>%
        mutate_each(
            funs(format_team_names(tolower(trimws(gsub('\\(.*\\)', '', .))))),
            away, home) %>%
        distinct(away, home) %>%  # selects first (latest) spread
        select(away, home, home_spread)
    
    # Outputting
    games
}

olg_games <- clean_olg_data(file_type = 'results', file_date = '2016-09-17')

#### Data Munging ####
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
games <- inner_join(os_games,
                    olg_games %>% rename(olg_home_spread = home_spread),
                    c('away', 'home')) %>%
    mutate(date = Sys.Date(),
           olg_os_gap = abs(olg_home_spread - os_home_spread),
           should_pick = ifelse(olg_home_spread < os_home_spread,
                                away, home)) %>%
    select(date, home, away, olg_home_spread, os_home_spread, olg_os_gap,
           should_pick)

# Outputting
print(games %>% arrange(desc(olg_os_gap)) %>% select(should_pick, olg_os_gap))

#### Reading Historical OddsShark Spreads ####
# Loading OddsShark game logs
google_sheets <- gs_key('1w-j9itmaUZQacCUmdirAFWZ2DpftqD40e0ECiGmxogY',
                        visibility = 'private')
os_team_dir <- gs_read(google_sheets, ws = 'os_team_dir')
olg_games <- clean_olg_data(file_type = 'results', file_date = '2016-09-17')

team_tabs <- NULL
for (i in min(os_team_dir$dir):max(os_team_dir$dir)) {
    print(paste('Loading', i - min(os_team_dir$dir) + 1, 'of',
                nrow(os_team_dir), 'team tables'))
    team <- os_team_dir$team[i - min(os_team_dir$dir) + 1]
    team_tab <- readHTMLTable(paste0(
        'http://www.oddsshark.com/stats/gamelog/football/nfl/', i),
        stringsAsFactors = FALSE)[[1]] %>%
        setNames(trimws(names(.))) %>%
        mutate(team = team)
    team_tabs <- rbind(team_tabs, team_tab)
}

# Cleaning
os_games <- team_tabs %>%
    mutate(date = as.Date(Date, format = "%b %d, %Y"),
           home = ifelse(grepl('@', Opponent), Opponent, team),
           away = ifelse(!grepl('@', Opponent), Opponent, team),
           team_spread = as.numeric(ifelse(Spread == 'Ev', 0, Spread)),
           home_spread = ifelse(team == home, team_spread, -team_spread)) %>%
    mutate_each(funs(tolower(trimws(gsub('@|^vs', '', .)))), home, away) %>%
    filter(Game == 'REG') %>%
    distinct(date, away, home) %>%
    select(date, away, home, home_spread)
    
# Joining
games <- inner_join(rename(olg_games, olg_home_spread = home_spread),
                    rename(os_games, os_home_spread = home_spread),
                    c('away', 'home'))

# Writing
games_collected <- gs_read(google_sheets, ws = 'spreads')
if (nrow(games_collected) != 0) {
    write_games <- games %>%
        anti_join(games_collected, c('away', 'home')) %>%
        select_(.dots = names(games_collected))
} else {
    write_games <- games %>%
        select_(.dots = names(games_collected))
}
if (nrow(write_games) > 0)
    gs_add_row(google_sheets, ws = 'spreads', input = write_games)
