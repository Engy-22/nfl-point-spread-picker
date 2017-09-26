#### Setup ####
library(googlesheets)
library(XML)
source("clean_olg_data.R")

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
    distinct(date, away, home, home_spread) %>%
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
