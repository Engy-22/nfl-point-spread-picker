#### Downloading OLG Point Spreads ####
# Go to https://www.proline.ca/#pointspread and roughly copy all the games
# and paste them into a text file titled olg_point_spreads_YYYY-MM-DD.txt and
# save them in your working directory

#### Loading Packages ####
library(rvest)
library(XML)
library(googlesheets)
source("clean_olg_data.R")

#### Defining Functions ####
# Calculate mode, from # http://www.tutorialspoint.com/r/r_mean_median_mode.htm
Mode <- function (x) {  
    uniq <- unique(x)
    uniq[which.max(tabulate(match(x, uniq)))]
}

#### Load Data ####
olg_games <- clean_olg_data(file_type = 'odds', file_date = "2016-09-17")

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
