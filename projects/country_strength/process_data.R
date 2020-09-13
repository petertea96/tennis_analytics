### Purpose: Process data for grand slam winners wince open era (1968)
# Date: September 12th, 2020

# -- Set working directory
setwd("/Users/petertea/tennis_analytics/projects/country_strength/")
# -- Load libraries
library(dplyr)

# -- Read in raw data
espn_grand_slam_winners_atp <- read.csv('./raw_espn_data/espn_atp_grand_slam_winners.csv',
                                        stringsAsFactors = FALSE)
#View(espn_grand_slam_winners_atp)

colnames(espn_grand_slam_winners_atp) <- c('year', 'tournament', 'winner', 'loser')

atp_players <- read.csv("./player_metadata/atp_players.csv",
                        header = FALSE,
                        # read only columns 2,3,4 and 6
                        colClasses = c('NULL', rep('character', 3), 'NULL', 'character'))

colnames(atp_players) <- c('first_name', 'last_name', 'handedness', 'country')


# -- Get to work and do some joins
# Add full name
atp_players <- atp_players %>%
  mutate(full_name = paste(first_name, last_name, sep = ' '))

#head(atp_players)

# Note: Manually changed names in espn data
#  -- Juan Martin Del Potro
# -- John McEnroe
# -- Andrei Medvedev

# Added
# -- Harold Soloman USA (1976)

# Note: Manually changed countries
# -- Ivan Lendl CZE
# -- Manuel Orantes ESP

atp_open_era_grand_slam_winners <- espn_grand_slam_winners_atp %>%
  left_join(atp_players %>% select(full_name, country), by = c('winner' = 'full_name')) %>%
  rename(winner_country = country) %>%
  left_join(atp_players %>% select(full_name, country), by = c('loser' = 'full_name')) %>%
  rename(loser_country = country)
  

# Manual additions
# -- Thomas Johannson SWE
atp_open_era_grand_slam_winners[atp_open_era_grand_slam_winners$winner == 'Thomas Johannson',]$winner_country <- 'SWE'

#table(atp_open_era_grand_slam_winners$country)

# -- Save ATP data
write.csv(atp_open_era_grand_slam_winners, 'atp_open_era_grand_slam_winners.csv',
          row.names = FALSE)



# --------------------------------------
#  -- Rinse and Repeat for the WTA #####
# --------------------------------------

# -- Read in raw data
espn_grand_slam_winners_wta <- read.csv('./raw_espn_data/espn_wta_grand_slam_winners.csv',
                                        stringsAsFactors = FALSE)
#View(espn_grand_slam_winners_wta)

colnames(espn_grand_slam_winners_wta) <- c('year', 'tournament', 'winner', 'loser')

wta_players <- read.csv("./player_metadata/wta_players.csv",
                        header = FALSE,
                        # read only columns 2,3,4 and 6
                        colClasses = c('NULL', rep('character', 3), 'NULL', 'character'))

colnames(wta_players) <- c('first_name', 'last_name', 'handedness', 'country')

# -- Get to work and do some joins
# Add full name
wta_players <- wta_players %>%
  mutate(full_name = paste(first_name, last_name, sep = ' '))

#head(wta_players)

# Manual changes to the data:

# -- Li Na coded backwards in wta_players
# Added Natasha Zvereva (BLR)
# Justine Henin-Hardenne changed to just Henin
# removed dash from "Arantxa Sanchez-Vicario"
# Mary Jo Fernandez --> Mary Joe Fernandez
# Zena Garrison --> Zina Garrison
# Chris Evert-Lloyd --> Chris Evert
# Changed Navratilova USE --> CZE
# Margaret Smith-Court --> Court
# -- 


wta_open_era_grand_slam_winners <- espn_grand_slam_winners_wta %>%
  left_join(wta_players %>% select(full_name, country), by = c('winner' = 'full_name')) %>%
  rename(winner_country = country) %>%
  left_join(wta_players %>% select(full_name, country), by = c('loser' = 'full_name')) %>%
  rename(loser_country = country)

#table(wta_open_era_grand_slam_winners$winner_country)
# -- Save ATP data
write.csv(wta_open_era_grand_slam_winners, 'wta_open_era_grand_slam_winners.csv',
          row.names = FALSE)
