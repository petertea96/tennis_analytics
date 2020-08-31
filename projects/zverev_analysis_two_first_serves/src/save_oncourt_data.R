# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
# Access and query the oncourt database (oncourt.mdb), and save relevant information in
# a .csv file
# This is a data cleaning script.
# ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
setwd('C:/Users/Peter/Documents/zverev_analysis')

#--> Load libraries
library(Hmisc)
library(RODBC)
library(dplyr)
library(lubridate)


#--> Connect to Oncourt database
password = "qKbE8lWacmYQsZ2"
con <- odbcConnectAccess("C:/Program Files (x86)/onCourt/OnCourt.mdb",
                         pwd = password)

### ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #
### Collect match data
# Variables of interest:
# * Player names in head-to-head match & serve match stats
### ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- ||||| ----- #

# --> Obtain Player level information including:
# * player_name [string]
# * player_id [numeric; key]
# * player_birth_date [date] 
atp_players <- sqlQuery(con, paste("select NAME_P as player_name, 
                                    ID_P as player_id,
                                    DATE_P as player_birth_date from players_atp"))



# --> Obtain match information including:
# * player1_id [numeric; key]
# * player2_id [numeric; key]
# * tournament_id [numeric; key]
# * match_result (score) [str]
# * match_date [date]
atp_matches <- sqlQuery(con, paste("select ID1_G as player1_id,
                                   ID2_G as player2_id, 
                                   ID_T_G as tournament_id, 
                                   RESULT_G as match_result,
                                   DATE_G as match_date,
                                   ID_R_G as match_round_id
                                   from games_atp"))


# --> Obtain richer match information including:
# * All serve and return stats
# * Ex: serve points played, 1st serves in, double faults, etc...
atp_stats <- sqlQuery(con, paste ("select * from stat_atp"))


#--> Obtain Tournament information:
# * tournament_id [numeric; key]
# * tournament_name [string]
# * tournament_date [date]
# * tournament_country [string]
# * tournament_level [numeric]; level of tournament (grand slam, challenger, etc)
atp_tournaments <- sqlQuery(con, paste ("select ID_T as tournament_id,
                                        NAME_T as tournament_name, 
                                        DATE_T as tournament_date,
                                        COUNTRY_T as tournament_country,
                                        RANK_T as tournament_level
                                        from tours_atp"))


#--> Obtain Player Rankings
atp_ratings <- sqlQuery(con, paste ("select DATE_R as rank_date,
                                    ID_P_R as player_id,
                                    POINT_R as ranking_pts,
                                    POS_R as ranking from ratings_atp"))

# Create Dataframe with end-of-year rankings
atp_ratings_end_of_year <- atp_ratings %>%
  mutate(year = year(rank_date)) %>%
  group_by(year) %>%
  filter(rank_date == max(rank_date))



## ***************************** ##
## COMBINE ALL DATASETS INTO ONE
## ***************************** ##

# FULL DATASET (all match data from 2003 - 2020)

full_data <- left_join(x = atp_matches, y = atp_players,
                 by = c("player1_id" = "player_id")) %>%
  rename(player1_name = player_name, player1_birth_date = player_birth_date) %>%
  #--> Join Match data and player 1 name 
  
  left_join(y = atp_players %>% select(player_id, player_name, player_birth_date),
            by = c("player2_id" = "player_id")) %>%
  rename(player2_name = player_name, player2_birth_date = player_birth_date) %>%
  #--> Add player 2 name
  
  filter(!grepl("/", player1_name))%>% 
  #--> Remove doubles matches... (keep only singles matches)
  
  #--> Remove retired matches
  filter(!grepl('ret',match_result))%>%
  
  #--> Add tournament info
  left_join(atp_tournaments,
            by = c("tournament_id" = "tournament_id"))  %>%
  
  #--> Add match stats
  left_join(atp_stats, 
            by = c("player1_id" = "ID1", "player2_id" = "ID2",
                   "tournament_id" = "ID_T", "match_round_id" = "ID_R")) %>%
  
  # --> Add rankings
  # first add year to make joining more sensible...
  mutate(year = year(match_date)) %>%
  # Add player1 rankings
  left_join(atp_ratings_end_of_year[c('player_id', 'ranking', 'year')],
            by = c('year' = 'year',"player1_id" = 'player_id' ) ) %>%
  rename(player1_ranking = ranking) %>%
  
  # Add player2 rankings
  left_join(atp_ratings_end_of_year[c('player_id', 'ranking', 'year')],
            by = c('year' = 'year',"player2_id" = 'player_id' ) ) %>%
  rename(player2_ranking = ranking)
  


# Make tournament level categories more readable 
full_data <- full_data %>%
  mutate(tournament_level = ifelse(tournament_level == 0, 'futures',
                                   ifelse(tournament_level == 1, 'challengers',
                                          ifelse(tournament_level == 2, 'main_tour',
                                                 ifelse(tournament_level == 3, 'masters',
                                                        ifelse(tournament_level == 4, 'grand_slam',
                                                               ifelse(tournament_level == 5, 'davis',
                                                                      ifelse(tournament_level == 6, 'non_atp', tournament_level))))))))

# Sanity Check
full_data %>%
  filter(player1_name == 'Alexander Zverev' | player2_name  == 'Alexander Zverev') %>%
  dim()

# Zverev player id is 24008
full_data %>%
  filter(player1_id == 24008 |player2_id == 24008 ) %>%
  dim()



# Okay, now get this data in terms of server/returner...
winner_frame <- data.frame(server =  full_data$player1_name,
                           returner = full_data$player2_name,
                           s_svpt =  full_data$FSOF_1,
                           s_1stIn =  full_data$FS_1,
                           s_1stWon = full_data$W1S_1,
                           s_2ndIn = full_data$W2SOF_1 - full_data$DF_1,
                           s_2ndWon =  full_data$W2S_1,
                           s_df = full_data$DF_1,
                           s_bpw =  full_data$BP_1,
                           s_bpo = full_data$BPOF_1,
                           s_rpw = full_data$RPW_1,
                           s_rpt = full_data$RPWOF_1,
                           bp_allowed = full_data$BP_2,
                           bpo_allowed = full_data$BPOF_2,                             
                           server_won = 1,
                           tournament_name = full_data$tournament_name,
                           tournament_date = full_data$tournament_date,
                           tournament_country = full_data$tournament_country,
                           tournament_level = full_data$tournament_level,
                           match_date = full_data$match_date,
                           match_result = full_data$match_result,
                           year = full_data$year,
                           server_rank = full_data$player1_ranking,
                           returner_rank = full_data$player2_ranking,
                           server_birthday = full_data$player1_birth_date,
                           returner_birthday = full_data$player2_birth_date
)

loser_frame <- data.frame(server =  full_data$player2_name,
                           returner = full_data$player1_name,
                           s_svpt =  full_data$FSOF_2,
                           s_1stIn =  full_data$FS_2,
                           s_1stWon = full_data$W1S_2,
                           s_2ndIn = full_data$W2SOF_2 - full_data$DF_2,
                           s_2ndWon =  full_data$W2S_2,
                           s_df = full_data$DF_2,
                           s_bpw =  full_data$BP_2,
                           s_bpo = full_data$BPOF_2,
                           s_rpw = full_data$RPW_2,
                           s_rpt = full_data$RPWOF_2,
                           bp_allowed = full_data$BP_1,
                           bpo_allowed = full_data$BPOF_1,                             
                           server_won = 0,
                           tournament_name = full_data$tournament_name,
                           tournament_date = full_data$tournament_date,
                           tournament_country = full_data$tournament_country,
                           tournament_level = full_data$tournament_level,
                           match_date = full_data$match_date,
                           match_result = full_data$match_result,
                           year = full_data$year,
                           server_rank = full_data$player2_ranking,
                           returner_rank = full_data$player1_ranking,
                           server_birthday = full_data$player2_birth_date,
                           returner_birthday = full_data$player1_birth_date
)                    
                           

atp_dat = rbind(winner_frame, loser_frame)  
sum(complete.cases(atp_dat %>% select(-server_rank, -returner_rank, -tournament_level)))                          
             



atp_dat_full <- atp_dat %>%
  
  filter(complete.cases(atp_dat %>% select(-server_rank, -returner_rank,
                                           -tournament_level, -server_birthday,
                                           -returner_birthday, -tournament_date,
                                           -tournament_country
                                           ))) %>%
  mutate(
    # --> Pr(1st serve in)
    pr_1stin = s_1stIn/s_svpt,
    
    # --> Pr(2nd serve in)
    pr_2ndin = s_2ndIn/(s_2ndIn + s_df),
    
    # --> Pr(Win service point | 1st serve in)
    pr_w1_giv_1in = s_1stWon/s_1stIn,
    
    # --> Pr(Win service point | 2nd serve in)
    pr_w2_giv_2in = s_2ndWon/s_2ndIn,
    
    # --> Pr(win on 1st serve)
    pr_win_on_1st_serve = pr_w1_giv_1in * pr_1stin,
    
    # --> Pr(win on 2nd serve)
    pr_win_on_2nd_serve = s_2ndWon/ (s_svpt - s_1stIn),
    
    #-->Pr(win on serve)
    pr_win_on_serve = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)*pr_2ndin*pr_w2_giv_2in) ,
    
    # --> Pr(win w/ 2 first serves)
    
    pr_win_two_first_serves = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)* pr_1stin*pr_w1_giv_1in )
  )



write.csv(x = atp_dat_full,
          file = "C:/Users/Peter/Documents/zverev_analysis/data/atp_fulldata.csv",
          row.names = FALSE)




# Note some names have '(number)' inside them
# some matches are not complete (i.e. 'ret')


