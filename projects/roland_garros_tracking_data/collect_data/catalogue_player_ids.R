# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
### Last update: March 21st, 2021
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

# ### --- ### --- ### --- ### --- ### 
### -- GOALS -----
# ### --- ### --- ### --- ### --- ### 
### * Create data frame of player names and associated player IDs
### * Add player handedness

# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 
# ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### --- ### 

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/collect_data/")

library(dplyr)

match_data_log <- read.csv("./data/matches_in_repo.csv")

match_data_log <- match_data_log %>%
  select(player1, player1_id, player2, player2_id) %>%
  mutate(player1 = gsub('\\s+', '', player1),
         player2 = gsub('\\s+', '', player2))

player1_df <- match_data_log[,c('player1', 'player1_id')]
colnames(player1_df) <- c('name', 'id')

player2_df <- match_data_log[,c('player2', 'player2_id')]
colnames(player2_df) <- c('name', 'id')

player_df <- rbind(player1_df, 
                   player2_df )

player_df <- player_df[!duplicated(player_df),]

# -- Some names still repeat
n_occur <- data.frame(table(player_df$id))
n_occur[n_occur$Freq > 1,]

player_df %>%
  filter(id %in% c('18274', '21623', '26485'))

unique(player_df$id)

names_to_remove <- c('A.SCHMIEDLOVA', 'PH.HERBERT', 'K.PLISKOVA')

player_df <- player_df %>%
  filter( ! (name %in% names_to_remove) ) %>%
  filter(!grepl('/', name))

# ### --- ### --- ### --- ### --- ### 
### -- ADD PLAYER HANDEDNESS -----
# ### --- ### --- ### --- ### --- ### 
atp_hand_df <- read.csv('./data/official_atp_handedness_2020.csv')
wta_hand_df <- read.csv('./data/official_wta_handedness_2020.csv')

# -- convert name to be entirely UPPERCASE
atp_hand_df$player_name <- toupper(atp_hand_df$player_name)
wta_hand_df$player_name <- toupper(wta_hand_df$player_name)

# -- get first letter of first name and entire last name
atp_first_letter <- substr(atp_hand_df$player_name, 1, 1)
atp_last_name <- stringr::str_extract(atp_hand_df$player_name, '[^ ]+$')

wta_first_letter <- substr(wta_hand_df$player_name, 1, 1)
wta_last_name <- stringr::str_extract(wta_hand_df$player_name, '[^ ]+$')

# -- Add modified name format to dataframe
atp_hand_df$new_name <- paste(atp_first_letter, '.', atp_last_name, sep = '')
wta_hand_df$new_name <- paste(wta_first_letter, '.', wta_last_name, sep = '')

# -- Fix some names manually (R.BAUTISTAAGUT; P.CARRENOBUSTA; J.DELPOTRO)
atp_hand_df[atp_hand_df$player_name == 'ROBERTO BAUTISTA AGUT','new_name'] <- 'R.BAUTISTAAGUT'
atp_hand_df[atp_hand_df$player_name == 'PABLO CARRENO BUSTA','new_name'] <- 'P.CARRENOBUSTA'
atp_hand_df[atp_hand_df$player_name == 'JUAN MARTIN DEL POTRO','new_name'] <- 'J.DELPOTRO'
atp_hand_df[atp_hand_df$player_name == 'DANIEL ELAHI GALAN','new_name'] <- 'DE.GALAN'
atp_hand_df[atp_hand_df$player_name == 'ROBERTO CARBALLES BAENA','new_name'] <- 'R.CARBALLESBAENA'


wta_hand_df[wta_hand_df$player_name == 'KAROLINA PLISKOVA','new_name'] <- 'Ka.PLISKOVA'
wta_hand_df[wta_hand_df$player_name == 'KRISTYNA PLISKOVA','new_name'] <- 'Kr.PLISKOVA'
wta_hand_df[wta_hand_df$player_name == 'ANNA KAROLINA SCHMIEDLOVA','new_name'] <- 'AK.SCHMIEDLOVA'
wta_hand_df[wta_hand_df$player_name == 'SARA SORRIBES TORMO','new_name'] <- 'S.SORRIBESTORMO'
wta_hand_df[wta_hand_df$player_name == 'PATRICIA MARIA TIG','new_name'] <- 'PM.TIG'




atp_and_wta_handedness <- rbind(atp_hand_df %>% select(new_name, player_handedness),
                                wta_hand_df %>% select(new_name, player_handedness))

# -- Add new columns to player_df
player_df <- player_df %>%
  left_join(atp_and_wta_handedness,
            by = c('name' = 'new_name'))
write.csv(player_df,
          'roland_garros_player_id.csv',
          row.names = FALSE)
