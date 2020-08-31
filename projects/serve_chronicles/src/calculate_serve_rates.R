# For each match, calculate rate of double faults per second serve attempt and aces
# Then, for each year calculate a player's average rate of double faults per match

# ***********************
# To DO: Add player rankings as column
# ***********************
library(dplyr)

calculate_rates <- function(tennis_match_data,year){
  # Calculate double fault rates (per 2nd serve attempts)
  # Calculate Ace rates (per total serve attempts)
  calculated_data <- tennis_match_data %>%
    group_by(server, server_id) %>%
    summarise(matches_played = n(),
              avg_df_rate = mean(df_rate),
              avg_ace_rate = mean(ace_rate)) %>%
    mutate(year = year) %>%
    
    arrange(desc(matches_played))
    
  return(calculated_data)
}



wta_data_path <- "C:/Users/peter.tea/projects/tennis/data/wta_data/"

year_list <- 2003:2019

datalist = list()

for(index in 1:length(year_list)){
  
  year = year_list[index]
  data_name <- paste(wta_data_path, 'wta_', year, '.csv', sep = '')
  
  wta_data <- read.csv(data_name)
  
  ace_df_ratio <- wta_data %>%
    group_by(server, server_id, year) %>%
    summarise(ace_df_ratio = sum(s_ace) / sum(s_df))
    
  
  wta_data_rates <- wta_data %>%
    filter((s_svpt) > 0 & (s_2ndIn > 0) ) %>%
    mutate(df_rate = s_df / (s_2ndIn + s_df),
           ace_rate = s_ace/s_svpt)
  

  
  rate_data <- calculate_rates(tennis_match_data = wta_data_rates, 
                                         year = year) %>%
    left_join(ace_df_ratio, by = c('server' = 'server',
                                   'server_id' = 'server_id',
                                   'year' = 'year'))
  
  datalist[[index]] <- rate_data
  
}


wta_serve_rates_data <- do.call(rbind, datalist)


#View(wta_serve_rates_data)

# -- add rankings data
wta_rank_filename <- "C:/Users/peter.tea/projects/tennis/data/processed/wta_rankings.csv"
wta_rankings <- read.csv(wta_rank_filename)

wta_serve_rates_data <- wta_serve_rates_data %>%
  left_join(wta_rankings[c('player', 'rank', 'year')],
            by = c('year' = 'year',"server_id" = 'player' ) ) 



wta_serve_rates_data %>% distinct() %>% 
  filter(server == 'Maria Sharapova') %>%
  View()

wta_serve_rates_data <- wta_serve_rates_data %>%
  distinct() 

write.csv(wta_serve_rates_data, 
          file = "C:/Users/peter.tea/projects/tennis/data/processed/wta_serve_rates.csv",
          row.names = FALSE)

wta_serve_rates_data %>%
  filter(rank <= 100) %>%
  filter(matches_played >= 20) %>%
  filter(avg_df_rate > 0.22) %>%
  arrange(desc(avg_df_rate)) %>%
  View()


wta_serve_rates_data %>%
  filter(rank <= 100) %>%
  filter(matches_played >= 20) %>%
  filter(avg_ace_rate > 0.1) %>%
  arrange(desc(avg_ace_rate)) %>%
  View()


wta_serve_rates_data %>%
  filter(rank <= 100) %>%
  filter(matches_played >= 20) %>%
  filter(ace_df_ratio > 1) %>%
  arrange(desc(ace_df_ratio)) %>%
  View()
# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # 
# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
# -- Collect ATP double fault rates

atp_data_path <- "C:/Users/peter.tea/projects/tennis/data/atp_data/"
list.files(atp_data_path)

atp_year_list <- 1991:2019

atp_datalist = list()

for(index in 1:length(atp_year_list)){
  
  year = atp_year_list[index]
  data_name <- paste(atp_data_path, 'atp_', year, '.csv', sep = '')
  
  atp_data <- read.csv(data_name)
  
  ace_df_ratio <- atp_data %>%
    group_by(server, server_id, year) %>%
    summarise(ace_df_ratio = sum(s_ace) / sum(s_df))
  
  atp_data_rates <- atp_data %>%
    filter((s_svpt) > 0 & (s_2ndIn > 0) ) %>%
    mutate(df_rate = s_df / (s_2ndIn + s_df),
           ace_rate = s_ace/s_svpt,
           ace_df_ratio = s_ace/s_df)
  
  rate_data <- calculate_rates(tennis_match_data = atp_data_rates, 
                               year = year) %>%
    
    left_join(ace_df_ratio, by = c('server' = 'server',
                                   'server_id' = 'server_id',
                                   'year' = 'year'))
  
  
  atp_datalist[[index]] <- rate_data
  
}


atp_serve_rates_data <- do.call(rbind, atp_datalist)


# -- add rankings data
atp_rank_filename <- "C:/Users/peter.tea/projects/tennis/data/processed/atp_rankings.csv"
atp_rankings <- read.csv(atp_rank_filename)

atp_serve_rates_data <- atp_serve_rates_data %>%
  left_join(atp_rankings[c('player', 'rank', 'year')],
            by = c('year' = 'year',"server_id" = 'player' ) ) 


atp_serve_rates_data %>%
  filter(rank <= 50) %>%
  filter(matches_played >= 25) %>%
  filter(avg_df_rate > 0.1) %>%
  arrange(desc(avg_df_rate)) %>%
  View()

atp_serve_rates_data %>%
  filter(rank <= 50) %>%
  filter(matches_played >= 25) %>%
  filter(avg_ace_rate > 0.1) %>%
  arrange(desc(avg_ace_rate)) %>%
  View()

atp_serve_rates_data %>%
  filter(rank <= 50) %>%
  filter(matches_played >= 20) %>%
  filter(ace_df_ratio > 1) %>%
  arrange(desc(ace_df_ratio)) %>%
  View()

atp_serve_rates_data <- atp_serve_rates_data %>%
  distinct() 

write.csv(x = atp_serve_rates_data, 
          file = "C:/Users/peter.tea/projects/tennis/data/processed/atp_serve_rates.csv",
          row.names = FALSE)






