### EXTRA FUNCTIONS EVALUATING SPEED & PBP DATAFRAME

#Calculate proportion of success (wins) in 1st service games vs 2nd service games
serve_success_rates <- function(mydata){
  library(dplyr)
  total <- mydata %>%
    group_by(ServeNumber) %>%
    tally()
  
  wins <- mydata %>%
    group_by(ServeNumber) %>%
    summarise(num_wins =sum(PointWinner==PointServer))
  
  result <- total %>%
    left_join(wins, by = "ServeNumber") %>%
    mutate(win_percentage = num_wins / n)
  
  colnames(result) = c("Serve Number", "Service Games", "Service Wins", "Win Percentage")
  
  return(result)
}
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Test out function
# ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# serve_success_rates(zverev_match3_dat)

calc_serve_rates <- function(grand_slam_data){
  
  df = grand_slam_data %>%
    group_by(grouped_score, year) %>%
    summarize(pts_played = n(),
              pts_won = sum(won_point),
              win_perc = sum(won_point) / n(),
              df = sum(server_df),
              aces = sum(server_ace),
              df_perc_of_losses = sum(server_df) / (n() - sum(won_point))) %>%
    arrange(desc(df_perc_of_losses))
  
  return(df)
  
}

