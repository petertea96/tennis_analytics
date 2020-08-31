serve_success_rates <- function(dataframe){
  dataframe %>%
    group_by(ServeNumber,year) %>%
    summarise(points_played = n(),
              aces = sum(server_ace),
              dfs = sum(server_df),
              win_perc = sum(won_point)/ n())
  
  
}