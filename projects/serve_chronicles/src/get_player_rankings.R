# -- Scrape ATP and WTA ranking data
library(dplyr)
library(lubridate)

get_end_of_year_rankings <- function(root_path, wta_flag = FALSE){
  decade_list <- c('90s', '00s', '10s')
  
  datalist <- list()
  
  for(index in 1:length(decade_list)){
    decade <- decade_list[index]
    ranking_data_path <- paste(root_path, decade, '.csv',
                               sep = '')    
    if(wta_flag){
      ranking_data <- read.csv(ranking_data_path, header = FALSE)
      ranking_data <- ranking_data[, 1:4]
      colnames(ranking_data) <- c('ranking_date', 'rank', 'player', 'points')
      
    } else{
          
    ranking_data <- read.csv(ranking_data_path)
    
    }
    


    
    # Create Dataframe with end-of-year rankings
    ranking_end_of_year <- ranking_data %>%
      mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d"),
             year = year(ranking_date)) %>%
      group_by(year) %>%
      filter(ranking_date == max(ranking_date)) %>%
      filter(rank <= 500)
    
    datalist[[index]] <- ranking_end_of_year
    
  }
  
  entire_ranking_data <- do.call(rbind, datalist)
  
  return(entire_ranking_data)
  
}


# -- collect the data
atp_root_path <- 'https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_'

atp_rankings <- get_end_of_year_rankings(root_path = atp_root_path)

atp_filename <- "C:/Users/peter.tea/projects/tennis/data/processed/atp_rankings.csv"
write.csv(x = atp_rankings, row.names = FALSE,
          file = atp_filename)

# --

wta_root_path <- 'https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_rankings_'

wta_rankings <- get_end_of_year_rankings(root_path =wta_root_path,
                                         wta_flag = TRUE)

wta_filename <- "C:/Users/peter.tea/projects/tennis/data/processed/wta_rankings.csv"
write.csv(x = wta_rankings, row.names = FALSE,
          file = wta_filename)


  