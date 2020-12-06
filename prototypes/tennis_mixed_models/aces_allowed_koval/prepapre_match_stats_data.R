# Prepare match stats data.
setwd("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/aces_allowed_koval")

#--> Load libraries
library(dplyr)
library(lubridate)




# --- Save ATP DAta

saving_path <- "/Users/petertea/tennis_analytics/projects/serve_chronicles/data/atp_data/"

data_list <- list()
for(year in 1991:2019){
  file_name <- paste(saving_path, 'atp_', year, '.csv', sep = '')
  atp_df <- read.csv(file_name)
  
  data_list[[year-1990]] <- atp_df %>%
    select(server, server_id, returner, returner_id,
           s_svpt, s_ace, server_won, tournament_name, tournament_date,
           tournament_level, year, surface, score)
}

match_stats <- do.call(rbind, data_list)

saveRDS(match_stats,
        'all_match_stats.RDS')
# -- DONE
