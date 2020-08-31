library(dplyr)

# https://tt.tennis-warehouse.com/index.php?threads/most-comebacks-from-two-sets-down.567602/

atp_set_data_path <- "/Users/petertea/Documents/Github/match-analysis/historic_set_data.rds"


atp_set_data <- readRDS(atp_set_data_path)

View(atp_set_data)

complete_comebacks <- atp_set_data %>%
  filter(five_set_complete_comeback == 1)

View(complete_comebacks)

player1 <- complete_comebacks %>%
  group_by(player1) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
  

player2 <- complete_comebacks %>%
  group_by(player2) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

player2 %>%
  filter(player2 == 'Roger Federer') %>%
  View()
