# -- Process MCP data...
library(dplyr)
library(lubridate)
mcp_points <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-points.csv')

mcp_metadata <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_MatchChartingProject/master/charting-m-matches.csv', 
                         quote = "", 
                         row.names = NULL, 
                         stringsAsFactors = FALSE)

mcp_metadata  <- mcp_metadata %>%
  mutate(Date = as.Date(as.character(Date), "%Y%m%d"))

# -- Consider Match Ids in 2019
match_ids_2019 <- mcp_metadata %>%
  filter(lubridate::year(Date) == 2019)

mcp_metadata %>%
  group_by(Charted.by) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))





# -- Does Federer actually have 
mcp_metadata %>%
  group_by(Player.1) %>%
  summarise(num_match = n()) %>%
  arrange(desc(num_match)) %>% View()

mcp_metadata %>%
  group_by(Player.2) %>%
  summarise(num_match = n()) %>%
  arrange(desc(num_match)) %>% View()


mcp_metadata %>%
  filter( (Player.1 == 'Roger Federer') | (Player.2 == 'Roger Federer')) %>% View()
