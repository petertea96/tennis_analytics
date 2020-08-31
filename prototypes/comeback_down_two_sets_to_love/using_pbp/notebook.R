# -- How often do players bounce back when trailing 2 sets to love?
source("~/Documents/GitHub/match-analysis/src/collect_set_data.R")

atp_grand_sets <- get_all_atp_set_data(years = 2011:2019)

atp_complete <- atp_grand_sets %>%
  filter(complete_data==1)
View(atp_complete)


atp_grand_sets %>%
  group_by(year) %>%
  summarise(matches = n())

atp_grand_sets %>%
  filter(year ==2019) %>%
  View()
