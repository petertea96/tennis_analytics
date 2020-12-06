# -- Run a mixed model
# -- Test subject: Novak Djokovic

# -- Load libraries
library(dplyr)
source("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/src/tidy_grand_slam_serve_pbp.R")


# -- Test subject: 2018 Rafael Nadal
rafa_nadal_2018_grand_slams <- collect_all_grand_slam_data(server_name = 'Rafael Nadal',
                                                           year = 2018,
                                                           tournament_vector = c('wimbledon', 'usopen'))

# -- I wonder if Rally Counts are predictive of winning point
# -- On 1st serve, longer rallies favour the returner (b/c of aces being rally length of 0)
# -- Servers have the advantage of finishing the point early on 1st serve
# -- 2nd serves are played with a different tactic (here, I think the traditional server
#    expects a longer rally -- so wins on 2nd serve is less associated with rally length)
# Look into: Balls in play (would need to know if point ended on an error?)
rafa_nadal_2018_grand_slams %>%
  group_by(won_point, ServeNumber, tournament) %>%
  summarise(avg_rally = mean(RallyCount)) %>%
  View()



# -- SErve location
# -- serving to the body prevents the returner from using a big swing return (more likley to block and float the ball)
# -- i.e. returner gets 'jammed'
# -- Hitting wide opens the court up

rafa_nadal_2018_grand_slams_mod_serve_location <- rafa_nadal_2018_grand_slams %>% 
  filter(!(is.null(ServeWidth)))%>%
  filter(!is.na(ServeWidth)) %>%
  filter(nchar(as.character(ServeWidth)) > 0) %>%
  mutate(serve_location = ifelse( grepl(pattern = 'C', x = as.character(ServeWidth)), 'T',
                                  ifelse(grepl(pattern = 'W', x = as.character(ServeWidth)), 'W',
                                         ifelse('B' == as.character(ServeWidth),'B', 'NA'
                                                ))),
         rally_index = ceiling(RallyCount/2)) 


table(rafa_nadal_2018_grand_slams_mod_serve_location$serve_location)

rafa_nadal_2018_grand_slams_mod_serve_location %>%
  group_by(serve_location, court_side, won_point) %>%
  summarise(sample_size = n()) %>%
  View()

# *************************************
# ** Fit Linear Mixed-Effects Models **
# *************************************

# -- lmer function
# -- response (y): won_point {0,1}
# -- predictors: 
# ServeNumber {1,2}, ServeSpeed, returner name, 
# serve_location {B, W, T}, court_side (Ad, Deuce), pressure_score


rafa_model <- glm(won_point ~ Speed_KMH + as.factor(ServeNumber) + returner + 
                    tournament + court_side + serve_location + pressure_score + court_side*serve_location +
                    rally_index:as.factor(ServeNumber),
                  data = rafa_nadal_2018_grand_slams_mod_serve_location,
                  family = 'binomial')





