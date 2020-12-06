### ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- ||
### Learning about Mixed models using lme4 and stan #####
### Material from Steph's github
### ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- ||

# Observations within players are correlated

library(dplyr)
library(ggplot2)
library(lme4)

# Data: ATP 2017 Grand Slam Matches #####

atp_data_2017_link <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2017.csv"

atp_data_2017 <- read.csv(atp_data_2017_link)

atp_data_2017_gs <- atp_data_2017 %>%
  filter(tourney_level=='G') %>%
  select(tourney_name, surface, winner_name, 
         w_svpt, w_1stWon, w_2ndWon,
         loser_name, l_svpt, l_1stWon, l_2ndWon) %>%
  dplyr::mutate(
    w_servewon = w_1stWon + w_2ndWon,
    l_servewon = l_1stWon + l_2ndWon
  )


winner <- loser <- atp_data_2017_gs 

# -- Change column names to be in terms of player and opponent
names(winner) <- sub("winner|w_", "player", names(winner))
names(loser) <- sub("loser|l_", "player", names(loser))

names(loser) <- sub("winner|w_", "opponent", names(loser))
names(winner) <- sub("loser|l_", "opponent", names(winner))

# -- Get data for each serving player (essentially 2 rows for each match, each in 
#    pov of server)
slams_2017 <- rbind(winner, loser)

# -- Plot showing different players involved in different number of matches 
slams_2017 %>%
  group_by(player_name) %>%
  dplyr::summarise(
    matches = n()
  ) %>%
  ggplot(aes(x = player_name, y = matches)) + 
  geom_bar(stat = "identity", fill = "#e5952c") + 
  coord_flip() + 
  #theme_hc() + 
  scale_y_continuous("Matches") + 
  scale_x_discrete("")  
  
  
# -- Modelling #####
# Prob(Win point) ~ Who's serving[random] + Who's returning[random] + Surface[fixed]
# * random effects for a group variable are given as (1 | group)

fit <- glmer(
  cbind(playerservewon, playersvpt - playerservewon) ~
    I(surface == "Grass") + 
    I(surface == "Clay") + 
    (1 | player_name) +
    (1 | opponent_name),
  data = slams_2017,
  family = "binomial"
)

summary(fit)

expit <- function(x) exp(x) / (1 + exp(x)) # Inverse logit

expit(fixef(fit)[1])

slams_2017 %>% filter(surface=='Hard') %>%
  mutate(avg = playerservewon/playersvpt) %>%
  .$avg %>%
  mean()


player_effects <- ranef(fit)
player_effects <- data.frame(
  serve_effect = expit(player_effects[["player_name"]][,1] + fixef(fit)[1])
  - expit(fixef(fit)[1]),
  player_name = rownames(player_effects[["player_name"]]),
  stringsAsFactors = F
)


### Empirical Bayes Estimates #####

# -- This is "How much do players differ from the average on hardcourt?"

# Note: Probability of winning for each server is : 
#       expit(player_effects[["player_name"]][,1] + fixef(fit)[1])
#       (Need to include the intercept!)
# Coefficient represents the change in logit with a one unit increase, but how much
# that changes depends on the intercept

player_effects <- ranef(fit) # ranef.mer object
player_effects <- data.frame(
  serve_effect = expit(player_effects[["player_name"]][,1] + fixef(fit)[1])
  - expit(fixef(fit)[1]),
  player_name = rownames(player_effects[["player_name"]]),
  stringsAsFactors = F
)


# Shrinking less certain observations to the mean is a bybroduct of mixed models
observed_effects <- slams_2017 %>%
  filter(surface == "Hard") %>%
  group_by(player_name) %>%
  dplyr::summarise(
    matches = n(),
    serve_effect = mean(playerservewon / playersvpt) - expit(fixef(fit)[1])
  )
combine <- observed_effects %>%
  inner_join(player_effects, by = "player_name")


combine %>%
  ggplot(aes(x = "Observed", xend = "Estimated", 
             y = serve_effect.x, yend = serve_effect.y,
             col = matches)) +
  geom_hline(yintercept = 0, col = "red") + 
  geom_segment() + 
  scale_y_continuous("Service Effect")
