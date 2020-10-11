# -- Analyze & plot the Height Advantage in tennis serves

# -- Set appropriate working directory
setwd("/Users/petertea/tennis_analytics/prototypes/player_height")

# -- Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
extrafont::loadfonts()

source("/Users/petertea/tennis_analytics/prototypes/player_height/plot_theme.R")

# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Identify Which Players to look at
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# -- Include players who played in 50 matches in the 2010s (2010 - 2019)
# -- Include players who reached a top 20 ranking (in the labels?)
# -- Maybe plot all points from players who played 50 matches,
#    but label only the "interesting" names?

# atp_ranking_filename <- "/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/end_of_year_atp_rankings.csv"
# atp_rankings <- read.csv(atp_ranking_filename)

# -- Which player labels will we display on the graphic?
# -- Players who held an end-of-year ranking of 20
# player_ids_to_keep <- atp_rankings %>%
#   filter((year >= 2010) & (year <= 2019)) %>%
#   filter(rank <= 10) %>%
#   select(player) %>%
#   .$player


# -- Load in processed aces data -----
aces_file_name <- '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19.csv'
atp_aces_20_19_df <- read.csv(aces_file_name, stringsAsFactors = FALSE)

atp_aces_20_19_df[,c(4:9)] <- 100*atp_aces_20_19_df[,c(4:9)]

write.csv(atp_aces_20_19_df,
          'atp_aces_20_19_df.csv',
          row.names = TRUE
          )
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #

# Plot Aces Allowed vs. Aces Hit ------
# ***** ... ***** ... ***** ... ***** ...  ***** ... ***** ... *****
# -- If considering height in terms of Feet and Inches:
# ***** ... ***** ... ***** ... ***** ...  ***** ... ***** ... *****
# atp_aces_20_19_df <- atp_aces_20_19_df %>%
#   mutate(grouped_height_inches = ifelse(player_height_cm <=173,
#                                         "At or below 5'8\"",
#                                         ifelse(player_height_cm>=196, "At or above 6'5\"",
#                                                player_height_inches)))
# 
# height_level_vector <- c("At or below 5'8\"", "5'9\"", "5'10\"", "5'11\"", "6'0\"",
#                          "6'1\"", "6'2\"", "6'3\"", "6'4\"", "At or above 6'5\"")
# 
# atp_aces_20_19_df$grouped_height_inches <- factor(atp_aces_20_19_df$grouped_height_inches,
#                                                   levels = height_level_vector)
# 
# atp_aces_20_19_df$grouped_height_integer <- as.integer(atp_aces_20_19_df$grouped_height_inches)

# -- There are just too many names...
# atp_aces_20_19_df %>%
#   filter(player_id %in% player_ids_to_keep) %>%
#   View()
# 
# atp_aces_20_19_df %>%
#   arrange(avg_ace_rate) %>%
#   View()
# atp_aces_20_19_df %>%
#   arrange(opp_avg_ace_rate) %>%
#   View()

atp_aces_20_19_df %>%
  mutate(cumulative_skill_sum = abs(opp_ace_rate_above_expected) + abs(ace_rate_above_expected)) %>%
  arrange(cumulative_skill_sum) %>%
  View()

# player_names <- c('Andy Roddick', 'Nick Kyrgios', 'Reilly Opelka','Ivo Karlovic', 'John Isner',
#   'Gael Monfils', 'Benoit Paire', 'Milos Raonic', 'Jo-Wilfried Tsonga', 'Denis Shapovalov',
#      
#   'David Ferrer', 'Rafael Nadal', 'Novak Djokovic', 'Andy Murray','Roger Federer',
#   'Daniil Medvedev', 'Alexander Zverev')

player_names_quad_1 <- c('Andy Roddick', 'Nick Kyrgios', 'Reilly Opelka','John Isner')
player_names_quad_2 <- c('Benoit Paire', 'Milos Raonic', 'Denis Shapovalov')
player_names_quad_3 <- c('Fabio Fognini', 'David Ferrer', 'Diego Schwartzman')
player_names_quad_4 <- c('Olivier Rochus', 'Yoshihito Nishioka')

text_position_right_x <- 9
text_position_up_y <- 8.5
text_position_down_y <- -5
text_position_left_x <- -15

ggplot() +
  # -- Manually adding Upper Right Quadrant
  annotate(geom="text", #x=172.5, 
           x= text_position_right_x,
           y=text_position_up_y,
           hjust = 0,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Above Average Server",
           color="green3") +
  annotate(geom="text", #x=172.5, 
           x= text_position_right_x,
           y=text_position_up_y - 0.5,
           hjust = 0,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Below Average Returner",
           color="red") +
  annotate("rect", xmin = text_position_right_x - 0.5, 
           xmax = Inf, 
           ymin = text_position_up_y - 1,
           ymax = Inf,
           alpha = .1) + 
  
  # -- Manually adding Lower Right Quadrant
  annotate(geom="text", #x=172.5, 
           x= text_position_right_x,
           y=text_position_down_y,
           hjust = 0,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Above Average Server",
           color="green3") +
  annotate(geom="text", #x=172.5, 
           x= text_position_right_x,
           y=text_position_down_y - 0.5,
           hjust = 0,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Above Average Returner",
           color="green3") +
  annotate("rect", xmin = text_position_right_x - 0.5, 
           xmax = Inf, 
           ymin = -Inf, ymax = text_position_down_y +0.5,
           alpha = .1) + 
  
  # -- Manually adding Upper Left Quadrant
  annotate(geom="text", #x=172.5, 
           x= -Inf,
           y=text_position_up_y,
           hjust = -0.05,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Below Average Server",
           color="red") +
  annotate(geom="text", #x=172.5, 
           x= -Inf,
           y=text_position_up_y - 0.5,
           hjust = -0.05,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Below Average Returner",
           color="red") +
  annotate("rect", xmin = -Inf, 
           xmax = -15 + 10, 
           ymin = text_position_up_y - 1,
           ymax = Inf,
           alpha = .1) + 
  
  # -- Manually adding Lower Left Quadrant
  annotate(geom="text", #x=172.5, 
           x= -Inf,
           y=text_position_down_y,
           hjust = -0.05,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Below Average Server",
           color="red") +
  annotate(geom="text", #x=172.5, 
           x= -Inf,
           y=text_position_down_y - 0.5,
           hjust = -0.05,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Above Average Returner",
           color="green3") +
  annotate("rect", xmin = -Inf, 
           xmax = -15 +10, 
           ymin = -Inf, ymax = text_position_down_y +0.5,
           alpha = .1) + 
  
  geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
  geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
  
  # -- Upper Right Quadrant player names
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player %in% player_names_quad_1),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = 2.8,
                   nudge_x = 0,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                    label.padding = unit(0.25, "lines")
   ) +
  # -- Karlovic
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player == 'Ivo Karlovic'),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = 1,
                   nudge_x = 3,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  # -- Lower Right Quadrant player names
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player %in% player_names_quad_2),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = 0,
                   nudge_x = 8,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  # Gael Monfils
  # -- Lower Right Quadrant player names
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player =='Gael Monfils'),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = -2,
                   nudge_x = 3,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  # Andy Murray
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player =='Andy Murray'),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = -4.25,
                   nudge_x = -0.75,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  
  # -- Lower Left Quadrant player names
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player %in% player_names_quad_3),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = 0,
                   nudge_x = -9,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  # -- Upper Left Quadrant player names
  geom_label_repel(data = atp_aces_20_19_df %>%
                     filter(player %in% player_names_quad_4),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = 0,
                   nudge_x = -7,
                   fontface = 'bold',
                   size = 2.8,
                   show.legend = FALSE,
                   arrow = arrow(ends ='last',
                                 type = 'closed',
                                 length = unit(0.015, "npc")),
                   point.padding = unit(0.5, "lines"),
                   label.padding = unit(0.25, "lines")
  ) +
  
  geom_point(data = atp_aces_20_19_df,
             aes(x = ace_rate_above_expected,
                 y = opp_ace_rate_above_expected, 
                 fill = player_height_cm),
             alpha = 0.75,
             shape = 21,
             size = 2.5
  ) +
  
  scale_fill_gradient2(midpoint = mean(atp_aces_20_19_df$player_height_cm),
                       low = "#83D0E9", mid = "white",
                       high = "indianred", space = "Lab" ) +
  labs(title = 'Trading Aces in the 2010s: Comparing Aces Hit\nto Aces Allowed',
       x='Aces Hit \n Above Average',
       y = 'Aces Allowed\n Above Average',
       fill = "Player Height \n(cm)",
       caption="Stats On-The-T\nData: Tennis Abstract &\natptour.com"
  ) + 
  
  scale_x_continuous(
    limits = c(-15,19),
    breaks = c(-10, -5, 0, 5, 10, 15),
    labels=c("-0.1" = "-10 %", '-0.05' ="-5 %", '0' = '0 %', "0.05" = "+5 %", "0.1" = "+10 %",
             "0.15" = "+15 %")
  ) + 
  scale_y_continuous(
    limits = c(-5.5,8.5),
    breaks = c(-5, -2.5, 0, 2.5, 5, 7.5),
    labels=c('-0.05' ="-5 %", "-0.025" = "-2.5 %","0" = "0 %", "0.025" = "+2.5 %",
             '0.05' ="+5 %", '0.075' ="+7.5 %")
  ) +
  plot_theme(family_font = 'Tahoma') +
  theme(legend.position = c(0.05, -0.15))



ggsave('atp_trading_Aces.jpg',
       width=7.25, height=5.5,
       dpi = 450)

ggsave('atp_trading_acess.jpg',
       width=7.25, height=5.5,
       dpi = 275)


ggplot(data = atp_aces_20_19_df,
       aes(x = ace_rate_above_expected,
           y = opp_ace_rate_above_expected, 
           fill = player_height_cm))+ 
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") 












# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #

### -- Univariate Analysis -----
height_model <- lm(data = atp_aces_20_19_df,
                   formula = avg_ace_rate ~ player_height_cm + I(player_height_cm^2))

summary(height_model)


# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# Height vs. ace rate -----
ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_height_cm,
                     y = avg_ace_rate)) +
  
  # -- Manually adding y-axis
  annotate(geom="text", #x=172.5, 
           x= -Inf + 1, hjust = -0.1,
           y=23.5, 
           size = 4,
           #hjust = 0,
           family = 'Tahoma',
           fontface = 'bold',
           label="Average Ace Rate (%)",
           color="black") +
  annotate("rect", #xmin = 167.5,
           xmin = -Inf, 
           xmax = 185, 
           ymin = 22.5, ymax = 24.5,
           alpha = .2) + 
  
  # -- Manually adding x-axis
  annotate(geom="text", x=204, #y=0, 
           y = -Inf, vjust = -1,
           size = 4,
           family = 'Tahoma',
           fontface = 'bold',
           label="Official Listed Height (cm)",
           color="black") +
  
  annotate("rect", xmin = 193, xmax = 215, 
           ymin = -Inf, ymax = 1.75,
           alpha = .2) + 
  geom_smooth(method='lm', formula= y~x + I(x^2),
              color = '#83D0E9') +
  
  geom_point(fill = '#83D0E9',
             shape = 21, 
             alpha = 0.5) +

  # -- Axis labels
  labs(title = "The Height Advantage in Serving Aces (ATP)",
       y = '',
       x = '',
       caption="Stats On-The-T\nData: Tennis Abstract & atptour.com") +
  # method='loess'
  plot_theme(family_font = 'Tahoma',
             text_colour = 'black') 

# -- should add 95% confidence band estimates on ace rates?


# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# Weight vs. ace rate -----
weight_model <- lm(data = atp_aces_20_19_df,
                   formula = avg_ace_rate ~ player_weight_lbs + I(player_weight_lbs^2))
summary(weight_model)

ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_weight_lbs,
                     y = avg_ace_rate)) +
  geom_smooth(method='lm', formula= y~x + I(x^2),
              color = 'green') +
  
  geom_point(fill = 'green',
             shape = 21, 
             alpha = 0.5) +
  
  # -- Axis labels
  labs(title = "The Weight Advantage in Serving Aces (ATP)",
       y = '',
       x = '',
       caption="Stats On-The-T\nData: Tennis Abstract & atptour.com") +
  # method='loess'
  plot_theme(family_font = 'Tahoma',
             text_colour = 'black') 


# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# BMI vs. ace rate -----
atp_aces_20_19_df$player_bmi <- atp_aces_20_19_df$player_weight_kg / (atp_aces_20_19_df$player_height_cm/100)^2

atp_aces_20_19_df %>%
  arrange(desc(player_bmi)) %>%
  View()

bmi_model <- lm(data = atp_aces_20_19_df,
                formula = avg_ace_rate ~ player_bmi )
summary(bmi_model)

ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_bmi,
                     y = avg_ace_rate)) +
  geom_point(fill = 'green',
             shape = 21, 
             alpha = 0.5) 


# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
#### -- Returning ability (Agility) -----

ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_height_cm,
                     y = opp_avg_ace_rate)) +
  geom_point(fill = 'green',
             shape = 21, 
             alpha = 0.5) 

# -- Weak relationship between weight and preventing aces
ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_weight_lbs,
                     y = opp_avg_ace_rate)) +
  geom_point(fill = 'green',
             shape = 21, 
             alpha = 0.5) 

# -- bmi is not a good predictor of preventing aces
ggplot(data = atp_aces_20_19_df,
       mapping = aes(x = player_bmi,
                     y = opp_avg_ace_rate)) +
  geom_point(fill = 'green',
             shape = 21, 
             alpha = 0.5) 
  