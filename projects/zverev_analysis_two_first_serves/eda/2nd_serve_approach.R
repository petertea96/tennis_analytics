### Tennis fun
#Inspiration: https://fivethirtyeight.com/features/why-tennis-players-dont-take-more-risks-on-second-serves/

## Purpose: Recreate the Observed 2nd serve win % vs. Theoretical 2nd serve win % for the
## 2019 Tennis season.

# Last update: Wednesday, March 4th, 2020


tennis_dat <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2019.csv")

# Number second serve opportunities = Total serve games - total 1st serves played
# --> w_svpt - w_1stIn

# Second serve percent = Number second serves in / Number of second serve opportunities
# --> (Number of second serve opportunities - # Double faults) / Number of second serve opportunities
#Note this is equivalent to: 
# --> Pr(win on 2nd serve) = Pr(win | 2nd serve in) * Pr(2nd serve in)
# --> prob_second_serve_win_given_in * second_serve_in_percent


relevant_data_winning_players <- tennis_dat %>%
  # remove incomplete matches
  # --> Scores include "RET"
  dplyr::filter(!grepl(pattern = "RET", x = score)) %>%
  
  # select relevent columns
  dplyr::select(surface, winner_name, best_of, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_df,
                loser_name, minutes, score) %>%
  
  
  dplyr::mutate(server_name = winner_name,
                opponent_name = loser_name,
                first_serve_in_percent = w_1stIn / w_svpt,
                prob_first_serve_win_given_in = w_1stWon / w_1stIn,
                second_serve_in_percent = (w_svpt - w_1stIn - w_df) / (w_svpt - w_1stIn),
                prob_second_serve_win_given_in = w_2ndWon / (w_svpt - w_1stIn - w_df),
                observed_2nd_serve_win_percentage = w_2ndWon / (w_svpt - w_1stIn),
                match_win_indicator = 1
                  ) %>%
  
  dplyr::select(server_name, opponent_name, first_serve_in_percent, prob_first_serve_win_given_in,
                second_serve_in_percent, prob_second_serve_win_given_in, match_win_indicator,
                observed_2nd_serve_win_percentage, score,
                surface) %>%
  
  dplyr::arrange(desc(first_serve_in_percent))





relevant_data_losing_players <-  tennis_dat %>%
  # remove incomplete matches
  # --> Scores include "RET"
  dplyr::filter(!grepl(pattern = "RET", x = score)) %>%
  
  # select relevent columns
  dplyr::select(surface, loser_name, best_of, l_svpt, l_1stIn, l_1stWon, l_2ndWon, l_df,
                winner_name, minutes, score) %>%
  
  dplyr::mutate(server_name = loser_name,
                opponent_name = winner_name,
                first_serve_in_percent = l_1stIn / l_svpt,
                prob_first_serve_win_given_in = l_1stWon / l_1stIn,
                second_serve_in_percent = (l_svpt - l_1stIn - l_df) / (l_svpt - l_1stIn),
                prob_second_serve_win_given_in = l_2ndWon / (l_svpt - l_1stIn - l_df),
                observed_2nd_serve_win_percentage = l_2ndWon / (l_svpt - l_1stIn),
                match_win_indicator = 0
                
  ) %>%
  
  dplyr::select(server_name, opponent_name, first_serve_in_percent, prob_first_serve_win_given_in,
                second_serve_in_percent, prob_second_serve_win_given_in, match_win_indicator, 
                observed_2nd_serve_win_percentage, score,
                surface) %>%
  
  dplyr::arrange(desc(first_serve_in_percent))



# Combine both datasets
relevant_data <- rbind(relevant_data_winning_players, relevant_data_losing_players)

#Remove NA values
relevant_data <- relevant_data[complete.cases(relevant_data),]

#Calculate expected 2nd serve win percentage
relevant_data_to_plot <- relevant_data %>%
  dplyr::mutate(theoretical_2nd_serve_win_percentage = first_serve_in_percent * prob_first_serve_win_given_in) %>%
  dplyr::select(server_name, observed_2nd_serve_win_percentage,
                theoretical_2nd_serve_win_percentage,
                match_win_indicator, surface, opponent_name
                )




ggplot(data = relevant_data_to_plot, aes(x=observed_2nd_serve_win_percentage, 
                                         y=theoretical_2nd_serve_win_percentage)) +
  geom_point(aes(color = as.factor(match_win_indicator)), alpha = 0.5)  +
  geom_abline(slope = 1, intercept = 0) + 
  ggtitle("A new serving strategy?") + 
  xlab("Actual 2nd serve Win percentage") +
  ylab("If 2nd serve approach was like 1st") + 
  labs(color = "Won Match" ) +
  scale_color_manual(values=c("#4be269", "#fc9559"), labels = c("No", "Yes") ) +
  xlim(0.1, 1) + 
  ylim(0.1, 0.8) + 
  
  theme(panel.background = element_rect(fill = "#f3f6fc", # background colour
                                       colour = "black", # border colour
                                       size = 0.5, linetype = "solid"),
       plot.title=element_text(size = rel(1.6),
                               face = "bold", hjust = 0.5),
       legend.position = "bottom",
       legend.background = element_rect(colour = "gray"),
       legend.key = element_rect(fill = "gray90"),
       axis.title = element_text(face = "bold", size = 13))


### What about across different surfaces???
relevant_data_to_plot_clay <- relevant_data_to_plot %>%
  filter(surface == "Clay")

ggplot(data = relevant_data_to_plot_clay, aes(x=observed_2nd_serve_win_percentage, 
                                         y=theoretical_2nd_serve_win_percentage)) +
  geom_point(aes(color = as.factor(match_win_indicator)), alpha = 0.5)  +
  geom_abline(slope = 1, intercept = 0) + 
  ggtitle("A new serving strategy? (Clay)") + 
  xlab("Actual 2nd serve Win percentage") +
  ylab("If 2nd serve approach was like 1st") + 
  labs(color = "Won Match" ) +
  scale_color_manual(values=c("#4be269", "#fc9559"), labels = c("No", "Yes") ) +
  xlim(0.1, 1) + 
  ylim(0.1, 0.8) + 
  
  theme(panel.background = element_rect(fill = "#f3f6fc", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1.6),
                                face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13))


relevant_data_to_plot_grass <- relevant_data_to_plot %>%
  filter(surface == "Grass")

ggplot(data = relevant_data_to_plot_grass, aes(x=observed_2nd_serve_win_percentage, 
                                              y=theoretical_2nd_serve_win_percentage)) +
  geom_point(aes(color = as.factor(match_win_indicator)), alpha = 0.5)  +
  geom_abline(slope = 1, intercept = 0) + 
  ggtitle("A new serving strategy? (Grass)") + 
  xlab("Actual 2nd serve Win percentage") +
  ylab("If 2nd serve approach was like 1st") + 
  labs(color = "Won Match" ) +
  scale_color_manual(values=c("#4be269", "#fc9559"), labels = c("No", "Yes") ) +
  xlim(0.1, 1) + 
  ylim(0.1, 0.8) + 
  
  theme(panel.background = element_rect(fill = "#f3f6fc", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1.6),
                                face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13))


relevant_data_to_plot_hard <- relevant_data_to_plot %>%
  filter(surface == "Hard")

ggplot(data = relevant_data_to_plot_hard, aes(x=observed_2nd_serve_win_percentage, 
                                               y=theoretical_2nd_serve_win_percentage)) +
  geom_point(aes(color = as.factor(match_win_indicator)), alpha = 0.5)  +
  geom_abline(slope = 1, intercept = 0) + 
  ggtitle("A new serving strategy? (Hard)") + 
  xlab("Actual 2nd serve Win percentage") +
  ylab("If 2nd serve approach was like 1st") + 
  labs(color = "Won Match" ) +
  scale_color_manual(values=c("#4be269", "#fc9559"), labels = c("No", "Yes") ) +
  xlim(0.1, 1) + 
  ylim(0.1, 0.8) + 
  
  theme(panel.background = element_rect(fill = "#f3f6fc", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1.6),
                                face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13))



##### Calculate some summary statsitics
relevant_data_to_plot %>%
  group_by(server_name) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  View()


relevant_data_to_plot %>%
  summarise(good_strat = sum(theoretical_2nd_serve_win_percentage > observed_2nd_serve_win_percentage)/ nrow(relevant_data_to_plot) )

# Only in 27% of cases is this actually a good idea...


# What about among players that lost the match:

relevant_data_to_plot %>%
  dplyr::filter(match_win_indicator == 0) %>%
  summarise(good_strat = sum(theoretical_2nd_serve_win_percentage > observed_2nd_serve_win_percentage)/ nrow(relevant_data_to_plot[relevant_data_to_plot$match_win_indicator == 0,]) )


# Which players tend to have better theoretical 2nd serve wins
relevant_data_to_plot %>%
  dplyr::group_by(server_name, surface) %>%
  dplyr::summarise(theo_adv = sum(theoretical_2nd_serve_win_percentage > observed_2nd_serve_win_percentage),
                   num_matches = n()) %>%
  dplyr::arrange(desc(theo_adv))




