# -- Which players have come back down 2 sets to love?
# -- Which players have forced a 5th set, when down 2 sets to love

library(dplyr)
library(ggplot2)
source("/Users/petertea/tennis_analytics/prototypes/comeback_down_two_sets_to_love/src/ggplot_theme.R")

# https://tt.tennis-warehouse.com/index.php?threads/most-comebacks-from-two-sets-down.567602/

atp_set_data_path <- "/Users/petertea/tennis_analytics/prototypes/comeback_down_two_sets_to_love/historic_set_data.rds"

atp_set_data <- readRDS(atp_set_data_path)

View(atp_set_data)

complete_comeback_entries <- atp_set_data %>%
  filter(five_set_complete_comeback == 1) 

atp_set_data %>%
  filter(five_set_complete_comeback == 1) %>%
  filter(winner == 'Roger Federer') %>%
  View()


# Count how many times a player has completed a 'down-2-sets-to-love' comeback
player_comeback_counts <- complete_comeback_entries %>%
  group_by(winner) %>%
  summarise(complete_comebacks = n()) %>%
  arrange(desc(complete_comebacks ))
  
# Count how many times a player has forced a 5th set from 'down-2-sets-to-love'
player_incomplete_comeback_counts <- complete_comeback_entries %>%
  group_by(loser) %>%
  summarise(incomplete_comebacks = n()) %>%
  arrange(desc(incomplete_comebacks))


down_2_sets_to_love_data <- player_comeback_counts %>%
  left_join(player_incomplete_comeback_counts,
            by = c('winner' = 'loser'))

down_2_sets_to_love_data[is.na(down_2_sets_to_love_data)] <- 0

down_2_sets_to_love_data <- down_2_sets_to_love_data %>%
  mutate(forced_5th_set = complete_comebacks + incomplete_comebacks)


# Add 1 to Andy Murray (US Open 2020, Sep 1st)
down_2_sets_to_love_data[down_2_sets_to_love_data$winner == 'Andy Murray',2] <- 10
down_2_sets_to_love_data[down_2_sets_to_love_data$winner == 'Andy Murray',4] <- 11


data_to_plot <- down_2_sets_to_love_data %>%
  head(10)

data_to_plot <- data_to_plot %>%
  mutate(winner = forcats::fct_reorder(winner, complete_comebacks)) 


ggplot() +
  geom_segment(
    aes(x = 0,
        xend = data_to_plot$forced_5th_set,
        y = data_to_plot$winner,
        yend = data_to_plot$winner),
    size = 0.75, color = "#C0C0C0"
  ) + 
  # -- Draws line between points
  geom_segment(
    aes(x = 0,
        xend = data_to_plot$complete_comebacks,
        y = data_to_plot$winner,
        yend = data_to_plot$winner,
        color=ifelse(data_to_plot$winner == "Andy Murray", 'black', 'pink')),
    size = 0.75, #color = "black"
    show.legend = FALSE
  ) +
# -- Drawspoints
  geom_point(
    data = data_to_plot,
    aes(x = forced_5th_set, 
        y = winner, 
        fill = 'Forced 5th Set'), 
    #shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size =3,
    color = '#C0C0C0'
    #fill = '#C0C0C0'
    )  +
  
geom_point(
  data = data_to_plot,
  aes(x = complete_comebacks, 
      y = winner, fill = 'Completed Comeback'), 
  shape = 21, # This adds black outline to points (why can't I use fill/color???)
  size =3.5,
  #fill = 'blue'
  ) +
  
  scale_color_manual(values = c('#ff3800', '#83D0E9')) +
  
  scale_fill_manual(values = c('#B0E0E6', '#C0C0C0'),
                    c('Completed Comeback', 'Forced 5th Set'),
                    name = " ")+
  
  scale_x_continuous(name = '# of Matches',
                     seq(0,15,5)) +
  
  # -- Axis labels
  labs(title = "Storming Back Down 2-Sets-To-Love",
       y = '',
       caption="@ptea_test\nData: Jeff Sackmann") +
  peter_theme(family_font = 'Tahoma',
                         text_colour = 'black')

# 'Trebuchet MS'
fonts()

# -- Save plot
ggsave('back_down_2_sets.jpg',
       width=6.25, height=5,
       dpi = 410)




