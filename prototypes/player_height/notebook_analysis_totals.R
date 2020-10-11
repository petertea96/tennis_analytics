# -- Analyze & plot the Height Advantage in tennis serves
# - Same script as `notebook_analysis.R`, instead we consider averages
#   in terms of total shots / Total opportunities

# -- Set appropriate working directory
setwd("/Users/petertea/tennis_analytics/prototypes/player_height")

# -- Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
extrafont::loadfonts()

source("/Users/petertea/tennis_analytics/prototypes/player_height/src/plot_theme.R")



# -- Load in processed aces data -----
aces_file_name <- '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19_TOTAL.csv'
atp_aces_10_19_df <- read.csv(aces_file_name, stringsAsFactors = FALSE)

atp_aces_10_19_df[,c(12:17)] <- 100*atp_aces_10_19_df[,c(12:17)]

write.csv(atp_aces_10_19_df,
          'atp_aces_10_19_df_TOTAL.csv',
          row.names = TRUE
)
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #

# Plot Aces Allowed vs. Aces Hit ------

atp_aces_10_19_df %>%
  mutate(cumulative_skill_sum = abs(opp_ace_rate_above_expected) + abs(ace_rate_above_expected)) %>%
  arrange(desc(cumulative_skill_sum)) %>%
  View()


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
           color="#058527") +
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
           color="#058527") +
  annotate(geom="text", #x=172.5, 
           x= text_position_right_x,
           y=text_position_down_y - 0.5,
           hjust = 0,
           size = 3.5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Above Average Returner",
           color="#058527") +
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
           color="#058527") +
  annotate("rect", xmin = -Inf, 
           xmax = -15 +10, 
           ymin = -Inf, ymax = text_position_down_y +0.5,
           alpha = .1) + 
  
  geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
  geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
  
  # -- Upper Right Quadrant player names
  geom_label_repel(data = atp_aces_10_19_df %>%
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
  geom_label_repel(data = atp_aces_10_19_df %>%
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
  geom_label_repel(data = atp_aces_10_19_df %>%
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
  geom_label_repel(data = atp_aces_10_19_df %>%
                     filter(player =='Gael Monfils'),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = -2,
                   nudge_x = 1.75,
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
  geom_label_repel(data = atp_aces_10_19_df %>%
                     filter(player =='Andy Murray'),
                   aes(x = ace_rate_above_expected,
                       y = opp_ace_rate_above_expected,
                       label = player,
                       fill = player_height_cm),
                   nudge_y = -4.25,
                   nudge_x = -2,
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
  geom_label_repel(data = atp_aces_10_19_df %>%
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
  geom_label_repel(data = atp_aces_10_19_df %>%
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
  
  geom_point(data = atp_aces_10_19_df,
             aes(x = ace_rate_above_expected,
                 y = opp_ace_rate_above_expected, 
                 fill = player_height_cm),
             alpha = 0.85,
             shape = 21,
             color = 'black',
             size = 2.5
  ) +
  
  # scale_fill_gradient2(midpoint = mean(atp_aces_10_19_df$player_height_cm),
  #                      low = "#83D0E9", mid = "white",
  #                      high = "#DD1717", space = "Lab" ) +
  
  scale_fill_gradient2(midpoint = mean(atp_aces_10_19_df$player_height_cm),
                       low = "#00CCBB", mid = "#FFFBE3",
                       high = "#DD1717", space = "Lab" ) +
  
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



ggsave('atp_trading_aces_total.jpg',
       width=7.25, height=5.5,
       dpi = 450)

# What is the average height of players with above average aces hit rate?
atp_aces_10_19_df %>%
  filter(ace_rate_above_expected > 0) %>%
  select(player_height_cm)%>%
  colMeans()

atp_aces_10_19_df %>%
  filter(ace_rate_above_expected < 0) %>%
  select(player_height_cm)%>%
  colMeans()


# -- Convert to heatmap???
ggplot(data = atp_aces_10_19_df,
       aes(x = ace_rate_above_expected,
           y = opp_ace_rate_above_expected, 
           fill = player_height_cm))+ 
  geom_tile() +
  scale_fill_distiller(palette = "RdPu") 


# library
library(latticeExtra) 

# showing data points on the same color scale 
levelplot(player_height_cm ~ ace_rate_above_expected * opp_ace_rate_above_expected,
          atp_aces_10_19_df, 
          panel = panel.levelplot.points, 
          cex = 1,
          ylim = c(min(atp_aces_10_19_df$opp_ace_rate_above_expected),
                   max(atp_aces_10_19_df$opp_ace_rate_above_expected)), 
          pretty = TRUE, 
          main="Trading Aces in the 2010s: Comparing Aces Hit\nto Aces Allowed",
          xlab = "Aces Hit \n Above Average (%)",
          ylab = "Aces Allowed \n Above Average (%)",
          col.regions=cm.colors(100),
          #col.regions = c('#ddf3f5', '#a6dcef', '#f2aaaa',
          #                '#e36387', 'white')
          colorkey=list(title = 'Player Height\n(cm)')
) + 
  layer_(panel.2dsmoother(..., n = 200)) 




# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || . ' ' . || #
# Height vs. ace rate -----

# Fit cubic regression model
height_model <- lm(data = atp_aces_10_19_df,
                   formula = ace_rate ~ player_height_cm + I(player_height_cm^2) +
                     I(player_height_cm^3))

summary(height_model)

# -- Fit Loess model
height_model_loess <- loess(data = atp_aces_10_19_df,
                            formula = ace_rate ~ player_height_cm )

# -- Plot cubic fit
ggplot(data = atp_aces_10_19_df,
       mapping = aes(x = player_height_cm,
                     y = ace_rate)) +
  
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
           xmax = 184, 
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
           ymin = -Inf, ymax = 0.8,
           alpha = .2) + 
  geom_smooth(method='lm', formula= y~x + I(x^2) + I(x^3),
              color = '#83D0E9') +
  #geom_smooth(method='loess',
  #            color = '#83D0E9') +
  # stat_smooth(method = lm,
  #            formula = y ~ splines::bs(x, df = 6),
  #            color = '#83D0E9')+
  geom_point(fill = '#83D0E9',
             shape = 21, 
             alpha = 0.25) +
  
  # -- Axis labels
  labs(title = "The Height Advantage in Serving Aces (ATP)",
       y = '',
       x = '',
       caption="Stats On-The-T\nData: Tennis Abstract & atptour.com") +
  # method='loess'
  plot_theme(family_font = 'Tahoma',
             text_colour = 'black') 


ggsave('atp_height_univariate.jpg',
       width=6.5, height=5,
       dpi = 450)

# -- For which players does the cubic model not work well for?

expected_ace_rate <- predict(object = height_model_loess,
                             atp_aces_10_19_df)

atp_aces_10_19_df$expected_ace_rate <- expected_ace_rate
# -- For which players does the cubic model not work well for?
outliers_data <- atp_aces_10_19_df %>%
  select(player, matches_played,
         ace_rate, player_height_cm, expected_ace_rate) %>%
  mutate(above_expected = ace_rate - expected_ace_rate,
         resid = resid(height_model)) %>%
  arrange(above_expected)
# Note residual is the same as 'above expected', but I wanted to be certain..

outliers_data_top_10 <- outliers_data %>%
  filter(abs(above_expected) > 5) 



# Add a column with your condition for the color
outliers_data_top_10 <- outliers_data_top_10 %>% 
  mutate(mycolor = ifelse(above_expected>0, "type1", "type2"),
         player_name_and_height = paste(player, ' (',
                                        player_height_cm, ' cm)',
                                        sep = ''))

# -- Add in fake row to better split the data
outliers_data_top_10 <- outliers_data_top_10 %>% 
  add_row(player ='...', matches_played = NA, ace_rate = NA,
          player_height_cm = NA, expected_ace_rate = NA,
          above_expected = 0, resid = NA, mycolor = 'type3',
          player_name_and_height = '...')



# Change order of factor levels
outliers_data_top_10 <- outliers_data_top_10 %>%
  mutate(player_name_and_height = forcats::fct_reorder(player_name_and_height, above_expected,.desc = FALSE)) 
outliers_data_top_10$player_name_and_height

ggplot(data = outliers_data_top_10,
       aes(x = above_expected, 
           y = player_name_and_height)) +
  # -- Draws line between points
  geom_segment(
    aes(x = 0,
        xend = above_expected,
        y = player_name_and_height,
        yend =player_name_and_height,
        color = mycolor),
    #show.legend = FALSE,
    size = 0.75
  )  + 
  # -- Draws residual points
  geom_point(aes(fill = mycolor, color = mycolor),
             shape = 21,
             show.legend = FALSE) +
  
  # -- Add Vertical line
  geom_vline(xintercept=0,
             linetype="dashed") +
  
  scale_color_manual(values = c('#006164','#CA0020', '#FEF5E7'), 
                     name ='',
                     breaks=c('type1', 'type2', 'type3'),
                     labels = c('Above Expected', 'Below Expected', '')
                     ) +
  scale_fill_manual(values = c('#0571B0','#DB4325', '#FEF5E7'), 
                    #c('type1', 'type2', 'type3')
                    ) +
  
  # -- Axis labels
  labs(x = 'Ace Rate Above Expected (%)',
       y = '',
       title =  "Largest Residuals from\nHeight vs. Ace Rate Model",
       caption="On-The-T") +
  plot_theme(family_font = 'Tahoma',
             text_colour = 'black',
             axis_text_size = 10) +
  theme(
    #legend.position = "none",
        axis.text.y = element_text(face = 'bold')) 

ggsave('atp_height_residuals.jpg',
       width=6.5, height=5,
       dpi = 350)


# -- Scatterplot matrix
library(GGally)
atp_aces_10_19_df$player_bmi <- atp_aces_10_19_df$player_weight_kg / (atp_aces_10_19_df$player_height_cm/100)^2

my_fn <- function(data, mapping, method="lm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.5,
               shape = 21,
               fill = '#83D0E9') +
    geom_smooth(method=method,color = '#83D0E9', ...) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4))
  
  
  p
}


ggpairs(data = atp_aces_10_19_df %>% 
          dplyr::filter(player != 'Olivier Rochus') %>%
          dplyr::select(ace_rate,
                        opp_ace_rate,
                        player_weight_kg,
                        player_height_cm,
                        player_bmi) %>%
          dplyr::rename(c('Aces Hit (%)' = 'ace_rate',
                          'Aces Allowed (%)' = 'opp_ace_rate',
                          'Weight (Kg)' = 'player_weight_kg',
                          'Height (cm)' = 'player_height_cm',
                          'BMI' = 'player_bmi')), 
        lower= list(continuous = wrap(my_fn)),
        upper = list(continuous = wrap("cor", size = 5, digits = 2)),
        title="") +
  plot_theme(family_font = 'Tahoma',
             text_colour = 'black',
             axis_text_size = 8
  )

ggsave('scatterplot.jpg',
       width=6, height=6.5,
       dpi = 450)



