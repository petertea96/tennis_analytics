##### --- How much do player stats vary from match-to-match?

##### -- || -- || -- || -- || -- || -- || -- || -- || --
# Loading libraries
library(dplyr)
library(ggplot2)
library(forcats,  lib.loc = "C:/Users/peter.tea/projects/R")
source("C:/Users/peter.tea/projects/tennis/serve_rates_project/peter_historic_theme.R")
##### -- || -- || -- || -- || -- || -- || -- || -- || -- 

#-- Read in raw match data
wta_data_path <- "C:/Users/peter.tea/projects/tennis/data/wta_data/"
year = 2019
data_name <- paste(wta_data_path, 'wta_', year, '.csv', sep = '')
wta_data <- read.csv(data_name)

wta_data %>%
  filter(complete.cases(.)) %>%
   View()

# -- Read in rankings data
wta_rank_filename <- "C:/Users/peter.tea/projects/tennis/data/processed/wta_rankings.csv"
wta_rankings <- read.csv(wta_rank_filename)

wta_complete_data <- wta_data %>%
  left_join(wta_rankings[c('player', 'rank', 'year')],
            by = c('year' = 'year',"server_id" = 'player' ) )  %>%
  dplyr::rename('server_rank' = 'rank')


wta_calculated_data <- wta_complete_data %>%
  filter(complete.cases(.)) %>%
  filter(server_rank <= 25) %>%
  group_by(server, server_id, server_rank) %>%
  summarise(matches_played = n(),
            first_in_sd = sd(pr_1stin),
            first_in_avg = mean(pr_1stin),
            second_in_sd = sd(pr_2ndin),
            second_in_avg = sd(pr_2ndin),
            win_on_serve_sd = sd(pr_win_on_serve), 
            win_on_serve_avg = mean(pr_win_on_serve)) %>%
   #%>%
  arrange(desc(first_in_avg)) %>%
  ungroup() %>% 
  mutate(order = row_number())


################################################################################
# Add theme
# Do same for 2nd serve In
# Do same for ATP
  
wta_first_serve_data <- wta_calculated_data %>% 
  mutate(first_in_se = first_in_sd / sqrt(matches_played),
         first_in_lower = first_in_avg - qt(1 - (0.05 / 2), matches_played - 1) * first_in_se,
         first_in_upper = first_in_avg + qt(1 - (0.05 / 2), matches_played - 1) * first_in_se)
wta_first_serve_data$server <- forcats::fct_reorder(wta_first_serve_data$server,
                                                    wta_first_serve_data$first_in_avg)


ggplot(wta_first_serve_data, aes(x=first_in_avg*100, y=server)) + 
  geom_errorbar(aes(xmin=first_in_lower*100, xmax=first_in_upper*100), 
                width=0.3, 
                colour = '#800000') +
  geom_point(aes(size=matches_played), shape=21, fill="#FFE4E1", colour = '#800000') +
  
  # -- Axis labels
  labs(
    size = 'Matches Played:',
    x = '1st Serve In %', 
    y = NULL,
    title =  "1st Serve In % Among Top WTA Players in 2019",
    caption="@ptea_test\nData: Jeff Sackmann") +
  peter_volatility_theme(family_font = 'sans')
  
# -- Save plot
ggsave('wta_1st_serve_in.jpg',
       width=8.25, height=6,
       dpi = 410)



# ATP Section ---------------------------------

#-- Read in raw match data
atp_data_path <- "C:/Users/peter.tea/projects/tennis/data/atp_data/"
year = 2019
atp_data_name <- paste(atp_data_path, 'atp_', year, '.csv', sep = '')
atp_data <- read.csv(atp_data_name)

atp_data %>%
  filter(complete.cases(.)) %>%
  View()

# -- Read in rankings data
atp_rank_filename <- "C:/Users/peter.tea/projects/tennis/data/processed/atp_rankings.csv"
atp_rankings <- read.csv(atp_rank_filename)

atp_complete_data <- atp_data %>%
  left_join(atp_rankings[c('player', 'rank', 'year')],
            by = c('year' = 'year',"server_id" = 'player' ) )  %>%
  dplyr::rename('server_rank' = 'rank')


atp_calculated_data <- atp_complete_data %>%
  filter(complete.cases(.)) %>%
  filter(server_rank <= 25 | server %in% c('Nick Kyrgios')) %>%
  group_by(server, server_id, server_rank) %>%
  summarise(matches_played = n(),
            first_in_sd = sd(pr_1stin),
            first_in_avg = mean(pr_1stin),
            first_in_lower_q = quantile(pr_1stin, c(0.1)),
            first_in_upper_q = quantile(pr_1stin, c(0.9)),
            second_in_sd = sd(pr_2ndin),
            second_in_avg = sd(pr_2ndin),
            win_on_serve_sd = sd(pr_win_on_serve), 
            win_on_serve_avg = mean(pr_win_on_serve)) %>%
  #%>%
  arrange(desc(first_in_avg)) %>%
  ungroup() %>% 
  mutate(order = row_number())


atp_first_serve_data <- atp_calculated_data %>% 
  mutate(first_in_se = first_in_sd / sqrt(matches_played),
         first_in_lower = first_in_avg - qt(1 - (0.05 / 2), matches_played - 1) * first_in_se,
         first_in_upper = first_in_avg + qt(1 - (0.05 / 2), matches_played - 1) * first_in_se)

atp_first_serve_data$server <- forcats::fct_reorder(atp_first_serve_data$server,
                                                    atp_first_serve_data$first_in_avg)


ggplot(atp_first_serve_data, aes(x=first_in_avg*100, y=server)) + 
  geom_errorbar(aes(xmin=first_in_lower*100, xmax=first_in_upper*100), 
                width=0.3, 
                colour = '#191970') +
  geom_point(aes(size=matches_played), shape=21, fill="#B0E0E6", colour = '#191970') +
  
  # -- Axis labels
  labs(
    size = 'Matches Played:',
    x = '1st Serve In %', 
    y = NULL,
    title =  "1st Serve In % Among Top ATP Players in 2019",
    caption="@ptea_test\nData: Jeff Sackmann") +
  peter_volatility_theme(family_font = 'sans')

# -- Save plot
ggsave('atp_1st_serve_in.jpg',
       width=8.25, height=6,
       dpi = 410)


# plot using 10% and 90% quantiles
ggplot(atp_first_serve_data, aes(x=first_in_avg*100, y=server)) + 
  geom_errorbar(aes(xmin=first_in_lower_q*100, xmax=first_in_upper_q*100), 
                width=0.3, 
                colour = '#191970') +
  geom_point(aes(size=matches_played), shape=21, fill="#B0E0E6", colour = '#191970') +
  
  # -- Axis labels
  labs(
    size = 'Matches Played:',
    x = '1st Serve In %', 
    y = NULL,
    title =  "1st Serve In % Among Top ATP Players in 2019",
    caption="@ptea_test\nData: Jeff Sackmann") +
  peter_volatility_theme(family_font = 'sans')

