# Get data to plot double fault rates throughout the years for WTA and ATP

library(dplyr)
library(ggplot2)

wta_data_path <- "C:/Users/peter.tea/projects/tennis/data/processed/wta_serve_rates.csv"

wta_rates_data <- read.csv(wta_data_path)


# -=- Look at the data
wta_rates_data %>%
  filter(rank <= 100) %>%
  arrange(year,rank) %>%
  View()


# -=- Summarize df rates by year
wta_year_rate <- wta_rates_data %>%
  dplyr::filter(rank <= 50) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg_matches = mean(matches_played),
            avg_df_rate = mean(avg_df_rate),
            avg_ace_rate = mean(avg_ace_rate),
            avg_ace_df_ratio = mean(ace_df_ratio)) %>%
  dplyr::mutate(avg_df_rate_per10 = avg_df_rate*10,
         avg_ace_rate_per10 = avg_ace_rate*10)

View(wta_year_rate)


# -- Replicate for ATP players

atp_data_path <- "C:/Users/peter.tea/projects/tennis/data/processed/atp_serve_rates.csv"

atp_rates_data <- read.csv(atp_data_path)


# -=- Look at the data
atp_rates_data %>%
  filter(rank <= 100) %>%
  arrange(year,rank) %>%
  View()



# -=- Summarize df rates by year
atp_year_rate <- atp_rates_data %>%
  dplyr::filter(rank <= 50) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg_matches = mean(matches_played),
            avg_df_rate = mean(avg_df_rate),
            avg_ace_rate = mean(avg_ace_rate),
            avg_ace_df_ratio = mean(ace_df_ratio)) %>%
  dplyr::mutate(avg_df_rate_per10 = avg_df_rate*10,
         avg_ace_rate_per10 = avg_ace_rate*10)

View(atp_year_rate)


wta_year_rate$league = 'WTA'
atp_year_rate$league = 'ATP'

all_year_dat <- rbind(wta_year_rate, atp_year_rate)


# ----- === ----- === ----- === ----- ===  ----- === ----- === 
# ----- === ----- === ----- === ----- ===  ----- === ----- === 
library(gganimate, lib = "C:/Users/peter.tea/projects/R")
library(gifski, lib = "C:/Users/peter.tea/projects/R")
library(plyr, lib = "C:/Users/peter.tea/projects/R")
 # Aces plot

y_level <- c("   Aces Hit", "(per 10 Serves)")

y_axis_lab <- y_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")

  
ace_gif_plot <- ggplot() +
  geom_line(data=all_year_dat, #%>% filter(year >=2003), 
            aes(x=year, y=avg_ace_rate_per10,
                color = league)) + 

  geom_point(data=all_year_dat, #%>% filter(year >=2003), 
             aes(x=year, y=avg_ace_rate_per10, 
                 group = seq_along(year),
                 fill = league),
             shape = 21,) +
  
  # -- Change legend color scale
  scale_fill_manual(values = c('#5DADE2', '#EC7063'), 
                     c('ATP', 'WTA')) +
  scale_color_manual(values = c('#5DADE2', '#EC7063'), 
                    c('ATP', 'WTA')) +
  
  scale_x_continuous(name= 'Year',
                     seq(1991,2019,4)) + 
  scale_y_continuous(name = y_axis_lab,
                     seq(0,1.2,0.2)) +
  
  # -- Axis labels
  labs(
    x = 'Year', 
    y = y_axis_lab,
    title =  "Ace Rates Through Time",
    caption="StatsOnTheT")  + 
  
  peter_year_theme() +
  theme(axis.title.y = element_text(hjust = 0.5,
                                    vjust = 1.5)) + 
  
  transition_reveal(year)
  


animate(ace_gif_plot, duration = 7, fps = 20,
        width = 580, height = 340, end_pause = 60,
        renderer = gifski_renderer('C:/Users/peter.tea/projects/tennis/serve_rates_post/plots/ace_trend.gif'))

  
  
  
# -- Repeat for Double Faults
# Double Fault plot

y_level <- c("   Double Faults", "(per 10 Second Serves)")

y_axis_lab <- y_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")


df_gif_plot <- ggplot() +
  geom_line(data=all_year_dat,  
            aes(x=year, y=avg_df_rate_per10,
                color = league)) + 
  
  geom_point(data=all_year_dat, 
             aes(x=year, y=avg_df_rate_per10, 
                 group = seq_along(year),
                 fill = league),
             shape = 21,) +
 # geom_smooth(data=all_year_dat,  
#              aes(x=year, y=avg_df_rate_per10,
#                  color = league)) + 
  
  # -- Change legend color scale
  scale_fill_manual(values = c('#5DADE2', '#EC7063'), 
                    c('ATP', 'WTA')) +
  scale_color_manual(values = c('#5DADE2', '#EC7063'), 
                     c('ATP', 'WTA')) +
  
  scale_x_continuous(name= 'Year',
                     seq(1991,2019,4)) + 
  scale_y_continuous(name = y_axis_lab,
                     seq(0.2,2.5,0.2)) +
  
  # -- Axis labels
  labs(
    x = 'Year', 
    y = y_axis_lab,
    title =  "   Double Fault Rates Through Time",
    caption="StatsOnTheT")  + 
  
  peter_year_theme() +
  theme(axis.title.y = element_text(hjust = 0.5,
                                    vjust = 1.5)) + 
  
  transition_reveal(year)



animate(df_gif_plot, duration = 7, fps = 20,
        width = 580, height = 340, end_pause = 45,
        renderer = gifski_renderer('C:/Users/peter.tea/projects/tennis/serve_rates_post/plots/df_trend.gif'))






# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# --              Repeat Again for Ace to Double Faults Ratio 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 

y_level <- c("Ace to Double Fault", "       Ratio")

y_axis_lab <- y_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")


ratio_gif_plot <- ggplot() +
  geom_line(data=all_year_dat,  
            aes(x=year, y=avg_ace_df_ratio,
                color = league)) + 
  
  geom_point(data=all_year_dat, 
             aes(x=year, y=avg_ace_df_ratio, 
                 group = seq_along(year),
                 fill = league),
             shape = 21,) +
  # geom_smooth(data=all_year_dat,  
  #              aes(x=year, y=avg_df_rate_per10,
  #                  color = league)) + 
  
  # -- Change legend color scale
  scale_fill_manual(values = c('#5DADE2', '#EC7063'), 
                    c('ATP', 'WTA')) +
  scale_color_manual(values = c('#5DADE2', '#EC7063'), 
                     c('ATP', 'WTA')) +
  
  scale_x_continuous(name= 'Year',
                     seq(1991,2019,4)) + 
  scale_y_continuous(name = y_axis_lab,
                     seq(0.5,3,0.5)) +
  geom_hline(yintercept = 1,
             linetype = 'dashed',
             color = '#808080') + 
  
  # -- Axis labels
  labs(
    x = 'Year', 
    y = y_axis_lab,
    title =  "Ace-to-Double-Fault-Ratios Through Time ",
    caption="StatsOnTheT")  + 
  
  peter_year_theme() +
  theme(axis.title.y = element_text(hjust = 0.5,
                                    vjust = 1.5)) + 
  
  transition_reveal(year)



animate(ratio_gif_plot, duration = 7, fps = 20,
        width = 580, height = 340, end_pause = 45,
        renderer = gifski_renderer('C:/Users/peter.tea/projects/tennis/serve_rates_project/serve_rates_post/plots/ratio_trend.gif'))




