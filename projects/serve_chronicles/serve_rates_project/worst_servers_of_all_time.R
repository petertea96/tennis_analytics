# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
#                  Dumbbell Chart of the most recorded
#               One side womens, other side mens df rates
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
library(dplyr)
library(ggplot2)

source("C:/Users/peter.tea/projects/tennis/peter_historic_theme.R")

abbreviated_name <- function(server_name, year){
  last_name <- strsplit(server_name, " ")[[1]][2]
  first_letter <- substr(server_name,1,1)
  short_name <- paste(first_letter, '. ', last_name, sep = "")
  
  final_label <- paste(short_name, ' (', year, ')', sep = '')
  
  return(final_label)
}

# ***** # ***** # ***** # ***** # ***** # ***** # ***** # ***** # ***** # ***** #
# -- Load Data
# ***** # ***** # ***** # ***** # ***** # ***** # ***** # ***** # ***** # ***** #
#data_path <- "/Users/petertea/Documents/GitHub/history_serves/data/processed/"
data_path <- "C:/Users/peter.tea/projects/tennis/data/processed/"

wta_df <- read.csv( paste(data_path, "wta_serve_rates.csv", sep = ''),
                    stringsAsFactors = FALSE)

# -- Look at data

wta_df %>%
  filter(matches_played >= 20) %>%
  arrange(desc(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  filter(avg_df_rate_per10 > 2) %>%
  View()

wta_df %>%
  arrange(desc(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  filter(server == 'Maria Sharapova') %>%
  View()

wta_df %>%
  arrange(desc(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  filter(server == 'Elena Dementieva') %>%
  View()


wta_df_worst <- wta_df %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  arrange(desc(avg_df_rate)) %>%
  head(8) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10,
         df_rank = 8:1)
wta_df_worst$server_tag <- mapply(wta_df_worst$server,
                                  wta_df_worst$year,
                                  FUN = abbreviated_name)


atp_df <- read.csv( paste(data_path, "atp_serve_rates.csv", sep = ''),
                    stringsAsFactors = FALSE)


atp_df %>%
  filter(matches_played >= 20) %>%
  arrange(desc(avg_df_rate)) %>%
  head(100) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  View()


atp_df_worst <- atp_df %>%
  filter(matches_played >= 30) %>%
  filter(rank <= 50) %>%
  arrange(desc(avg_df_rate)) %>%
  head(8) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10,
         df_rank = 8:1) 
atp_df_worst$server_tag <- mapply(atp_df_worst$server,
                                  atp_df_worst$year,
                                  FUN = abbreviated_name)



wta_df_worst$league = 'WTA'
atp_df_worst$league = 'ATP'

all_dat <- rbind(wta_df_worst, atp_df_worst)




# --- == -- == ---
x_level <- c("Double Faults", "(per 10 Second Serves)")
x_axis_lab <- x_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")

family_font = 'mono'

ggplot() +
  
  # -- Draws line between points
  geom_segment(
    aes(x = atp_df_worst$avg_df_rate_per10,
        xend = wta_df_worst$avg_df_rate_per10,
        y = atp_df_worst$df_rank,
        yend = wta_df_worst$df_rank),
    size = 0.75, color = "black"
  ) +
  
  # -- Draws ATP & WTA points
  geom_point(
    data = all_dat,
    aes(x = avg_df_rate_per10, 
        y = df_rank, 
        color = league), 
    size = 3.5
  ) +
  
  # -- = Draws Name Labels (change to ggrepel?)
  # - Add WTA name label
  #  geom_text(aes(x= wta_df_worst$avg_df_rate_per10, 
  #                y=8:1, 
  #                label=wta_df_worst$server_tag,
  #                family=family_font),
  #            color='#A93226', 
  #            size=2.75, 
  #            vjust=-1.5) +
  
geom_label(aes(x= wta_df_worst$avg_df_rate_per10, 
               y=8:1, 
               label=wta_df_worst$server_tag,
               family=family_font),
           color='#A93226', 
           size=3,
           nudge_y = 0.4,
           nudge_x = 0) +
  
  
  
  # - Add WTA numeric value
  geom_text(aes(x=wta_df_worst$avg_df_rate_per10 + 0.1,
                y=8:1, 
                label=round(wta_df_worst$avg_df_rate_per10, 2)),
            family=family_font,
            color= 'black',
            fontface = 'bold',
            size=3, 
            vjust=0.5) +
  
  # - Add ATP name label
  # geom_text(aes(x= atp_df_worst$avg_df_rate_per10 + 0.1, 
  #                y=8:1, 
  #                label=atp_df_worst$server_tag),
  #            color='#1A5276', 
  #            size=2.75, 
  #            vjust=-1.5,
  #            family=family_font) +
  
  geom_label(aes(x= atp_df_worst$avg_df_rate_per10 + 0.1, 
                 y=8:1, 
                 label=atp_df_worst$server_tag),
             color='#1A5276', 
             size=3, 
             family=family_font,
             nudge_y = 0.4,
             #label.padding = 0.5
  ) +
  
  
  # - Add ATP numeric value
  geom_text(aes(x=atp_df_worst$avg_df_rate_per10  - 0.1,
                y=8:1, 
                label=round(atp_df_worst$avg_df_rate_per10, 2),
                family = family_font),
            color='black', 
            fontface  ='bold',
            size=3, 
            vjust= 0.5) +
  
  # -- Change legend color scale
  scale_color_manual(values = c('#5DADE2', '#EC7063'), 
                     c('ATP', 'WTA')) +
  
  # -- Axis labels
  labs(
    x = x_axis_lab, 
    #y = 'Performance \nRank',
    y = NULL,
    
    title =  "ATP & WTA Worst Double Fault \nYears Recorded",
    caption="StatsOnTheT") +
  
  # -- Axis limits
  ylim(c(0.5,8.5))  + 
  xlim(c(1.65, 4)) +
  
  # -- Add custom theme
  peter_historic_theme()


