# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
#                  Dumbbell Chart of the most recorded
#               One side womens, other side mens df rates
#               Each row represents a year 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
# ----- || ----- || ----- || ----- || ----- || ----- || ----- || ----- || 
source("C:/Users/peter.tea/projects/tennis/serve_rates_project/peter_historic_theme.R")
library(dplyr)
library(ggplot2)

abbreviated_name <- function(server_name){
  last_name <- strsplit(server_name, " ")[[1]][2]
  first_letter <- substr(server_name,1,1)
  short_name <- paste(first_letter, '. ', last_name, sep = "")
  
  final_label <- short_name
  return(final_label)
}


data_path <- "C:/Users/peter.tea/projects/tennis/data/processed/"

wta_df <- read.csv( paste(data_path, "wta_serve_rates.csv", sep = ''),
                    stringsAsFactors = FALSE)
atp_df <- read.csv( paste(data_path, "atp_serve_rates.csv", sep = ''),
                    stringsAsFactors = FALSE)


# -- DOUBLE FAULTING ANALYSIS

wta_df_worst_each_year <- wta_df %>%
  #filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(avg_df_rate == max(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  arrange(year)

atp_df_worst_each_year <-atp_df %>%
  filter(year >=2003) %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(avg_df_rate == max(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  arrange(year)

atp_df_worst_each_year$server_tag <- mapply(atp_df_worst_each_year$server,
                                            FUN = abbreviated_name)
wta_df_worst_each_year$server_tag <- mapply(wta_df_worst_each_year$server,
                                            FUN = abbreviated_name)

wta_df_worst_each_year$league <- ifelse(wta_df_worst_each_year$rank <= 10, 'Ranked Top 10', 
                                        'WTA')
atp_df_worst_each_year$league <- ifelse(atp_df_worst_each_year$rank <= 10, 'Ranked Top 10', 
                                        'ATP')

all_dat_year <- rbind(wta_df_worst_each_year, 
                      atp_df_worst_each_year)


all_dat_year$league <- factor(all_dat_year$league,
                              levels = c('ATP', 'WTA', 'Ranked Top 10'))

lollipop_backbone <- ggplot() +
  
  geom_vline(xintercept = 2.01, size = 0.5,
             linetype="dashed", color = '#5DADE2') +
  geom_vline(xintercept = 3.71, size = 0.5,
             linetype="dashed", color = '#EC7063') +
  
  # -- Draws line between points
  geom_segment(
    aes(x = atp_df_worst_each_year$avg_df_rate_per10,
        xend = wta_df_worst_each_year$avg_df_rate_per10,
        y = atp_df_worst_each_year$year,
        yend = wta_df_worst_each_year$year),
    size = 0.75, color = "black"
  )



lollipop_backbone
# Add Name Labels

name_label_y_adj <- 0.38
name_label_x_adj <- 0.1

lollipop_player_names <- lollipop_backbone +
  
  # -- WTA names After 2005 (modify nudge_x)
  geom_label(data = wta_df_worst_each_year %>% filter(year >= 2005),
             aes(x= avg_df_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             fill = 'white',
             color='#A93226', 
             size=2.8,
             label.padding = unit(0.2, "lines"),
             nudge_y = name_label_y_adj,
             nudge_x = name_label_x_adj) +
  
  # -- WTA names Before 2005
  geom_label(data = wta_df_worst_each_year %>% filter(year < 2005),
             aes(x= avg_df_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             color='#A93226', 
             size=2.8,
             label.padding = unit(0.2, "lines"),
             nudge_y = name_label_y_adj,
             nudge_x = - name_label_x_adj) +
  
  #  -- Add ATP NAmes after 2005
  geom_label(data = atp_df_worst_each_year %>% filter(year >= 2005),
             aes(x= avg_df_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             color='#1A5276', 
             size=2.8, 
             label.padding = unit(0.2, "lines"),
             family='mono',
             nudge_y = name_label_y_adj,
             nudge_x = - name_label_x_adj
  ) +
  #  -- Add ATP NAmes before 2005
  geom_label(data = atp_df_worst_each_year %>% filter(year < 2005),
             aes(x= avg_df_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             color='#1A5276', 
             size=2.8, 
             label.padding = unit(0.2, "lines"),
             family='mono',
             nudge_y = name_label_y_adj,
             nudge_x =  name_label_x_adj
  ) 

lollipop_player_names
label_size = 2
label_y = 0.07
label_x = 0.1

lollipop_all_labels <- lollipop_player_names + 
  # - Add WTA numeric value
  
  geom_label(data = wta_df_worst_each_year  %>% filter(year >= 2005),
             aes(x=avg_df_rate_per10,
                 y=year , 
                 label=round(avg_df_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#EC7063',
             fontface = 'bold',
             size=label_size,
             label.padding = unit(0.1, "lines"),
             nudge_x = label_x,
             nudge_y = -label_y) +
  
  geom_label(data = wta_df_worst_each_year  %>% filter(year < 2005),
             aes(x=avg_df_rate_per10,
                 y=year, 
                 label=round(avg_df_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#EC7063',
             fontface = 'bold',
             size=label_size,
             label.padding = unit(0.1, "lines"),
             nudge_x = -label_x,
             nudge_y = -label_y) +
  
  # - Add ATP numeric value
  geom_label(data = atp_df_worst_each_year  %>% filter(year >= 2005) ,
             aes(x=avg_df_rate_per10,
                 y=year, 
                 label=round(avg_df_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#5DADE2',
             fontface = 'bold',
             size=label_size,
             label.padding = unit(0.1, "lines"),
             nudge_x = -label_x,
             nudge_y = -label_y) + 
  
  # - Add ATP numeric value
  geom_label(data = atp_df_worst_each_year  %>% filter(year < 2005),
             aes(x=avg_df_rate_per10,
                 y=year, 
                 label=round(avg_df_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#5DADE2',
             fontface = 'bold',
             size=label_size,
             label.padding = unit(0.1, "lines"),
             nudge_x = label_x,
             nudge_y = -label_y)  

lollipop_all_labels
# --- == -- == --- == -- == --- == -- == --- == -- == --- #
# --- == -- == --- == -- == --- == -- == --- == -- == --- #

x_level <- c(" Double Faults", "(per 10 Second Serves)")
x_axis_lab <- x_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")


double_fault_final_plot <- lollipop_all_labels +
  # -- Draws ATP & WTA points
  geom_point(
    data = all_dat_year,
    aes(x = avg_df_rate_per10, 
        y = year, 
        fill = as.factor(league)), 
    shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size =3.5)  +
  
  scale_fill_manual(values = c('#5DADE2', '#EC7063', '#ffdd1a'), 
                    c('ATP', 'WTA', 'Top 10')) +
  
  scale_y_continuous(name= 'Year',
                     seq(2003,2019,2)) + 
  scale_x_continuous(name = 'Double Fault Rate (per 10 Second Serves)',
                     seq(1,4,0.5)) +
  
  # -- Axis labels
  labs(#x = x_axis_lab,
    x = 'Double Fault Rate (per 10 Second Serves)',
    y = 'Year',
    title =  "ATP & WTA Worst Double Faulters By Year (Ranked Top 50)",
    caption="StatsOnTheT") +
  
  # -- Axis limits
  #ylim(c(2002.8,2019.5))   + 
  #xlim(c(0, 5)) 
  
  
  # -- Add custom theme
  peter_year_theme()



tiff("double_faults.tiff", units="px", width=950, height=720, res=120)
double_fault_final_plot
dev.off()

# ACES -----------------------------------------------
##### -- ACES ANALYSIS

wta_ace_best_each_year <- wta_df %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(avg_ace_rate == max(avg_ace_rate)) %>%
  mutate(avg_ace_rate_per10 = avg_ace_rate*10) %>%
  arrange(year,avg_ace_rate_per10)

atp_ace_best_each_year <-atp_df %>%
  filter(year >=2003) %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(avg_ace_rate == max(avg_ace_rate)) %>%
  mutate(avg_ace_rate_per10 = avg_ace_rate*10) %>%
  arrange(year)

atp_ace_best_each_year$server_tag <- mapply(atp_ace_best_each_year$server,
                                            FUN = abbreviated_name)
wta_ace_best_each_year$server_tag <- mapply(wta_ace_best_each_year$server,
                                            FUN = abbreviated_name)


wta_ace_best_each_year$league <-ifelse(wta_ace_best_each_year$rank <= 10, 'Ranked Top 10', 
                                       'WTA')

atp_ace_best_each_year$league <- ifelse(atp_ace_best_each_year$rank <= 10, 'Ranked Top 10', 
                                         'ATP')


all_dat_year_ace <- rbind(wta_ace_best_each_year, 
                          atp_ace_best_each_year)

all_dat_year_ace$league <- factor(all_dat_year_ace$league,
                                     levels = c('ATP', 'WTA', 'Ranked Top 10'))

#Plotting aces


lollipop_backbone_ace <- ggplot() +
  
  geom_vline(xintercept = 2.69, size = 0.5,
             linetype="dashed", color = '#5DADE2') +
  geom_vline(xintercept = 1.58, size = 0.5,
             linetype="dashed", color = '#EC7063') +
  
  # -- Draws line between points
  geom_segment(
    aes(x = atp_ace_best_each_year$avg_ace_rate_per10,
        xend = wta_ace_best_each_year$avg_ace_rate_per10,
        y = atp_ace_best_each_year$year,
        yend = wta_ace_best_each_year$year),
    size = 0.75, color = "black"
  ) 

lollipop_backbone_ace


# Add Name Labels

name_label_y_adj <- 0.36
name_label_x_adj <- 0.11

lollipop_player_names_ace <- lollipop_backbone_ace +
  
  # -- WTA names After 2005 (modify nudge_x)
  geom_label(data = wta_ace_best_each_year,
             aes(x= avg_ace_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             fill = 'white',
             color='#A93226', 
             size=2.8,
             nudge_y = name_label_y_adj,
             nudge_x = - name_label_x_adj) +
  
  
  #  -- Add ATP NAmes after 2005
  geom_label(data = atp_ace_best_each_year,
             aes(x= avg_ace_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             color='#1A5276', 
             size=2.8, 
             family='mono',
             nudge_y = name_label_y_adj,
             nudge_x =  name_label_x_adj
  ) 

lollipop_player_names_ace



lollipop_all_labels_ace <- lollipop_player_names_ace + 
  # - Add WTA numeric value
  
  geom_label(data = wta_ace_best_each_year,
             aes(x=avg_ace_rate_per10,
                 y=year , 
                 label=round(avg_ace_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#EC7063',
             fontface = 'bold',
             size=2,
             label.padding = unit(0.12, "lines"),
             nudge_x = -0.07,
             nudge_y = -0.1) +
  
  
  # - Add ATP numeric value
  geom_label(data = atp_ace_best_each_year,
             aes(x=avg_ace_rate_per10,
                 y=year, 
                 label=round(avg_ace_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#5DADE2',
             fontface = 'bold',
             size=2,
             label.padding = unit(0.12, "lines"),
             nudge_x = 0.07,
             nudge_y = -0.1) 



lollipop_all_labels_ace

x_level <- c(" Aces Hit", "(per 10 Serves)")
x_axis_lab <- x_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")



lollipop_all_labels_ace +
 
   # -- Draws ATP & WTA points
  geom_point(
    data = all_dat_year_ace,
    aes(x = avg_ace_rate_per10, 
        y = year, 
        fill = as.factor(league)), 
    shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size = 3.5) +

  
  scale_fill_manual(values = c('#5DADE2', '#EC7063', '#ffdd1a'), 
                    c('ATP', 'WTA', 'Top 10')) +
  
  scale_y_continuous(name= 'Year',
                     seq(2003,2019,2)) + 
  scale_x_continuous(name = 'Ace Rate (per 10 Serves)',
                     seq(0.4,2.8,0.5)) +
  
  # -- Axis labels
  labs(#x = x_axis_lab,
    x = 'Ace Rate (per 10 Second Serves)',
    y = 'Year',
    title =  "ATP & WTA Best Aces By Year (Ranked Top 50)",
    caption="StatsOnTheT") +
  
  # -- Axis limits
  #ylim(c(2002.8,2019.5))   + 
  #xlim(c(0, 5)) 
  
  
  # -- Add custom theme
  peter_year_theme()





# Ace To Double Fault Ratio -----------------------------------------------
##### Aces to Double Faults Analysis

# -- Who are the best?
wta_df %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  arrange(desc(ace_df_ratio)) %>%
  View()

atp_df %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  arrange(desc(ace_df_ratio)) %>%
  View()
  


wta_ace_df_ratio_best_each_year <- wta_df %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(ace_df_ratio == max(ace_df_ratio)) %>%
  arrange(year)

atp_ace_df_ratio_best_each_year <- atp_df %>%
  filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  filter(year >= 2003) %>%
  group_by(year) %>%
  filter(ace_df_ratio == max(ace_df_ratio)) %>%
  arrange(year)


atp_ace_df_ratio_best_each_year$server_tag <- mapply(atp_ace_df_ratio_best_each_year$server,
                                                     FUN = abbreviated_name)
wta_ace_df_ratio_best_each_year$server_tag <- mapply(wta_ace_df_ratio_best_each_year$server,
                                                     FUN = abbreviated_name)


wta_ace_df_ratio_best_each_year$league <-ifelse(wta_ace_df_ratio_best_each_year$rank <= 10,
                                                'Ranked Top 10', 
                                                'WTA')

atp_ace_df_ratio_best_each_year$league <- ifelse(atp_ace_df_ratio_best_each_year$rank <= 10,
                                                 'Ranked Top 10', 
                                                 'ATP')


all_dat_year_ratio <- rbind(wta_ace_df_ratio_best_each_year, 
                          atp_ace_df_ratio_best_each_year)


all_dat_year_ratio$league <- factor(all_dat_year_ratio$league,
                                    levels = c('ATP', 'WTA', 'Ranked Top 10'))
# -- PLOT Ratio

lollipop_backbone_ratio <- ggplot() +
  
  geom_vline(xintercept = 12.54, size = 0.5,
             linetype="dashed", color = '#5DADE2') +
  geom_vline(xintercept = 4.02, size = 0.5,
             linetype="dashed", color = '#EC7063') +
  
  # -- Draws line between points
  geom_segment(
    aes(x = atp_ace_df_ratio_best_each_year$ace_df_ratio,
        xend = wta_ace_df_ratio_best_each_year$ace_df_ratio,
        y = atp_ace_df_ratio_best_each_year$year,
        yend = wta_ace_df_ratio_best_each_year$year),
    size = 0.75, color = "black"
  ) 



lollipop_backbone_ratio


# Add Name Labels

name_label_y_adj <- 0.38

lollipop_player_names_ratio <- lollipop_backbone_ratio +
  
  # -- WTA names After 2005 (modify nudge_x)
  geom_label(data = wta_ace_df_ratio_best_each_year,
             aes(x= ace_df_ratio, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             fill = 'white',
             color='#A93226', 
             size=2.8,
             label.padding = unit(0.2, "lines"),
             nudge_y = name_label_y_adj,
             nudge_x = - 0.1) +
  
  
  #  -- Add ATP NAmes after 2005
  geom_label(data = atp_ace_df_ratio_best_each_year,
             aes(x= ace_df_ratio, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             color='#1A5276', 
             size=2.8, 
             label.padding = unit(0.2, "lines"),
             family='mono',
             nudge_y = name_label_y_adj,
             nudge_x =  0.1
  ) 

lollipop_player_names_ratio



lollipop_all_labels_ratio <- lollipop_player_names_ratio + 
  # - Add WTA numeric value
  
  geom_label(data = wta_ace_df_ratio_best_each_year,
             aes(x=ace_df_ratio,
                 y=year , 
                 label=round(ace_df_ratio, 2)),
             family='mono',
             color= 'black',
             fill = '#EC7063',
             fontface = 'bold',
             size=2,
             label.padding = unit(0.13, "lines"),
             nudge_x = -0.38,
             nudge_y = -0.1) +
  
  
  # - Add ATP numeric value
  geom_label(data = atp_ace_df_ratio_best_each_year,
             aes(x=ace_df_ratio,
                 y=year, 
                 label=round(ace_df_ratio, 2)),
             family='mono',
             color= 'black',
             fill = '#5DADE2',
             fontface = 'bold',
             size=2,
             label.padding = unit(0.13, "lines"),
             nudge_x = 0.38,
             nudge_y = -0.1) 



lollipop_all_labels_ratio
#---|||

x_level <- c("Ace to Double Fault", "       Ratio")
x_axis_lab <- x_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")


ratio_final_plot <- lollipop_all_labels_ratio +
  
  # -- Draws ATP & WTA points
  geom_point(
    data = all_dat_year_ratio,
    aes(x = ace_df_ratio, 
        y = year, 
        fill = as.factor(league)), 
    shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size = 3.5) +
  
  scale_fill_manual(values = c('#5DADE2', '#EC7063', '#ffdd1a'), 
                    c('ATP', 'WTA', 'Top 10')) +
  
  
  scale_y_continuous(name= 'Year',
                     seq(2003,2019,2)) + 
  scale_x_continuous(name = x_axis_lab,
                     seq(0.5,13,2)) +
  
  # -- Axis labels
  labs(#x = x_axis_lab,
    x = 'Ace Rate (per 10 Second Serves)',
    y = 'Year',
    title =  "ATP & WTA Best Servers By Year (Ranked Top 50)",
    caption="StatsOnTheT") +
  
  # -- Axis limits
  #ylim(c(2002.8,2019.5))   + 
  #xlim(c(0, 5)) 
  
  
  # -- Add custom theme
  peter_year_theme()


tiff("ratio.tiff", units="px", width=880, height=750, res=120)
ratio_final_plot
dev.off()

ggsave(filename = "survival-curves.png",
       plot = ratio_final_plot)



##### UPDATED DOUBLE FAULT PLOT -----
# HAD PREVIOUSLY FILTERED TO ONLY INCLUDE PLAYERS WITH
# AT LEAST 20 MATCHES. THIS DOESN;T WORK IN 2003, 2004 WTA

wta_df_worst_each_year <- wta_df %>%
  #filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(avg_df_rate == max(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  arrange(year)

atp_df_worst_each_year <-atp_df %>%
  filter(year >=2003) %>%
  #filter(matches_played >= 20) %>%
  filter(rank <= 50) %>%
  group_by(year) %>%
  filter(avg_df_rate == max(avg_df_rate)) %>%
  mutate(avg_df_rate_per10 = avg_df_rate*10) %>%
  arrange(year)

atp_df_worst_each_year$server_tag <- mapply(atp_df_worst_each_year$server,
                                            FUN = abbreviated_name)
wta_df_worst_each_year$server_tag <- mapply(wta_df_worst_each_year$server,
                                            FUN = abbreviated_name)

wta_df_worst_each_year$league <- ifelse(wta_df_worst_each_year$rank <= 10, 'Ranked Top 10', 
                                        'WTA')
atp_df_worst_each_year$league <- ifelse(atp_df_worst_each_year$rank <= 10, 'Ranked Top 10', 
                                        'ATP')

all_dat_year <- rbind(wta_df_worst_each_year, 
                      atp_df_worst_each_year)


all_dat_year$league <- factor(all_dat_year$league,
                              levels = c('ATP', 'WTA', 'Ranked Top 10'))

lollipop_backbone <- ggplot() +
  
  geom_vline(xintercept = 2.01, size = 0.5,
             linetype="dashed", color = '#5DADE2') +
  geom_vline(xintercept = 3.71, size = 0.5,
             linetype="dashed", color = '#EC7063') +
  
  # -- Draws line between points
  geom_segment(
    aes(x = atp_df_worst_each_year$avg_df_rate_per10,
        xend = wta_df_worst_each_year$avg_df_rate_per10,
        y = atp_df_worst_each_year$year,
        yend = wta_df_worst_each_year$year),
    size = 0.75, color = "black"
  )



lollipop_backbone
# Add Name Labels

name_label_y_adj <- 0.38
name_label_x_adj <- 0.09

lollipop_player_names <- lollipop_backbone +
  
  # -- WTA names After 2003 (modify nudge_x)
  geom_label(data = wta_df_worst_each_year %>% filter(year >= 2003),
             aes(x= avg_df_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             fill = 'white',
             color='#A93226', 
             size=2.8,
             label.padding = unit(0.2, "lines"),
             nudge_y = name_label_y_adj,
             nudge_x = name_label_x_adj) +
  

  
  #  -- Add ATP NAmes after 2003
  geom_label(data = atp_df_worst_each_year %>% filter(year >= 2003),
             aes(x= avg_df_rate_per10, 
                 y=year, 
                 label=server_tag,
                 fontface = ifelse(rank <=10, 2, 1),
                 family='mono'),
             color='#1A5276', 
             size=2.8, 
             label.padding = unit(0.2, "lines"),
             family='mono',
             nudge_y = name_label_y_adj,
             nudge_x = - name_label_x_adj
  )

lollipop_player_names

label_size = 2
label_y = 0.07
label_x = 0.1

lollipop_all_labels <- lollipop_player_names + 
  # - Add WTA numeric value
  
  geom_label(data = wta_df_worst_each_year  %>% filter(year >= 2003),
             aes(x=avg_df_rate_per10,
                 y=year , 
                 label=round(avg_df_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#EC7063',
             fontface = 'bold',
             size=label_size,
             label.padding = unit(0.1, "lines"),
             nudge_x = label_x,
             nudge_y = -label_y) +

  # - Add ATP numeric value
  geom_label(data = atp_df_worst_each_year  %>% filter(year >= 2003) ,
             aes(x=avg_df_rate_per10,
                 y=year, 
                 label=round(avg_df_rate_per10, 2)),
             family='mono',
             color= 'black',
             fill = '#5DADE2',
             fontface = 'bold',
             size=label_size,
             label.padding = unit(0.1, "lines"),
             nudge_x = -label_x,
             nudge_y = -label_y) 

lollipop_all_labels
# --- == -- == --- == -- == --- == -- == --- == -- == --- #
# --- == -- == --- == -- == --- == -- == --- == -- == --- #

x_level <- c(" Double Faults", "(per 10 Second Serves)")
x_axis_lab <- x_level %>%
  stringr::str_pad(width = max(stringr::str_length(.)), 
                   side = "right") %>% 
  stringr::str_c(collapse = "\n")


double_fault_final_plot <- lollipop_all_labels +
  # -- Draws ATP & WTA points
  geom_point(
    data = all_dat_year,
    aes(x = avg_df_rate_per10, 
        y = year, 
        fill = as.factor(league)), 
    shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size =3.5)  +
  
  scale_fill_manual(values = c('#5DADE2', '#EC7063', '#ffdd1a'), 
                    c('ATP', 'WTA', 'Top 10')) +
  
  scale_y_continuous(name= 'Year',
                     seq(2003,2019,2)) + 
  scale_x_continuous(name = 'Double Fault Rate (per 10 Second Serves)',
                     seq(1,4,0.5)) +
  
  # -- Axis labels
  labs(#x = x_axis_lab,
    x = 'Double Fault Rate (per 10 Second Serves)',
    y = 'Year',
    title =  "ATP & WTA Worst Double Faulters By Year (Ranked Top 50)",
    caption="StatsOnTheT") +
  
  # -- Axis limits
  #ylim(c(2002.8,2019.5))   + 
  #xlim(c(0, 5)) 
  
  
  # -- Add custom theme
  peter_year_theme()

double_fault_final_plot
