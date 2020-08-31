# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
#                      Zverev Notebook
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 

# load libraries
library(dplyr)
library(ggplot2)
library(reshape2)

# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####                GET THE CLEANED DATA                   ####             
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
#--> Get Sackmann speed and play-by-play data
source("~/Documents/tennis/zverev_analysis/serve_speeds/src/collect_speed_data_functions.R")

# collect data from grand slams 2018 - 2019
zverev_grand_slam_data_18_19 = collect_all_grand_slam_data('Alexander Zverev')

# Check who Zverev played against:
table(zverev_grand_slam_data_18_19$returner)
# --> Previously we had some errors where Zverev was playing against himself..

# --> How many matches were played?
zverev_grand_slam_data_18_19 %>%
  group_by(year) %>%
  summarize( num_matches = length(unique(match_id)))

# --> Average serve speeds
zverev_grand_slam_data_18_19  %>%
  group_by(year, ServeNumber) %>%
  summarise(speed = mean(Speed_KMH))


#--> Calculate proportion of success (wins) in 1st service games vs 2nd service games
source("~/Documents/tennis/zverev_analysis/serve_speeds/src/analysis_function.R")
serve_success_rates(zverev_grand_slam_data_18_19)

# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####              DOUBLE FAULTING ANALYSIS                 ####             
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
# --> Add grouped score column
source("~/Documents/tennis/zverev_analysis/serve_speeds/src/score_function.R")

zverev_grand_slam_data_18_19<- zverev_grand_slam_data_18_19 %>%
  mutate(grouped_score = get_score_linkedin(current_score))

# check if we missed some scores
zverev_grand_slam_data_18_19 %>%
  filter(grouped_score == 'ERROR')

# Calculate df and ace rates
calc_serve_rates(zverev_grand_slam_data_18_19) %>%
  View()  


# --> Plot proportion of double faults
source("~/Documents/tennis/zverev_analysis/serve_speeds/src/serves.R")

# Get data on proportion of aces and double faults for each 6 serve situations
zverev_double_faults_data <- serve_result_proportions_data(grand_slam_data = zverev_grand_slam_data_18_19,
                                                           double_fault_flag = TRUE)
print(zverev_double_faults_data)

tiff("zverev_df_rate.tiff",width=556, height=471, res = 105)
plot_serve_proportions(title_name = 'Zverev\'s Recent Grand Slams \nDouble Fault Rates',
                       my_data = zverev_double_faults_data)
dev.off()
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####                ACES ANALYSIS                          ####
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
# should we look @ aces on 1st serve vs. on 2nd serve???
# check how many aces he has on 2nd serve...

zverev_grand_slam_data_18_19 %>%
  filter(server_ace==1) %>%
  group_by(ServeNumber) %>%
  summarise(n = n())
# --> 9 aces on 2nd serve vs. 380 on 1st


zverev_aces_data <- serve_result_proportions_data(grand_slam_data = zverev_grand_slam_data_18_19,
                                                  double_fault_flag = FALSE)
print(zverev_aces_data)

tiff("zverev_ace_rate.tiff",width=556, height=471, res = 105)
plot_serve_proportions(title_name = 'Zverev\'s Recent Grand Slams \nAce Rates',
                       my_data = zverev_aces_data, 
                       y_upper_limit = 0.2,
                       bg_colour = '#FFCCCC',
                         #'#fdaaaa',
                       y_axis_title = 'Ace Rate' 
                      )
dev.off()
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####          AUSTRALIAN OPEN SERVE SPEEDS                 ####
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
# How are the sample sizes for each category?

zverev_ao_data <- zverev_grand_slam_data_18_19 %>%
  filter(tournament == 'ausopen')

zverev_ao_data %>%
  group_by(ServeNumber,year) %>%
  summarise(avg_speed = mean(Speed_KMH),
            n = n(),
            df = sum(server_df),
            ace = sum(server_ace))



plot_jitter_dfs <- function(dataframe, cur_year, point_type){
  geom_jitter(data = dataframe %>% 
                filter(year == cur_year, 
                       grouped_score == point_type) %>%
                filter(server_df == 1), 
              aes(x = as.factor(year), y = Speed_KMH),
              position= position_nudge(x = 0.225),
              color = 'blue',
              fill = 'black',
              alpha = 0.25) 
}

plot_jitter_aces <- function(dataframe, cur_year, point_type, 
                             first_serve = TRUE, nudge_x =  -0.225){

  if(first_serve){
    dataframe <- dataframe %>%
      filter(ServeNumber == 1)
  } else{
    dataframe <- dataframe %>%
      filter(ServeNumber == 2)
    nudge_x = 0.225
    
  }
  geom_jitter(data = dataframe %>% 
                filter(year == cur_year, 
                       grouped_score == point_type) %>%
                filter(server_ace == 1), 
              aes(x = as.factor(year), y = Speed_KMH),
              position= position_nudge(x = nudge_x),
              color = 'red',
              fill = 'black',
              alpha = 0.25
  ) 
}

zverev_ao_data %>%
  filter(ServeNumber == 2) %>%
  group_by(grouped_score, year) %>%
  summarise(aces = sum(server_ace))


serve_levels <- c('Break Point', 'Game Point', 'First Point', 'Behind','Ahead', 'Even')

zverev_ao_data <- zverev_ao_data %>%
  mutate(grouped_score = factor(grouped_score, 
                                levels =serve_levels ))

tiff("zverev_speed_boxplot.tiff",width=556, height=450, res = 115)
ggplot(zverev_ao_data, aes(x = as.factor(year), y = Speed_KMH)) + 
  geom_boxplot(aes(fill = as.factor(ServeNumber)),
               position = position_dodge(0.9),
               alpha = 0.65,
               color = '#6c7a86',
               outlier.shape = NA
               ) +
  #facet_grid(rows = vars(grouped_score)) +
  facet_wrap(~ grouped_score, ncol = 3) + 
  ylim(c(130, 225)) + 
  
  plot_jitter_aces(zverev_ao_data, 2018, 'Ahead') + 
  plot_jitter_aces(zverev_ao_data, 2018, 'Break Point') + 
  plot_jitter_aces(zverev_ao_data, 2018, 'Behind') +
  plot_jitter_aces(zverev_ao_data, 2018, 'Even') +
  plot_jitter_aces(zverev_ao_data, 2018, 'First Point') +
  plot_jitter_aces(zverev_ao_data, 2018, 'Game Point')  + 
  plot_jitter_aces(zverev_ao_data, 2019, 'Ahead') + 
  plot_jitter_aces(zverev_ao_data, 2019, 'Break Point') + 
  plot_jitter_aces(zverev_ao_data, 2019, 'Behind') +
  plot_jitter_aces(zverev_ao_data, 2019, 'Even') +
  plot_jitter_aces(zverev_ao_data, 2019, 'First Point') +
  plot_jitter_aces(zverev_ao_data, 2019, 'Game Point')  +
  plot_jitter_dfs(zverev_ao_data, 2018, 'Ahead') +
  plot_jitter_dfs(zverev_ao_data, 2018, 'Break Point') + 
  plot_jitter_dfs(zverev_ao_data, 2018, 'Behind') +
  plot_jitter_dfs(zverev_ao_data, 2018, 'Even') +
  plot_jitter_dfs(zverev_ao_data, 2018, 'First Point') +
  plot_jitter_dfs(zverev_ao_data, 2018, 'Game Point') +
  plot_jitter_dfs(zverev_ao_data, 2019, 'Ahead') +
  plot_jitter_dfs(zverev_ao_data, 2019, 'Break Point') + 
  plot_jitter_dfs(zverev_ao_data, 2019, 'Behind') +
  plot_jitter_dfs(zverev_ao_data, 2019, 'Even') +
  plot_jitter_dfs(zverev_ao_data, 2019, 'First Point') +
  plot_jitter_dfs(zverev_ao_data, 2019, 'Game Point') + 
  
  # Add aces on 2nd serve
  plot_jitter_aces(zverev_ao_data, 2019, 'Ahead', first_serve = FALSE) +
  plot_jitter_aces(zverev_ao_data, 2018, 'Game Point', first_serve = FALSE) +
  plot_jitter_aces(zverev_ao_data, 2019, 'Game Point', first_serve = FALSE) +
  geom_hline(yintercept = 167,
             linetype = 'dashed',
             size = 0.5,
             color = '#6c7a86',
             alpha = 0.7) +
  ggtitle('Zverev\'s Australian Open Serve \nSpeed Distributions (2018 - 2019)') + 
  ylab("Serve Speed (KM/H)") + xlab("Year") + labs(fill = "Serve Number:" ) +
  theme_bw() +
  scale_fill_manual(values=c("#FA8072", "#AFEEEE"), labels = c("1st", "2nd") ) +  
  theme(panel.background = element_rect(fill = 'white', # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1.1),
                                face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title =  element_text(face = "bold", size = 9),
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 9),
        # --> Change facet wrap background colour
        strip.background = element_rect(fill="#DEFFDE", color = 'black')
        )
dev.off()  

  
  
  

  

  


  