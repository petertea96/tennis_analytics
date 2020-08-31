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
source("~/Documents/Github/serve_speeds/src/collect_speed_data_functions.R")

# collect data from grand slams 2018 - 2019
zverev_grand_slam_data_18 = collect_all_grand_slam_data('Alexander Zverev',
                                                        year = 2018)
zverev_grand_slam_data_19 = collect_all_grand_slam_data('Alexander Zverev',
                                                        year = 2019)

zverev_grand_slam_data_18_19 <- rbind(zverev_grand_slam_data_18,
                                      zverev_grand_slam_data_19)

# Check who Zverev played against:
table(zverev_grand_slam_data_18_19$returner)
# --> Previously we had some errors where Zverev was playing against himself..

# --> How many matches were played each year?
zverev_grand_slam_data_18_19 %>%
  group_by(year) %>%
  summarize( num_matches = length(unique(match_id)))

# --> Average serve speeds each year?
zverev_grand_slam_data_18_19  %>%
  group_by(year, ServeNumber) %>%
  summarise(speed = mean(Speed_KMH))

#--> Calculate proportion of success (wins) in 1st service games vs 2nd service games
source("~/Documents/Github/serve_speeds/src/summary_stats.R")
serve_success_rates(zverev_grand_slam_data_18_19)

# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####              DOUBLE FAULTING ANALYSIS                 ####             
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
# --> Add grouped score column
source("~/Documents/Github/serve_speeds/src/grouped_score.R")

zverev_grand_slam_data_18_19$grouped_score <- mapply(zverev_grand_slam_data_18_19$server_score,
       zverev_grand_slam_data_18_19$returner_score,
       FUN = get_grouped_score)

# check if we missed some scores
zverev_grand_slam_data_18_19 %>%
  filter(is.null(grouped_score))

# Check how many points Zverev played in each situation
tosee=zverev_grand_slam_data_18_19%>%
  group_by(year, grouped_score) %>%
  summarise(points_played = n())


# --> Plot proportion of double faults
source("~/Documents/Github/serve_speeds/src/serve_rates.R")

# Get data on proportion of aces and double faults for each 6 serve situations
zverev_double_faults_data <- prepare_serve_rates_data(zverev_grand_slam_data_18_19)

print(zverev_double_faults_data)

#tiff("zverev_df_rate.tiff",width=556, height=471, res = 105)
plot_serve_rates(zverev_double_faults_data)
#dev.off()

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


zverev_aces_data <- prepare_serve_rates_data(zverev_grand_slam_data_18_19, 
                                             df_flag = FALSE)
print(zverev_aces_data)

#tiff("zverev_ace_rate.tiff",width=556, height=471, res = 105)
plot_serve_rates(zverev_aces_data, 
                 df_flag = FALSE)
#dev.off()



# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
####          AUSTRALIAN OPEN SERVE SPEEDS                 ####
# ----- # ----- # -----  # -----  # -----  #  -----  # -----  # 
source("~/Documents/Github/serve_speeds/src/boxplot_serve_speeds.R")

# How are the sample sizes for each category?
zverev_ao_data <- zverev_grand_slam_data_18_19 %>%
  filter(tournament == 'ausopen')

zverev_ao_data %>%
  group_by(ServeNumber,year) %>%
  summarise(avg_speed = mean(Speed_KMH),
            n = n(),
            df = sum(server_df),
            ace = sum(server_ace))

#Where did his aces on 2nd serve come from?
zverev_ao_data %>%
  filter(ServeNumber == 2) %>%
  group_by(grouped_score, year) %>%
  summarise(aces = sum(server_ace))


serve_levels <- c('Break Point', 'Game Point', 'First Point', 'Behind','Ahead', 'Even')

zverev_ao_data <- zverev_ao_data %>%
  mutate(grouped_score = factor(grouped_score, 
                                levels =serve_levels ))

#tiff("zverev_speed_boxplot.tiff",width=556, height=450, res = 115)
plot_speed_boxplot(zverev_ao_data,
                   title = 'Zverev\'s Australian Open Serve \nSpeed Distributions (2018 - 2019)')
#dev.off()  



# -- Save Zverev data for shiny app
#write.csv(zverev_grand_slam_data_18_19, 
#          'zverev_grand_slams_18_19.csv',
#          row.names = FALSE)







