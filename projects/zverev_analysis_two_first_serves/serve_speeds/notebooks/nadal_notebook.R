# ----- ##### ----- ##### -----  ##### -----  ##### -----  ##### 
# ----- ##### ----- ##### -----  ##### -----  ##### -----  ##### 
# Nadal Notebook
# ----- ##### ----- ##### -----  ##### -----  ##### -----  ##### 
# ----- ##### ----- ##### -----  ##### -----  ##### -----  ##### 

# load libraries
library(dplyr)
library(ggplot2)
library(reshape2)

### Get Sackmann Data

source("~/Documents/tennis/zverev_analysis/serve_speeds/src/collect_speed_data_functions.R")


# collect data from grand slams 2018 - 2019
nadal_grand_slam_data_18_19 = collect_all_grand_slam_18_19('Rafael Nadal')



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
#### DOUBLE FAULTING ANALYSIS                                ####
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# -->Add grouped score column
source("~/Documents/tennis/zverev_analysis/serve_speeds/src/score_function.R")

nadal_grand_slam_data_18_19<- nadal_grand_slam_data_18_19 %>%
  mutate(grouped_score = get_score_linkedin(current_score))

# check if we missed some scores
nadal_grand_slam_data_18_19 %>%
  filter(grouped_score == 'ERROR')


source("~/Documents/tennis/zverev_analysis/serve_speeds/src/double_faults.R")

nadal_double_faults <- player_doublefault_proportions(nadal_grand_slam_data_18_19)


plot_double_fault_proportions(title_name = 'Recent Nadal Grand Slam \nDouble Faults',
                   nadal_double_faults )
