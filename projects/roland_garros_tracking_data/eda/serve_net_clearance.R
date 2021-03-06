### || ### || ### || ### || ### || ### || ###
##### Plot serve net clearance #####
### || ### || ### || ### || ### || ### || ###

library(dplyr)
library(ggplot2)

setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/eda/")

atp_pbp_df <- read.csv('atp_pbp_df.csv')


ggplot(data = atp_pbp_df, 
       aes(x=z_net_serve, fill = as.factor(serve_num))) +
  geom_density( aes(y = ..density.., fill=as.factor(serve_num)), alpha = 0.6) + 
  labs(fill="Serve Number")

atp_pbp_df$z_net_serve  
  



