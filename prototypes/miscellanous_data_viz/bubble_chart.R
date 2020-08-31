# Date: December 24, 2019
# Author: Peter Tea
# Purpose: Create bubble chart plotting break points won and service games won
# per game. Hopefully we see the major players as outliers...

library(dplyr)
library(ggplot2)
# % service games won = serve games won/ total serve games
# %breaks = breaks/total opponent serve games

the_url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2019.csv"
Sackmann_Table <- read.csv(the_url, quote = "", 
                           row.names = NULL, 
                           stringsAsFactors = FALSE)

#Subet only Grand Slam matches
ATP_matches_2019 <- Sackmann_Table %>% 
  filter(tourney_level == "G")


### Create data frame with player name, # matches won, % service games won, % break points won
#   as columns


# Get vector of all player names that participated in grand slams
ATP_2019_GS_Names <- union(ATP_matches_2019$winner_name, ATP_matches_2019$loser_name)


# Initialize list
bubble_list <- vector(mode="list", length=length(ATP_2019_GS_Names))
names(bubble_list) <- ATP_2019_GS_Names

###############################################################################
# For each player, calculate the following:
# --> total number matches played
# --> total player service games (i.e. number games where player served)
# --> total number service games won
# --> total number break points won 
# --> total number of opponent service games played



for(tag in 1:length(ATP_2019_GS_Names)){
  
  name <- ATP_2019_GS_Names[tag]
  
  #Note: dat1 filters data where the player is the winner of the match
  #      dat2 filters data where the player is the loser of the match
  
  dat1 <- ATP_matches_2019 %>%
    filter(winner_name == name) %>%

    # Number Service games won = Total player service games - # times player was broken
    # Number of break points = Number opponent break points faced - Number of opponent break points saved
    mutate(Service_Games_Won = w_SvGms - w_bpFaced + w_bpSaved,
           Break_points_Won = l_bpFaced - l_bpSaved) %>%
    
    summarise(n = n(),
              T_Service_Games = sum(w_SvGms, na.rm = T),
              T_Service_Games_Won = sum(Service_Games_Won, na.rm = T),
              T_Break_points_Won = sum(Break_points_Won, na.rm = T),
              T_opp_Service_Games = sum(l_SvGms, na.rm = T))
  
  dat2 <- ATP_matches_2019 %>%
    filter(loser_name == name) %>%
    mutate(Service_Games_Won = l_SvGms - l_bpFaced + l_bpSaved,
           Break_points_Won = w_bpFaced - w_bpSaved) %>%
    summarise(n = n(), 
              T_Service_Games = sum(l_SvGms, na.rm = T),
              T_Service_Games_Won = sum(Service_Games_Won, na.rm = T),
              T_Break_points_Won = sum(Break_points_Won, na.rm = T),
              T_opp_Service_Games = sum(w_SvGms, na.rm = T))
  
  dat <- rbind(dat1, dat2)
  bubble_list[[tag]] <- dat
  
}

# Now create a data frame with the following 4 columns:
# --> Player name
# --> Total number matches played
# --> Proportion of service games won
# --> Proportion of opponent service games broken

to_plot_dat <- data.frame(matrix(nrow = length(ATP_2019_GS_Names), ncol = 4))
to_plot_dat[,1] <- ATP_2019_GS_Names


for (index in 1:length(bubble_list)){
  player_dat <- bubble_list[[index]] %>%
    summarise_each(sum) %>%
    mutate(Prop_Service_Games_Won = T_Service_Games_Won/T_Service_Games ,
           Prop_Break_points_Won = T_Break_points_Won/T_opp_Service_Games)
  
  to_plot_dat[index,2] <- player_dat$n
  to_plot_dat[index,3] <- player_dat$Prop_Service_Games_Won
  to_plot_dat[index,4] <- player_dat$Prop_Break_points_Won
}

colnames(to_plot_dat) <- c("Player_Name", "Matches", "SvGmsW_Prop", "BPW_Prop")

# Aside: Add a new column that extracts the last name of each player
get.last.name <- function(name) {
  d <- lapply(ifelse(grepl(",",name),strsplit(name,","),strsplit(name," ")),`[[`,2)
  lapply(strsplit(gsub("^ ","",d), " "),`[[`,1)
}

#Add in Matches won as well
to_plot_dat <- to_plot_dat %>%
  left_join(
    ATP_matches_2019 %>%
      group_by(winner_name) %>%
      dplyr::summarise(Matches_Won=n()),
   by = c('Player_Name' = 'winner_name') 
  ) %>% 
  mutate(Matches_Won = ifelse(is.na(Matches_Won), 0, Matches_Won),
         SvGmsW_Prop = as.numeric(SvGmsW_Prop),
         BPW_Prop = as.numeric(BPW_Prop),
         Last_Name = get.last.name(Player_Name)
         ) %>%
  arrange(desc(Matches))




# create bubble plot
p <- ggplot(to_plot_dat[-c(1,2,3),], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won, label = Player_Name)) + 
  geom_point(alpha = 0.8, color = "#008080") +
  
  #Add labels for Djoker, Roger and Rafa
  #geom_text_repel(aes(label=ifelse(SvGmsW_Prop > 0.886486 & BPW_Prop > 0.2635869 , 
  #                                 as.character(Last_Name),'')
  #), size = 4.0, nudge_y = -4) +
  
  geom_text(data =  to_plot_dat %>% filter(Last_Name == "Djokovic"), 
            aes(label=as.character(Last_Name)), size = 4, color = "blue",
            nudge_x = -6) +
  
  geom_text(data =  to_plot_dat %>% filter(Last_Name == "Nadal"), 
            aes(label=as.character(Last_Name)), size = 4, color = "red",
            nudge_y = -3) +
  geom_text(data =  to_plot_dat %>% filter(Last_Name == "Federer"), 
            aes(label=as.character(Last_Name)), size = 4, color = "forestgreen",
            nudge_y = 3) +
  
  #Adjust size of bubble points and legend title
  scale_size(range = c(.1, 8), name="Matches Won") +
  
  #Colour Nadal red
  geom_point(data=to_plot_dat[1, ], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won),
             colour="red", alpha = 0.7) +
  
  #Colour Federer in Green
  geom_point(data=to_plot_dat[3, ], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won),
             colour="forestgreen", alpha = 0.7) +
  
  #Colour Djoker in Blue
  geom_point(data=to_plot_dat[2, ], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won),
             colour="blue", alpha = 0.7) +
  
  #Add titles
  ggtitle("2019 Grand Slam Season") + 
  xlab("% Service Games Held") + ylab("% Service Games Broken") +
  theme_classic() +
  
  theme(panel.background = element_rect(fill = "#F1F8F7", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1.6),
                                face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "#F1F8F7", linetype = 1),
        axis.title = element_text(face = "bold", size = 13)) +
  
  guides(size = guide_legend(override.aes = list(colour = list("#008080"))))

p




#######################################################
# GGPLOTLY Code:
# Create interactive plot
library(plotly)

p2 <- ggplot(to_plot_dat, aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won, label = Player_Name)) + 
  geom_point(alpha = 0.8, color = "#008080") +
  
  #Add labels for Djoker, Roger and Rafa
  #geom_text_repel(aes(label=ifelse(SvGmsW_Prop > 0.886486 & BPW_Prop > 0.2635869 , 
  #                                 as.character(Last_Name),'')
  #), size = 4.0, nudge_y = -4) +
  
  geom_text(data =  to_plot_dat %>% filter(Last_Name == "Djokovic"), 
            aes(label=as.character(Last_Name)), color = "blue", size = 4,
            nudge_x = -6.5) +
  
  geom_text(data =  to_plot_dat %>% filter(Last_Name == "Nadal"), 
            aes(label=as.character(Last_Name)), size = 4, color = "red",
            nudge_y = -4) +
  geom_text(data =  to_plot_dat %>% filter(Last_Name == "Federer"), 
            aes(label=as.character(Last_Name)), size = 4, color = "forestgreen",
            nudge_y = 3) +
  
  #Adjust size of bubble points and legend title
  scale_size(range = c(.1, 8), name="Matches Played") +
  
  #Colour Djokovic blue
  geom_point(data=to_plot_dat[2, ], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won),
             colour="blue", alpha = 0.7) +
  
  #Colour Federer in Green
  geom_point(data=to_plot_dat[3, ], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won),
             colour="forestgreen", alpha = 0.7) +
  
  #Colour Rafa in Red
  geom_point(data=to_plot_dat[1, ], aes(x=SvGmsW_Prop*100, y=BPW_Prop*100, size = Matches_Won),
             colour="red", alpha = 0.7) +
  
  #Add titles
  ggtitle("2019 Grand Slam Season") + 
  xlab("% Service Games Held") + ylab("% Service Games Broken") +
  theme_classic() +
  
  theme(panel.background = element_rect(fill = "#F1F8F7", # background colour
                                        colour = "black", # border colour
                                        size = 0.5, linetype = "solid"),
        plot.title=element_text(size = rel(1.6),
                                face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "#F1F8F7", linetype = 1),
        axis.title = element_text(face = "bold", size = 13)) +
  
  guides(size = guide_legend(override.aes = list(colour = list("#008080"))))

ggplotly(p2, tooltip = c("size", "Player_Name"))
