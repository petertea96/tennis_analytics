# Purpose: Plot quantiles of player performances
# For simplicity and compactness, only consider the top 7 ATP players

setwd("C:/Users/Peter/Documents/zverev_analysis")

library(dplyr)
library(ggplot2)
library(reshape2)


dat = read.csv('./data/top10.csv')
dat = dat[-c(4,5,8),]
strengths = dat %>%
  select(name ,pr_1stin, pr_w1_giv_1in, pr_win_on_1st_serve,
         pr_2ndin, pr_w2_giv_2in, pr_win_on_2nd_serve)



# --> Display last name, instead of full name
strengths = strengths %>%
  mutate(name = sub(".* ", "",name) )

# --> Stack the data, to make it easier to plot
strengths_melt = melt(strengths, 
                      variable.name = 'Performance',
                      value.name = 'Percentile')

strengths_melt = strengths_melt %>%
  mutate(Performance = dplyr::recode(Performance, 
                                     pr_1stin ='Prob(1st Serve In)',  
                                     pr_w1_giv_1in = 'Prob(Win | 1st Serve In)',
                                     pr_2ndin ='Prob(2nd Serve In)',
                                     pr_w2_giv_2in = 'Prob(Win | 2nd Serve In)',
                                     pr_win_on_2nd_serve = 'Prob(Win on 2nd Serve)',
                                     pr_win_on_1st_serve = 'Prob(Win on 1st Serve)'))

#get_label = function(num){
#  num = floor(num)
#  suff <- case_when(num %in% c(11,12,13) ~ "th",
#                    num %% 10 == 1 ~ 'st',
#                    num %% 10 == 2 ~ 'nd',
##                    num %% 10 == 3 ~'rd',
#                    TRUE ~ "th")
  
#  return(paste(num, suff, sep = ""))
#}

fillcolors = c('Djokovic' = '#64A4E6', 'Nadal' = '#FB7971', 'Federer' = '#B8EBD0',
               'Zverev' = '#FFF694', 'Thiem' = '#BACEFF', 'Medvedev' = '#EAD7D7',
               'Tsitsipas' = '#FFB3A1')
peter_theme <- function(){
  theme_bw() +
    theme(panel.background = element_rect(#fill = "#DBF5F0", 
      colour = "black", # border colour
      size = 0.8, linetype = "solid"),
      plot.title=element_text(size = rel(1.5),
                              face = "bold", hjust = 0.5),
      axis.text.y = element_text(face="bold", color="black", 
                                 size=11),
      axis.line = element_line(colour = "black", 
                               size = 0.8, linetype = "solid"),
      axis.title.y=element_blank(),
      axis.title.x = element_text(colour = "black", face = "bold",
                                  size = 12) ,
      plot.background = element_rect(#fill = "#DBF5F0",
        colour = "black",size = 1),
      legend.position = "none"
      )
}

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# --> First serve performances
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
first_in_dat = strengths_melt %>%
  filter( Performance %in% c('Prob(1st Serve In)'))

# Order name factor by percentile value...
first_in_dat$name <- factor(first_in_dat$name,
                            levels = first_in_dat$name[order(first_in_dat$Percentile)])


first_inwin_dat = strengths_melt %>%
  filter( Performance %in% c('Prob(Win | 1st Serve In)' ))

first_inwin_dat$name <- factor(first_inwin_dat$name,
                               levels = first_inwin_dat$name[order(first_inwin_dat$Percentile)])

ggplot() + 
  coord_flip() +
  scale_fill_manual(values = fillcolors, 'Player') +
  geom_bar( data = first_inwin_dat, aes(x = Performance, fill = name, y = Percentile), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black') +
  geom_text(data = first_inwin_dat,
            aes(x = Performance, fill = name, y = Percentile,
                label =  paste(name,': ', round(Percentile,3)*100, '%', sep = '')),
                  
                # label if we used rankings
                #label = paste(name,': ',get_label(Percentile), sep = '')),
            position = position_dodge(width = 0.8), 
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.25,
            fontface = 'bold') +
  geom_bar(data = first_in_dat, aes(x = Performance, fill = name, y = Percentile),
           stat = "identity", position = 'dodge', width = 0.8, color = 'black') + 
  geom_text(data = first_in_dat,
            aes(x = Performance, fill = name,
                y = Percentile,
                label =  paste(name,': ', round(Percentile,3)*100, '%', sep = '')),
            position=position_dodge(0.8), 
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.25,
            fontface = 'bold') +
  ylab("Probability") +
  xlab("") + 
  ggtitle('2019 First Serve Performances') + 
  peter_theme()




##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# --> Second serve performances
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
second_in_dat = strengths_melt %>%
  filter( Performance %in% c('Prob(2nd Serve In)'))

# Order name factor by percentile value...
second_in_dat$name <- factor(second_in_dat$name,
                             levels = second_in_dat$name[order(second_in_dat$Percentile)])


second_inwin_dat = strengths_melt %>%
  filter( Performance %in% c('Prob(Win | 2nd Serve In)' ))

second_inwin_dat$name <- factor(second_inwin_dat$name,
                                levels = second_inwin_dat$name[order(second_inwin_dat$Percentile)])

secondin_label = second_in_dat$Percentile
#secondin_label[1] = secondin_label[1] + 24

ggplot() + 
  coord_flip() +
  scale_fill_manual(values = fillcolors, 'Player') +
  geom_bar( data = second_inwin_dat, aes(x = Performance, fill = name, y = Percentile), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black') +
  geom_text(data = second_inwin_dat,
            aes(x = Performance, fill = name, y = Percentile,
                label = paste(name, ': ',round(Percentile,3)*100, '%', sep = '')),
            position = position_dodge(width = 0.8), 
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.25,
            fontface = 'bold') +
  geom_bar(data = second_in_dat, aes(x = Performance, fill = name, y = Percentile),
           stat = "identity", position = 'dodge', width = 0.8, color = 'black') + 
  geom_text(data = second_in_dat,
            aes(x = Performance, fill = name,
                y = secondin_label,
                #second_in_dat$Percentile; but modifying zverev's position
                label = paste(name, ': ',round(Percentile,3)*100, '%', sep = '')),
            position=position_dodge(0.8), 
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.25,
            fontface = 'bold') +
  ylab("Probability") +
  xlab("") + 
  ggtitle('2019 Second Serve Performances') + 
  peter_theme()




##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# --> 1st and 2nd Serve WIN percentages
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 

first_win_dat = strengths_melt %>%
  filter( Performance %in% c('Prob(Win on 1st Serve)'))

# Order name factor by percentile value...
first_win_dat$name <- factor(first_win_dat$name,
                             levels = first_win_dat$name[order(first_win_dat$Percentile)])




second_win_dat = strengths_melt %>%
  filter( Performance %in% c('Prob(Win on 2nd Serve)'))

# Order name factor by percentile value...
second_win_dat$name <- factor(second_win_dat$name,
                              levels = second_win_dat$name[order(second_win_dat$Percentile)])

secondwin_labels = second_win_dat$Percentile
#secondwin_labels[1] = secondwin_labels[1] + 2.5 

ggplot() + 
  coord_flip() +
  scale_fill_manual(values = fillcolors, 'Player') +
  
  geom_bar( data = first_win_dat, aes(x = Performance, fill = name, y = Percentile), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black') +
  geom_text(data = first_win_dat,
            aes(x = Performance, fill = name, y = Percentile,
                label = paste(name, ': ',round(Percentile,3)*100, '%', sep = '')),
            position = position_dodge(width = 0.8), 
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.25,
            fontface = 'bold') +
  geom_bar( data = second_win_dat, aes(x = Performance, fill = name, y = Percentile), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black') +
  geom_text(data = second_win_dat,
            aes(x = Performance, fill = name, y = secondwin_labels,
                label = paste(name, ': ',round(Percentile,3)*100, '%', sep = '')),
            position = position_dodge(width = 0.8), 
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.25,
            fontface = 'bold') +
  ylab("Probability") +
  xlab("") + 
  ggtitle('2019 Overall Serve Performances') + 
  peter_theme()



  