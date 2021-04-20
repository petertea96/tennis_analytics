### -- Plotting the tennis court functions
# For aesthetic reasons, it's nice to visualize tennis courts with shot heat maps


library(ggplot2)

# -- Draw a half tennis court
# -- Colours are inspired by Miami Open

draw_half_tennis_court <- function(){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-1, xmax=11.89, ymin=-5.485, ymax=-4.115), 
              size = 0.75,
              color="white", alpha=0.25, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=-1, xmax=11.89, ymin= 4.115, ymax= 5.485), 
              size = 0.75,
              color="white", alpha=0.25, fill = '#3C638E'),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-1, xmax=0, ymin=-4.115, ymax=0), 
              color="white", size = 0.75, alpha=0.25, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115), 
              color="white", size = 0.75, alpha=0.25, fill = '#3C638E'),
    geom_rect(mapping=aes(xmin=-1, xmax=0, ymin=0, ymax= 4.115), 
              color="white", size = 0.75, alpha=0.25, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115), 
              color="white", size = 0.75, alpha=0.25, fill = '#3C638E'),
    # -- Baseline
    #geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115), 
    #          color="black", alpha=0.5, fill = 'white'),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115), 
              color="white", alpha=0.25, fill = '#3C638E'),
    
    # -- Add serve center marker
    geom_rect(mapping=aes(xmin=11.5, xmax=11.89, ymin=0, ymax=0), 
              color="white", size = 1), 
    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1.25, colour = '#a9a9a9'), 
    # -- Add dashed lines separating 3 serve locations
    geom_segment(aes(x = -1, xend = 6.4, y = 1.37, yend = 1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    geom_segment(aes(x = -1, xend = 6.4, y = 2*1.37, yend = 2*1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    geom_segment(aes(x = -1, xend = 6.4, y = -1.37, yend = -1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    geom_segment(aes(x = -1, xend = 6.4, y = -2*1.37, yend = -2*1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    labs(x = '', y = ''),
    theme_classic(),
    theme(panel.background = element_rect(fill="#ad5049"), #99e6b3,#aaf0d1 <-- Miami teal colours
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line=element_blank(),
          strip.text = element_text(colour = 'black',face = 'bold'))
    
  )
}


# ggplot() +
#   draw_half_tennis_court()



# -- Half tennis court with labels
draw_half_tennis_court_with_annotation <- function(){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-1, xmax=11.89, ymin=-5.485, ymax=-4.115), 
              size = 0.75,
              color="white", fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=-1, xmax=11.89, ymin= 4.115, ymax= 5.485), 
              size = 0.75,
              color="white", fill = '#3C638E'),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-1, xmax=0, ymin=-4.115, ymax=0), 
              color="white", size = 0.75, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115), 
              color="white", size = 0.75, fill = '#3C638E'),
    geom_rect(mapping=aes(xmin=-1, xmax=0, ymin=0, ymax= 4.115), 
              color="white", size = 0.75, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115), 
              color="white", size = 0.75, fill = '#3C638E'),
    # -- Baseline
    #geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115), 
    #          color="black", alpha=0.5, fill = 'white'),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115), 
              color="white", fill = '#3C638E'),
    
    # -- Add serve center marker
    geom_rect(mapping=aes(xmin=11.5, xmax=11.89, ymin=0, ymax=0), 
              color="white", size = 1), 
    
    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1.25, colour = '#a9a9a9'), 
    # -- Add dashed lines separating 3 serve locations
    geom_segment(aes(x = -1, xend = 6.4, y = 1.37, yend = 1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    geom_segment(aes(x = -1, xend = 6.4, y = 2*1.37, yend = 2*1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    geom_segment(aes(x = -1, xend = 6.4, y = -1.37, yend = -1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    geom_segment(aes(x = -1, xend = 6.4, y = -2*1.37, yend = -2*1.37),
                 size = 0.05, linetype='dashed', colour = 'white'),
    labs(x = '', y = ''),
    # -- annotations
    annotate("text", x = 0.2, y = 5.8, label = "Net", colour = 'white',
             fontface =2),
      annotate("text", x = 1, y = 2, label = "Deuce Court",
               colour = 'white', fontface =2, angle = 90),
      annotate("text", x = 1, y = -2, label = "Ad. Court", 
               colour = 'white', fontface =2, angle = 90),
      annotate("text", x = 9, y = 4.5, label = "Singles Sideline", 
               colour = 'white', fontface =2),
      annotate("text", x = 9, y = 5.8, label = "Doubles Sideline",
               colour = 'white', fontface =2),
      annotate("text", x = 6.8, y = 0, label = "Service Line",
               angle = 90, colour = 'white', fontface =2),
      annotate("text", x = 12.3, y = 0, label = "Baseline",
               angle = 90, colour = 'white', fontface =2),
    # -- Label serve regions
    annotate("text", x = 5.5, y = 3.5, label = "Wide",
             colour = 'white', fontface =1),
    annotate("text", x = 5.5, y = -3.5, label = "Wide",
             colour = 'white', fontface =1),
    annotate("text", x = 5.5, y = -2, label = "Body",
             colour = 'white', fontface =1),
    annotate("text", x = 5.5, y = 2, label = "Body",
             colour = 'white', fontface =1),
    annotate("text", x = 5.5, y = -0.75, label = "T",
             colour = 'white', fontface =1),
    annotate("text", x = 5.5, y = 0.75, label = "T",
             colour = 'white', fontface =1),
    # -- Center mark
    annotate("text", x = 10.75, y = 0, label = "Center\nMark",
             size = 3.5,
             colour = 'white', fontface =1),
    theme_classic(),
    theme(panel.background = element_rect(fill="#ad5049"), 
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line=element_blank(),
          strip.text = element_text(colour = 'black',face = 'bold'))
    
  )
}


# ggplot() +
#   draw_half_tennis_court_with_annotation()
# 
# ggsave('tennis_court_anatomy.jpg',
#        width=7.25, height=5,
#        dpi = 400)


# -- Draw a full tennis court
draw_full_tennis_court <- function(){
  list(
    # -- Two outer doubles lines
    geom_rect(mapping=aes(xmin=-11.89, xmax=11.89, ymin=-5.485, ymax=-4.115), 
              color="white", alpha=0.5, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=-11.89, xmax=11.89, ymin= 4.115, ymax= 5.485), 
              color="white", alpha=0.5, fill = '#3C638E'),
    # -- Service boxes
    geom_rect(mapping=aes(xmin=-6.4, xmax=0, ymin=-4.115, ymax=0), 
              color="white", alpha=0.5, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=-4.115), 
              color="white", alpha=0.5, fill = '#3C638E'),
    geom_rect(mapping=aes(xmin=-6.4, xmax=0, ymin=0, ymax= 4.115), 
              color="white", alpha=0.5, fill = '#3C638E'), 
    geom_rect(mapping=aes(xmin=0, xmax=6.4, ymin=0, ymax=4.115), 
              color="white", alpha=0.5, fill = '#3C638E'),
    # -- Baseline
    geom_rect(mapping=aes(xmin=-11.89, xmax=-6.4, ymin=-4.115, ymax=4.115), 
              color="white", alpha=0.5, fill = '#3C638E'),
    geom_rect(mapping=aes(xmin=6.4, xmax=11.89, ymin=-4.115, ymax=4.115), 
              color="white", alpha=0.5, fill = '#3C638E'),
    
    # -- Emphasize the net
    geom_segment(aes(x = 0, xend = 0, y = -5.485, yend = 5.485),
                 size = 1.5), 
    labs(x = '', y = ''),
    theme_classic(),
    theme(panel.background = element_rect(fill="#ad5049"), 
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line=element_blank(),
          strip.text = element_text(colour = 'black',face = 'bold')))
}

 # ggplot() +
 #   draw_full_tennis_court()




