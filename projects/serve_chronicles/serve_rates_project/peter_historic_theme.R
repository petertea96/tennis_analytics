# -- Theme for Historic serve rates


peter_historic_theme <- function(number_text_color = 'black',
                                 family_font = 'mono',
                                 bg_colour = '#FEF5E7'
                                 ){
  theme(
    
    # -- Title
    title = element_text(size = rel(1.25),
                         face = "bold",
                         family=family_font),
    
    # -- Legend
    legend.key=element_blank(),
    #legend.position = c(0.56, - 0.075),
    legend.position = 'bottom',
    legend.title = element_blank(), # no title
    legend.direction = 'horizontal',
    legend.margin=margin(t=0, r=-2, b=5, l=-2),
    legend.background = element_blank(),
    legend.text = element_text(family=family_font,
                               face = 'bold'),
    
    plot.caption=element_text(hjust=1,
                              vjust = -1,
                              size=9,
                              margin=margin(t=-17),
                              family=family_font, 
                              face='italic'),
    
    plot.margin = margin(10, 10, 10, 10),
    
    
    #grids
    #panel.grid.major = element_blank(), 
    panel.grid.major.x = element_line(colour="#E5E4E2", size=0.01),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour="#E5E4E2", size=0.005),
    panel.grid.minor.y = element_blank(), 
    
    
    #axis
    #axis.ticks = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(vjust=28, hjust = 1,
                                size = 10,
                                family=family_font,
                                face="bold"),
    
    axis.title.y = element_text(size = 10,
                                hjust = 0.6,   # Down (-), Up (+)
    ),
    
   # axis.title.y = element_text(size = 10,
  #                              angle= 0 ,
  #                              hjust = 1,   # Down (-), Up (+)
  #  ),
    
    axis.text.x = element_text(hjust=0.5,
                               face = 'bold'),

    axis.line = element_line(),
    axis.line.x = element_line(size = 0.75),
    axis.line.y = element_line(size = 0.75),
    
    # -- Plot background
    plot.background = element_rect(fill = bg_colour, #outside plot background
                                   colour = "black",size = 1),
    panel.background = element_rect(fill = bg_colour), #inside plot background
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )
  
  
  
}



# -- Adjusted theme for Ace and Double fault trends through time (GG Animation)

# -- Theme for Historic serve rates

peter_year_theme <- function(number_text_color = 'black',
                                 family_font = 'mono',
                                 bg_colour = '#FEF5E7'
){
  theme(
    
    # -- Title
    title = element_text(size = rel(1.0),
                         face = "bold",
                         family=family_font),
    
    # -- Legend
    legend.key=element_blank(),
    #legend.position = c(0.56, - 0.075),
    legend.position = 'bottom',
    legend.title = element_blank(), # no title
    legend.direction = 'horizontal',
    #legend.margin=margin(t=0, r=-2, b=5, l=-2),
    legend.background = element_blank(),
    legend.text = element_text(family=family_font,
                               face = 'bold'),
    
    plot.caption=element_text(hjust=1, 
                              size=9,
                              margin=margin(t=-17),
                              family=family_font, 
                              face='italic'),
    
    plot.margin = margin(10, 10, 10, 10),
    
    
    #grids
    panel.grid.major.x = element_line(colour="#E5E4E2", size=0.01),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour="#E5E4E2", size=0.005),
    panel.grid.minor.y = element_blank(), 
    
    
    #axis
    #axis.title.x = element_text(size = 11,
    #                            family=family_font,
    #                            face="bold"),
    
    axis.title.x = element_text(vjust = 0.95,
                                hjust = 0.5,
                                #vjust=22, hjust = 1, 
                                size = 12,
                                family=family_font,
                                face="bold"),
    
    #axis.title.y = element_text(hjust = 0.5,
    #                            vjust = 1.5,
    #                            size = 11),
    
    axis.title.y = element_text(#angle = 0,
                                vjust = 0.7,
                                hjust = 0.55,
                                size = 12,
                                face = 'bold'),
    
    
    axis.text.x = element_text(hjust=0.5,
                               face = 'bold'),
    
    axis.line = element_line(),
    axis.line.x = element_line(size = 0.75),
    axis.line.y = element_line(size = 0.75),
    
    # -- Plot background
    plot.background = element_rect(fill = bg_colour, #outside plot background
                                   colour = "black",size = 1),
    panel.background = element_rect(fill = bg_colour), #inside plot background
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )
  
  
  
}
