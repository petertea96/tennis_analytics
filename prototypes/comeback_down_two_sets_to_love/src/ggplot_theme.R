# -- Theme

#### Theme for volatility plot

peter_theme <- function(number_text_color = 'black',
                                   family_font = 'mono',
                                   bg_colour = '#FEF5E7',
                                   text_colour = "#414141"){
  theme(
    
    # -- Title
    plot.title = element_text(hjust = 0.5),
    title = element_text(size = rel(1.1),
                         face = "bold",
                         family=family_font,
                         colour = 'black'),
    
    # -- Legend
    legend.key=element_blank(),
    #legend.position = c(0.56, - 0.075),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    #legend.margin=margin(t=0, r=-2, b=5, l=-2),
    legend.background = element_blank(),
    legend.text = element_text(family=family_font,
                               size = 9,
                               face = 'bold'),
    legend.title = element_text(family=family_font,
                                size = 10,
                                colour = 'black'),
    #legend.box.background = element_rect(colour = "black",fill = bg_colour),
    
    plot.caption=element_text(hjust=1, 
                              size=8.5,
                              margin=margin(t=-17),
                              family=family_font, 
                              face='italic'),
    
    plot.margin = margin(10, 10, 10, 10),
    
    
    #grids
    panel.grid.major.x = element_line(colour="#E5E4E2", size=0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour="#E5E4E2", size=0.01),
    panel.grid.minor.y = element_blank(), 
    
    
    #axis
    axis.title.x = element_text(vjust = 0.95,
                                hjust = 0.48,
                                size = 12,
                                family=family_font,
                                face="bold"),
    
    axis.title.y = element_text(#angle = 0,
      vjust = 0.7,
      hjust = 0.6,
      size = 12,
      face = 'bold'),
    
    
    axis.text.x = element_text(hjust=0.5,
                               size = 11,
                               color=text_colour),
    axis.text.y = element_text(color=text_colour, 
                               size=11,
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


