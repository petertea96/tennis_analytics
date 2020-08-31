# 2 Choropleth mapts:
setwd("/Users/petertea/Documents")
df <- read.csv("ATP_Country_Rank.csv")

library(dplyr)
library(plotly)

l <- list(color = "black", width = 0.8)

# specify map projection/options
g <- list(showframe=F,
          showcoastlines=F,
          projection_type='equirectangular',
          landcolor = '#CAD2C9',#'#CCECC7',
          
          showland = T,
          showcountries = T,
          countrycolor = 'black',
          countrywidth = 0.5,
          projection = list(
            type = 'natural earth')
)






df1 <- df %>%
  filter(year == 2003)

df2 <- df %>%
  filter(year == 2004)



p <- plot_geo() %>%
  add_trace(data = df1,
    type='choropleth', # type of map-plot
    colors = 'BuGn', #Bluered
    #autocolorscale = F,
    locationmode='country names',
    locations = ~COUNTRY_NAME, # Identifies which country
    reversescale = F, # We want dark colours to represent higher counts and light colour to represent lower counts
    z = ~Prop_R_P, #Variable we want to plot on map
    marker = list(color = "black", width = 0.8),
    visible = T
    #text = ~Country_Name, #Aesthetics, gives full name of country as well a number
  )  %>%
  colorbar(title = '% of yearly<br>Ranking Points',
           tickprefix = '%') %>%
  add_trace(data = df2,
    type='choropleth', # type of map-plot
    colors = 'BuGn', #Bluered
    #autocolorscale = F,
    locationmode='country names',
    locations = ~COUNTRY_NAME, # Identifies which country
    reversescale = F, # We want dark colours to represent higher counts and light colour to represent lower counts
    z = ~Prop_R_P, #Variable we want to plot on map
    marker = list(color = "black", width = 0.8),
    visible = F
    #text = ~Country_Name, #Aesthetics, gives full name of country as well a number
  ) %>%
  colorbar(title = '% of yearly<br>Ranking Points',
           tickprefix = '%') %>%
  layout(title = 'Country contribution to ATP Ranking Points',
         geo = g,
    updatemenus = list(
      list(
        yanchor = 'auto',
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(T,F)),
               label = '2003'),
          list(method = "restyle",
               args = list("visible", list(F,T)),
               label = '2004')
  ))))  


p

#############################################################






years = 2003:2019


for (i in 1:length(years) ){
  cur_year <- years[i]
  
  tennis_rank_dat <- df %>%
    filter(year == cur_year)
  
  visible_list <- as.list(rep(FALSE, times = length(years)))
  visible_list[[i]] <- TRUE
  if (cur_year == 2003){
    p <- plot_geo() %>%
      add_trace(
        data = tennis_rank_dat,
        type='choropleth', # type of map-plot
        colors = 'BuGn', #Bluered
        #autocolorscale = F,
        locationmode='country names',
        locations = ~COUNTRY_NAME, # Identifies which country
        reversescale = F, # We want dark colours to represent higher counts and light colour to represent lower counts
        z = ~Prop_R_P, #Variable we want to plot on map
        marker = list(color = "black", width = 0.8),
        visible = visible_list)
    
    buttons <- list(
      list(method = "restyle",
           args = list("visible", visible_list),
           label = '2003'))
    
  }
  
  else {
    p <- p %>%
      add_trace(
        data = tennis_rank_dat,
        type='choropleth', # type of map-plot
        colors = 'BuGn', #Bluered
        #autocolorscale = F,
        locationmode='country names',
        locations = ~COUNTRY_NAME, # Identifies which country
        reversescale = F, # We want dark colours to represent higher counts and light colour to represent lower counts
        z = ~Prop_R_P, #Variable we want to plot on map
        marker = list(color = "black", width = 0.8),
        visible = visible_list
        #text = ~Country_Name, #Aesthetics, gives full name of country as well a number
      )
    
    buttons[[i]] <- list(
      list(method = "restyle",
           args = list("visible", visible_list),
           label = as.character(cur_year)))
    
  }
  
}


p %>%
  colorbar(title = '% of yearly<br>Ranking Points',
           tickprefix = '%') %>%
  layout(
    title = 'Country contribution to ATP Ranking Points',
    geo = g,
    updatemenus = list(
      list(
        yanchor = 'auto',
        buttons = buttons)))

