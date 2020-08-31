# Recreate choropleth maps in R
setwd("/Users/petertea/Documents")

df <- read.csv("ATP_Country_Rank.csv")

df <- df %>%
  filter(year == 2019)
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

p <- plot_geo() %>%
  add_trace(
    data = df,
    type='choropleth', # type of map-plot
    colors = 'BuGn', #Bluered
    #autocolorscale = F,
    locationmode='country names',
    locations = ~COUNTRY_NAME, # Identifies which country
    reversescale = F, # We want dark colours to represent higher counts and light colour to represent lower counts
    z = ~Prop_R_P, #Variable we want to plot on map
    marker = l
    #text = ~Country_Name, #Aesthetics, gives full name of country as well a number
  ) %>%
  colorbar(title = '% of yearly<br>Ranking Points',
           tickprefix = '%') %>%
  layout(
    title = 'Country contribution to ATP Ranking Points',
    geo = g
  )

p






data_slider = list()
years <- 2003:2019

for (i in 1:length(years)){
  year <- years[i]
  df <- df %>%
    filter(year == year)
  
  p <- plot_geo(df) %>%
    add_trace(
      type='choropleth', # type of map-plot
      colors = 'BuGn', #Bluered
      #autocolorscale = F,
      locationmode='country names',
      locations = ~COUNTRY_NAME, # Identifies which country
      reversescale = F, # We want dark colours to represent higher counts and light colour to represent lower counts
      z = ~Prop_R_P, #Variable we want to plot on map
      marker = l
      #text = ~Country_Name, #Aesthetics, gives full name of country as well a number
    ) %>%
    colorbar(title = '% of yearly<br>Ranking Points',
             tickprefix = '%') %>%
    layout(
      title = 'Country contribution to ATP Ranking Points',
      geo = g,
      sliders = sliders,
      annotations = list(
        x=0.55,
        y=0.1,
        xref='paper',
        yref='paper',
        text='Data Source: <a href="http://www.oncourt.info/download.html">OnCourt</a>',
        showarrow = F
      )
    
    )

  data_slider[[i]] <- p

}


steps = list()

for (i in 1:length(data_slider)){
  step = list(method='restyle',
              args=list('visible', rep(FALSE, times = length(data_slider))),
              label=paste('Year {}',(i + 2003))) # label to be displayed for each step (year)
  step$args[[2]][i] = TRUE
  steps[[i]] = step

}



sliders = list(active = 0,
               pad = list(t=1),
               steps = steps)



####
# Set up the layout (including slider option)

layout = list(
  title_text = "Country contribution to ATP Ranking Points",
  geo=list(
    showframe=F,
    showcoastlines=F,
    projection_type='equirectangular',
    landcolor = '#CAD2C9',#'#CCECC7',
    
    showland = T,
    showcountries = T,
    countrycolor = 'black',
    countrywidth = 0.5,
    projection = dict(
      type = 'natural earth')
    
  ),
  sliders = sliders,
  annotations = [dict(
    x=0.55,
    y=0.1,
    xref='paper',
    yref='paper',
    text='Data Source: <a href="http://www.oncourt.info/download.html">OnCourt</a>',
    showarrow = F
  )]
)


# I create the figure object:
fig = dict(data=data_slider, layout=layout) 

# to plot in the notebook
plotly.offline.iplot(fig)




