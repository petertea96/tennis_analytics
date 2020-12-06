# deploy R shiny app
# Note: Need to authorize first
# https://shiny.rstudio.com/articles/shinyapps.html

library(rsconnect)
rsconnect::deployApp("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/aces_allowed_koval/prototype_shiny")


rsconnect::showLogs(appName="prototype_shiny",streaming=TRUE)
