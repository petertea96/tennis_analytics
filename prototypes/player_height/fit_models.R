##### Different model fits for Ace Rate vs. Height -----

# -- Set appropriate working directory
setwd("/Users/petertea/tennis_analytics/prototypes/player_height")


# -- Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
extrafont::loadfonts()

source("/Users/petertea/tennis_analytics/prototypes/player_height/src/plot_theme.R")

# -- Load in processed aces data -----
aces_file_name <- '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19_TOTAL.csv'
atp_aces_10_19_df <- read.csv(aces_file_name, stringsAsFactors = FALSE)

atp_aces_10_19_df[,c(12:17)] <- 100*atp_aces_10_19_df[,c(12:17)]

# Arrange by X for future plotting
atp_aces_10_19_df <- atp_aces_10_19_df[order(atp_aces_10_19_df$player_height_cm),]

### Cubic regression model -----
cubic_model <- lm(data = atp_aces_10_19_df,
                  formula = ace_rate ~ player_height_cm + I(player_height_cm^2) +
                  I(player_height_cm^3))

summary(cubic_model)

# -- Fit Loess model
# (Default is span = 75% & Polynomial 2)
loess_model <- loess(data = atp_aces_10_19_df,
                     formula = ace_rate ~ player_height_cm )

plot(x=atp_aces_10_19_df$player_height_cm,
     y=atp_aces_10_19_df$ace_rate,
     main="Loess Kernel Smoothers", col="gray")

lines(x=atp_aces_10_19_df$player_height_cm, 
      y=predict(cubic_model, newdata=atp_aces_10_19_df), col=colors()[24], lwd=2)

lines(x=atp_aces_10_19_df$player_height_cm,
      y=predict(loess_model, newdata=atp_aces_10_19_df), col=colors()[121], lwd=2)

# -- Basis (cubic) splines

# 5 DF spline (2 knots)
cub.spl.5 <- lm(data = atp_aces_10_19_df,
                formula = ace_rate ~ splines::bs(player_height_cm,df=5))

lines(x=atp_aces_10_19_df$player_height_cm,
      y=predict(cub.spl.5, newdata=atp_aces_10_19_df), col='red', lwd=2)

# 7 DF spline (4 knots)
cub.spl.7 <- lm(data = atp_aces_10_19_df,
                formula = ace_rate ~ splines::bs(player_height_cm,df=7))

lines(x=atp_aces_10_19_df$player_height_cm,
      y=predict(cub.spl.7, newdata=atp_aces_10_19_df), col='green', lwd=2)


# Natural splines

nat.spl.5 <- lm(data = atp_aces_10_19_df,
                ace_rate ~ splines::ns(player_height_cm,df=5))

lines(x=atp_aces_10_19_df$player_height_cm,
      y=predict(nat.spl.5, newdata=atp_aces_10_19_df), col='orange', lwd=2)


# Smoothing splines
# -- smoothing splines have knots at each point
# -- Adds regularization
sm.spl.5 <- smooth.spline(x=atp_aces_10_19_df$player_height_cm,
                          y=atp_aces_10_19_df$ace_rate, df=5)
sm.spl.5
lines(sm.spl.5, col='red', lwd=2)

# Optimal Spline 
# USe GCV
sm.spl.opt <- smooth.spline(x=atp_aces_10_19_df$player_height_cm,
                            y=atp_aces_10_19_df$ace_rate, cv=FALSE)

lines(sm.spl.opt, col='red', lwd=2)

