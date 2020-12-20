##### Fit Mixed Models (GLMMs & GAMMs)

# -- Load libraries
library(dplyr)
library(ggplot2)
library(mgcv)
library(lme4)
library(gamm4)

setwd("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/")

# --> Load Data
filename <- "/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/predict_serve_direction/process_serve_location_pbp/processed_modified_body_serve_slam_pointbypoint.rds"
slam_data <- readRDS(filename)

slam_data %>%
  group_by(tournament, year) %>%
  summarise(missing_speed = sum(Speed_KMH == 0),
            missing_mph = sum(is.null(Speed_MPH)),
            num_points = n())

slam_data %>%
  group_by()

# -- Keep only interesting variables
training_data <- slam_data %>%
  select(ServeNumber, Speed_KMH, server_won, server_name, serve_location,
         returner_name, court_side, server_pressure, year, 
         tournament, player_last_serve_loc, player_last_serve2_loc) %>%
  mutate(is_body = ifelse(serve_location == 'B', 1, 0),
         previous_serve_is_body = ifelse(player_last_serve_loc == 'B', 1, 
                                   ifelse((player_last_serve_loc %in% c('W', 'T')), 0,
                                           player_last_serve_loc))) %>%
  filter(complete.cases(.)) %>%
  filter(Speed_KMH>0) #%>%
  filter(year < 2019)

training_data <- cbind(training_data,
                       nnet::class.ind(training_data$server_pressure))

training_data <- training_data %>%
  mutate(scaled_serve_speed = (Speed_KMH - mean(Speed_KMH)) / sd(Speed_KMH) ) 



################################################################################
################################################################################
################################################################################
# ----- Multinomial Regression #####
# multinom_data = training_data[complete.cases(training_data),]
# multinom_data$serve_loc_hand <- ifelse(multinom_data$serve_loc_hand == 'forehand',0,1)
# library(nnet)
# mod.fit <- multinom(data=multinom_data, 
#                     formula=serve_loc_hand ~ as.factor(ServeNumber) + Speed_KMH + 
#                       court_side + player_last_serve_binary_loc + 
#                       player_last_serve2_binary_loc +
#                       Advantage + Ahead + Behind + Break,
#                     maxit=5000, trace=TRUE)

mod.fit <- glm(data=training_data, 
               formula=is_body ~ as.factor(ServeNumber) + scaled_serve_speed + 
                 court_side + previous_serve_is_body + 
                 Advantage +  Break + Ahead + Behind,
               family = 'binomial')

summary(mod.fit)

#pred.class.1 <- predict(mod.fit, newdata=multinom_data)
pred.prob.1 <- round(predict(mod.fit, newdata=training_data, type="response"), digits=3)

table(training_data$is_body, 
      ifelse(pred.prob.1 >0.5,1,0), 
      dnn=c("Obs","Pred"))  

(misclass_error <- mean(ifelse(pred.prob.1 > 0.5, 0, 1) == training_data$is_body))


################################################################################
################################################################################
################################################################################
# ----- Random Forest #####

multinom_data$ServeNumber <- as.factor(multinom_data$ServeNumber)
multinom_data$serve_location <- as.factor(multinom_data$serve_location)
multinom_data$court_side <- as.factor(multinom_data$court_side)
multinom_data$player_last_serve_loc <- as.factor(multinom_data$player_last_serve_loc)
multinom_data$player_last_serve2_loc <- as.factor(multinom_data$player_last_serve2_loc)
multinom_data$Advantage <- as.factor(multinom_data$Advantage)
multinom_data$Ahead <- as.factor(multinom_data$Ahead)
multinom_data$Behind <- as.factor(multinom_data$Behind)
multinom_data$Break <- as.factor(multinom_data$Break)

library(randomForest)
rf_model = randomForest(data=multinom_data, 
             serve_location ~ ServeNumber + Speed_KMH + 
               court_side + player_last_serve_loc + player_last_serve2_loc +
               Advantage + Ahead + Behind + Break, 
             importance=TRUE, 
             ntree=1000, 
             keep.forest=TRUE)

(oob_error <- mean(ifelse(rf_model$predicted == multinom_data$serve_location, yes=0, no=1)))

table(multinom_data$serve_location,rf_model$predicted, dnn=c("Obs","Pred"))  


################################################################################
################################################################################
################################################################################
#### GLMM

# mlogit?
# mme
# MCMCMglmm

# Scale Speed variable????
mixed_logit_fit <- glmer(formula = is_body ~ as.factor(ServeNumber) + 
                           scaled_serve_speed + 
                           court_side + previous_serve_is_body + 
                           Advantage + Ahead + Behind + Break +
                           (1 | server_name) +
                           (1 | returner_name), 
                         data = training_data, 
                         family = binomial)


names(summary(mixed_logit_fit))

summary(mixed_logit_fit)$varcor
summary(mixed_logit_fit)$coefficients

random_server_intercept = ranef(mixed_logit_fit)$server_name
rownames(random_server_intercept)

random_int_df <- data.frame(server = rownames(random_server_intercept),
                            intercept = random_server_intercept$`(Intercept)`)

random_int_df %>%
  arrange(desc(intercept)) %>%
  View()

y_pred_lmm <- predict(mixed_logit_fit,
                      newdata=training_data,
                      type="response",
                      allow.new.levels=TRUE)

(misclass_error_mm <- mean(ifelse(y_pred_lmm > 0.5, 0, 1) == training_data$is_body))


# Summary options/names of fitted mixed model:
#[1] "methTitle"    "objClass"     "devcomp"      "isLmer"       "useScale"     "logLik"      
#[7] "family"       "link"         "ngrps"        "coefficients" "sigma"        "vcov"        
#[13] "varcor"       "AICtab"       "call"         "residuals"    "fitMsgs"      "optinfo"  

summary(mixed_logit_fit)$varcor
qqnorm(residuals(mixed_logit_fit))
################################################################################
################################################################################
################################################################################




######################### 
# --GAMMM
#########################



# Fitting a model and plotting it:
#gamm4 or gamm?
gamm_model<- gamm4(data=training_data, 
             formula=is_body ~ as.factor(ServeNumber) + s( scaled_serve_speed) + 
               court_side + previous_serve_is_body + 
               Advantage +  Break + Ahead + Behind, random = ~ (1 + scaled_serve_speed|server_name))

y_pred_gamm <- predict(gamm_model$gam,
                       newdata=training_data,
                       type="response",
                       allow.new.levels=TRUE)

(misclass_error_gamm <- mean(ifelse(y_pred_gamm > 0.5, 0, 1) == training_data$is_body))








############################################
# EXTRA
# -- Add player height & handedness data
player_height_data <- read.csv('/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/official_atp_height_2020.csv')

full_slam_data <- slam_data %>%
  
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('server_name' = 'player_name')) %>%
  rename('server_handedness' = 'player_handedness') %>%
  
  left_join(player_height_data %>% select(player_handedness, player_name),
            by = c('returner_name' = 'player_name')) %>%
  rename('returner_handedness' = 'player_handedness') %>%
  filter(complete.cases(.))

