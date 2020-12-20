#################################################################################
#####       ---------- Cross - Validation Mixed Models ----------           #####
#################################################################################

# -- Load libraries
library(dplyr)
library(ggplot2)
library(mgcv)
library(lme4)
library(reshape)
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
  filter(Speed_KMH>0) %>%
  filter(year < 2019)

training_data <- cbind(training_data,
                       nnet::class.ind(training_data$server_pressure))


### Cross Validation on certain players?
training_data %>%
  group_by(server_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

players_of_interest <- c('Rafael Nadal', 'Kevin Anderson', 'Juan Martin del Potro',
                         'John Isner', 'Roger Federer', 'Milos Raonic', 'Kei Nishikori',
                         'Novak Djokovic', 'Dominic Thiem', 'Andy Murray', 
                         'Stan Wawrinka', 'Alexander Zverev', 'Nick Kyrgios',
                         'David Goffin', 'Denis Shapovalov', 'Diego Schwartzman',
                         'Andrey Rublev', 'Stefanos Tsitsipas', 'Daniil Medvedev', 
                         'David Ferrer', 'Vasek Pospisil', 'Felix Auger Aliassime',
                         'Alexander Bublik', 'Laslo Djere', 'Viktor Troicki',
                         'Matteo Berrettini')

cv_training_data <- training_data %>%
  filter(year != 2019)



# Number of iterations
m = 10
individual_brier_score_glm <- matrix(nrow = 10*length(unique(cv_training_data$server_name)),
                                     ncol = m)
individual_brier_score_mm <- matrix(nrow = 10*length(unique(cv_training_data$server_name)),
                                    ncol = m)
individual_brier_score_gamm <- matrix(nrow = 10*length(unique(cv_training_data$server_name)),
                                    ncol = m)

individual_misclassmat_glm <- matrix(nrow = 10*length(unique(cv_training_data$server_name)),
                                     ncol = m)
individual_misclassmat_mm <- matrix(nrow = 10*length(unique(cv_training_data$server_name)),
                                    ncol = m)
individual_misclassmat_gamm <- matrix(nrow = 10*length(unique(cv_training_data$server_name)),
                                    ncol = m)

overall_brier_score <- matrix(ncol = 3, nrow = m)
overall_misclass_rate <- matrix(ncol = 3, nrow = m)

cv_training_data$index <- 1:nrow(cv_training_data)
set.seed(824)

for(iter in 1:m){
  # Shuffle data
  cv_training_data <- cv_training_data[sample(nrow(cv_training_data)),]
  
  # Create validation set (1 observation from each group)
  validation_data <- cv_training_data %>%
    #filter(server_name %in% players_of_interest) %>%
    group_by(server_name) %>%
    slice(1:10)
  
  # Create Training data
  fit_data <- cv_training_data %>%
    filter(! (index %in% validation_data$index) )
  
  # Add scaled speed
  fit_data <- fit_data %>%
    mutate(scaled_serve_speed = (Speed_KMH - mean(Speed_KMH)) / sd(Speed_KMH) ) 
  
  validation_data$scaled_serve_speed <- (validation_data$Speed_KMH - mean(fit_data$Speed_KMH)) / sd(fit_data$Speed_KMH)
  
  # Fit GLM
  glm.fit <- glm(data=fit_data, 
                 formula=is_body ~ as.factor(ServeNumber) + scaled_serve_speed + 
                   court_side + previous_serve_is_body + 
                   Advantage +  Break + Ahead + Behind,
                 family = 'binomial')
  
  pred.prob.glm <- round(predict(glm.fit, newdata=validation_data, type="response"), digits=3)
  overall_brier_score_glm = mean((pred.prob.glm - validation_data$is_body)^2)
  individual_brier_scores_glm <- (pred.prob.glm - validation_data$is_body)^2
  
  individual_misclass_glm <- ifelse(pred.prob.glm > 0.5, 0, 1) == validation_data$is_body
  misclass_error_glm <- mean(ifelse(pred.prob.glm > 0.5, 0, 1) == validation_data$is_body)
  
  # -- Mixed model
  mixed_model <- glmer(formula = is_body ~ as.factor(ServeNumber) + 
                         scaled_serve_speed + 
                         court_side + previous_serve_is_body + 
                         Advantage + Ahead + Behind + Break +
                         (1  + scaled_serve_speed| server_name), 
                       data = fit_data, 
                       family = binomial(link = 'logit'))
  
  
  mm_predictions <- predict(mixed_model,
                            newdata=validation_data,
                            type="response",
                            allow.new.levels=TRUE)
  
  overall_brier_score_mm = mean((mm_predictions - validation_data$is_body)^2)
  individual_brier_scores_mm <- (mm_predictions - validation_data$is_body)^2
  
  individual_misclass_mm <- ifelse(mm_predictions > 0.5, 0, 1) == validation_data$is_body
  
  misclass_error_mm <- mean(ifelse(mm_predictions > 0.5, 0, 1) == validation_data$is_body)
  
  
  # GAMMs
  gamm_model<- gamm4(data=fit_data, 
                     formula=is_body ~ as.factor(ServeNumber) + s( scaled_serve_speed) + 
                       court_side + previous_serve_is_body + 
                       Advantage +  Break + Ahead + Behind, random = ~ (1 + scaled_serve_speed|server_name))
  y_pred_gamm <- predict(gamm_model$gam,
                         newdata=validation_data,
                         type="response")

  overall_brier_score_gamm = mean((y_pred_gamm - validation_data$is_body)^2)
  individual_brier_scores_gamm <- (y_pred_gamm - validation_data$is_body)^2
  
  individual_misclass_gamm <- ifelse(y_pred_gamm > 0.5, 0, 1) == validation_data$is_body
  misclass_error_gamm <- mean(ifelse(y_pred_gamm > 0.5, 0, 1) == validation_data$is_body)
  
  
  # Save results
  individual_brier_score_glm[,iter] <- individual_brier_scores_glm
  individual_misclassmat_glm[,iter] <- individual_misclass_glm
  
  individual_brier_score_mm[,iter] <- individual_brier_scores_mm 
  individual_misclassmat_mm[,iter] <- individual_misclass_mm
  
  individual_brier_score_gamm[,iter] <- individual_brier_scores_gamm 
  individual_misclassmat_gamm[,iter] <- individual_misclass_gamm
  
  overall_brier_score[iter,1] <- overall_brier_score_glm
  overall_brier_score[iter,2] <- overall_brier_score_mm
  overall_brier_score[iter,3] <- overall_brier_score_gamm
  
  overall_misclass_rate[iter,1] <- misclass_error_glm
  overall_misclass_rate[iter,2] <- misclass_error_mm
  overall_misclass_rate[iter,3] <- misclass_error_gamm
  
  print(paste('Done Iteration: ', iter, sep = ''))
  
}

# individual scores

test <- as.data.frame(individual_misclassmat_gamm)
test$server <- rep(unique(training_data$server_name)[order(unique(training_data$server_name))], each = 10)

test2 <- melt(test, id.vars = 'server')

individual_misclass_gamm_df <- test2 %>%
  group_by(server, variable) %>%
  summarise(misclass = mean(value)) %>%
  mutate(iteration = variable,
         method = 'gamm') %>%
  select(-variable)

individual_misclass_glmm_df <- as.data.frame(individual_misclassmat_mm) %>%
  mutate(server = rep(unique(training_data$server_name)[order(unique(training_data$server_name))], each = 10)) %>%
  melt(id.vars = 'server') %>%
  group_by(server, variable) %>%
  summarise(misclass = mean(value)) %>%
  mutate(iteration = variable,
         method = 'glmm') %>%
  select(-variable)

individual_misclass_glm_df <- as.data.frame(individual_misclassmat_glm) %>%
  mutate(server = rep(unique(training_data$server_name)[order(unique(training_data$server_name))], each = 10)) %>%
  melt(id.vars = 'server') %>%
  group_by(server, variable) %>%
  summarise(misclass = mean(value)) %>%
  mutate(iteration = variable,
         method = 'glm') %>%
  select(-variable)


# **********
misclass_individuals_df <- rbind(individual_misclass_glm_df,
                                 individual_misclass_glmm_df,
                                 individual_misclass_gamm_df)

write.csv(misclass_individuals_df,
          'misclass_individuals_df.csv', row.names = FALSE)

misclass_individuals_df %>%
  filter(server %in% cv_players_to_plot) %>%
  ggplot() +
  geom_boxplot(aes(x = server,
                   y = misclass,
                   fill = method)) +
  labs(y = 'Brier Score',
       fill = 'Model',
       x = '',
       title = ' Mixed Model Individual Predictive Brier Scores'
  )



individual_brier_glmm_df <- as.data.frame(individual_brier_score_mm) %>%
  mutate(server = rep(unique(training_data$server_name)[order(unique(training_data$server_name))], each = 10)) %>%
  melt(id.vars = 'server') %>%
  group_by(server, variable) %>%
  summarise(misclass = mean(value)) %>%
  mutate(iteration = variable,
         method = 'glmm') %>%
  select(-variable)

individual_brier_glm_df <- as.data.frame(individual_brier_score_glm) %>%
  mutate(server = rep(unique(training_data$server_name)[order(unique(training_data$server_name))], each = 10)) %>%
  melt(id.vars = 'server') %>%
  group_by(server, variable) %>%
  summarise(misclass = mean(value)) %>%
  mutate(iteration = variable,
         method = 'glm') %>%
  select(-variable)

individual_brier_gamm_df <- as.data.frame(individual_brier_score_gamm) %>%
  mutate(server = rep(unique(training_data$server_name)[order(unique(training_data$server_name))], each = 10)) %>%
  melt(id.vars = 'server') %>%
  group_by(server, variable) %>%
  summarise(misclass = mean(value)) %>%
  mutate(iteration = variable,
         method = 'gamm') %>%
  select(-variable)

# ***********
brier_individuals_df <- rbind(individual_brier_glm_df,
                              individual_brier_glmm_df,
                              individual_brier_gamm_df)

write.csv(brier_individuals_df,
          'brier_individuals_df.csv', row.names = FALSE)

brier_individuals_df %>%
  filter(method == 'glmm') %>%
  group_by(server) %>%
  summarise(brier_avg = mean(misclass)) %>%
  arrange(desc(brier_avg)) %>%
  View()
    
# Players vary in predictive ability... lets show the interesting ones
cv_players_to_plot <- c('Daniil Medvedev', 'Stefanos Tsitsipas', 'Roger Federer', 
                        'Rafael Nadal', 'Andy Murray', 'Novak Djokovic')

brier_ind_to_plot <- brier_individuals_df %>%
  filter(server %in% cv_players_to_plot) %>%
  mutate(last_name = stringr::str_extract(server, '[^ ]+$'))

brier_ind_to_plot$last_name <- factor(brier_ind_to_plot$last_name,
                                      levels = c('Medvedev', 'Tsitsipas', 
                                                 'Federer', 'Djokovic', 'Murray',
                                                 'Nadal'))
brier_ind_to_plot %>%
  ggplot() +
  geom_boxplot(aes(x = last_name,
                   y = misclass,
                   fill = method)) +
  labs(y = 'Brier Score',
       fill = 'Model',
       x = '',
       title = ' Mixed Model Individual Predictive Brier Scores'
  ) +
  plot_theme(family_font = 'Tahoma') +
  theme(axis.text.x = element_text(angle = 0))

ggsave('brier_individual.jpg',
       width=8.25, height=6,
       dpi = 410)

# -- Save and process CV results of GLM individual players??
head(individual_brier_score_glm)
colnames(individual_brier_score_glm) <- paste('iteration', 1:m, sep = '')

df_individual_brier_score_glm <- as.data.frame(individual_brier_score_glm)
df_individual_brier_score_glm$player <- unique(cv_training_data$server_name)[order(unique(cv_training_data$server_name))]
  
df_individual_brier_score_glm <- melt(df_individual_brier_score_glm,id.vars = 'player')

df_agg_individual_brier_score_glm <- df_individual_brier_score_glm %>%
  group_by(player, variable) %>%
  summarise(avg_brier_score = mean(value))

write.csv(individual_brier_score_glm,
          'individual_brier_score_glm.csv', row.names = FALSE)
write.csv(df_agg_individual_brier_score_glm,
          'df_agg_individual_brier_score_glm.csv', row.names = FALSE)


#############################################################
# -- Save and process CV results of GLMM individual players
colnames(individual_brier_score_mm) <- paste('iteration', 1:m, sep = '')

df_individual_brier_score_mm <- as.data.frame(individual_brier_score_mm)
df_individual_brier_score_mm$player <- unique(cv_training_data$server_name)[order(unique(cv_training_data$server_name))]

df_individual_brier_score_mm <- melt(df_individual_brier_score_mm,id.vars = 'player')

df_agg_individual_brier_score_mm <- df_individual_brier_score_mm %>%
  group_by(player, variable) %>%
  summarise(avg_brier_score = mean(value))

write.csv(individual_brier_score_mm,
          'individual_brier_score_mm.csv', row.names = FALSE)
write.csv(df_agg_individual_brier_score_mm,
          'df_agg_individual_brier_score_mm.csv', row.names = FALSE)




#############################################################
# -- Save and process CV results of GLM Aggregate
colnames(overall_brier_score) <- c('glm', 'glmm', 'gamm')
colnames(overall_misclass_rate) <- c('glm', 'glmm', 'gamm')

write.csv(overall_brier_score,
          'overall_brier_score.csv', row.names = FALSE)

write.csv(overall_misclass_rate,
          'overall_misclass_rate.csv', row.names = FALSE)


############################################################
# Plots

brier_score_df_transformed <- melt(overall_brier_score) %>%
  mutate(performance = 'brier',
         iteration = X1,
         model_name = X2) %>%
  select(iteration, model_name, performance, value)

missclass_df_transformed <- melt(overall_misclass_rate) %>%
  mutate(performance = 'misclassification',
         iteration = X1,
         model_name = X2) %>%
  select(iteration, model_name, performance, value)

overall_perforance_combined <- rbind(brier_score_df_transformed,
                                     missclass_df_transformed)

overall_perforance_combined$model_name <- factor(overall_perforance_combined$model_name, 
                                                 levels = overall_perforance_combined$model_name[c(2,3,1)])
ggplot(data = overall_perforance_combined) + 
  geom_boxplot(aes(x = performance,
                   y = value,
                   fill = model_name)) +
  labs(y = '',
       fill = 'Model',
       x = '',
       title = ' Mixed Model Overall Performances in Tennis'
        ) +
  plot_theme(family_font = 'Tahoma') +
  theme(axis.text.x = element_text(angle = 0))

ggsave('overall_perf.jpg',
       width=6.25, height=4,
       dpi = 410)


