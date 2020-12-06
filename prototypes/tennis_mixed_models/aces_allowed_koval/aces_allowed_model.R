#install.packages('cmdstanr', 
#repos = c('https://mc-stan.org/r-packages/', getOption('repos')))
#cmdstanr::install_cmdstan()

library(cmdstanr)
library(data.table)

setwd("/Users/petertea/tennis_analytics/prototypes/tennis_mixed_models/aces_allowed_koval")

match_stats <- readRDS('all_match_stats.RDS')

match_stats$server_index <- as.numeric(factor(match_stats$server))
match_stats$receiver_index <- as.numeric(factor(match_stats$returner))

model <- cmdstan_model("aces_allowed.stan")	

stan_model_data <- list(
		N = nrow(match_stats),
		aces = match_stats$s_ace,
		serves = match_stats$s_svpt,
		server_index = match_stats$server_index,
		receiver_index = match_stats$receiver_index,
		P = max(match_stats$server_index)
		)
		
fitted_model <- model$variational(
		data = stan_model_data,
		tol_rel_obj = 0.001
	)
		

# Example summary for a parameter, in this case the 'beta_receiver' for the receiver effect on ace probability (on the logit scale)		
return_effects <- fitted_model$draws("beta_receiver")

# Posterior means
return_effects_means <- colMeans(return_effects)

# Returners with biggest negative effect on server
levels(factor(match_stats$server))[which(return_effects_means  < quantile(return_effects_means, .005))]
levels(factor(match_stats$server))[which.min(return_effects_means)]

levels(factor(match_stats$server))[order(return_effects_means, decreasing = FALSE)[1:16]]


# Example summary for a parameter, in this case the 'beta_server' for the server effect on ace probability (on the logit scale)		
serve_effects <- fitted_model$draws("beta_server")

# Posterior means
serve_effects_means <- colMeans(serve_effects)

# Returners with biggest negative effect on server
levels(factor(match_stats$server))[which(serve_effects_means  > quantile(serve_effects_means, .99))]
levels(factor(match_stats$server))[order(serve_effects_means, decreasing = TRUE)[1:16]]


# -- Changing scale of random effects
expit <- function(x) exp(x) / (1 + exp(x)) # Inverse logit

global_intercept <-fitted_model$draws('beta_intercept')

# -- Full effects include the intercept
full_return_logit_effect = (as.vector(global_intercept) + return_effects)
full_server_logit_effect = (as.vector(global_intercept) + serve_effects)


# Return effects in probability scale
full_return_effect_in_pr = expit(full_return_logit_effect) - expit(as.vector(global_intercept))
return_effects_pr_means = colMeans(full_return_effect_in_pr)

# -- Sanity Check
levels(factor(match_stats$server))[order(return_effects_pr_means, decreasing = FALSE)[1:16]]
levels(factor(match_stats$server))[which.min(return_effects_pr_means)]


# Serve effects in probability scale
full_serve_effect_in_pr = expit(full_server_logit_effect) - expit(as.vector(global_intercept))
serve_effects_pr_means = colMeans(full_serve_effect_in_pr)

levels(factor(match_stats$server))[order(serve_effects_pr_means, decreasing = TRUE)[1:16]]

# -- quantiles (95% Credible Intervals)
quants <- c(0.05, 0.95)

serve_effects_quantiles <- apply(full_serve_effect_in_pr , 2 , quantile ,
                                 probs = quants , na.rm = TRUE) %>%
  t()

return_effects_quantiles <- apply(full_serve_effect_in_pr, 2 , quantile ,
                                  probs = quants , na.rm = TRUE) %>%
  t()


marginal_return_effect_in_pr = expit(full_return_logit_effect)
marginal_return_effect_pr_means <- colMeans(marginal_return_effect_in_pr)

marginal_serve_effect_in_pr = expit(full_server_logit_effect)
marginal_serve_effect_pr_means <- colMeans(marginal_serve_effect_in_pr)

effects_dataframe <- tibble::tibble(player = levels(factor(match_stats$server)),
                                    avg_ace_hit_effect_pr = serve_effects_pr_means,
                                    ace_hit_low = serve_effects_quantiles[,1],
                                    ace_hit_high = serve_effects_quantiles[,2],
                                    avg_ace_allowed_effect_pr = return_effects_pr_means,
                                    ace_allowed_low = return_effects_quantiles[,1],
                                    ace_allowed_high = return_effects_quantiles[,2],
                                    avg_ace_hit_effect = serve_effects_means,
                                    avg_ace_allowed_effect = return_effects_means,
                                    marginal_ace_hit = marginal_serve_effect_pr_means,
                                    marginal_ace_allowed = marginal_return_effect_pr_means
                                    ) 

# -- Number matches played
player_num_matches_df <- match_stats %>%
  group_by(server) %>%
  summarise(num_matches = n()) %>%
  arrange(desc(num_matches))

effects_dataframe <- effects_dataframe %>%
  left_join(player_num_matches_df, by = c('player'='server'))


write.csv(effects_dataframe,
          'effects_dataframe.csv',
          row.names = FALSE)



# -- Best at hitting aces

effects_dataframe %>%
  arrange(desc(avg_ace_hit_effect_pr)) %>%
  head(12) %>%
  View()


# -- Best at preventing aces

effects_dataframe %>%
  arrange(avg_ace_allowed_effect_pr) %>%
  head(12) %>%
  View()

# -- Worst at preventing aces
levels(factor(match_stats$server))[order(serve_effects_pr_means, decreasing = FALSE)[1:50]]



effects_dataframe %>%
  filter(player == 'Felix Auger Aliassime')
