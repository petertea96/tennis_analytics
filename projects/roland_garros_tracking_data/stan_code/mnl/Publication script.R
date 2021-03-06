## This script accompanies the paper, "The Multinomial Analysis of Behavior,"
## by Jeremy Koster and Richard McElreath. Included as a supplemental file,
## this script can be used in concert with the supplemental data to replicate
## all analyses in the paper. The annotations throughout this script are
## intended to provide clarifications that could be helpful to other 
## researchers who seek to use the Multilevel Multinomial Behavior Model
## and its extensions on analogous datasets.

## This script uses functions from three R packages and their dependencies:
## 1) rstan -- see quickstart instructions here: http://mc-stan.org/interfaces/rstan
## 2) rethinking -- see installation instructions: http://xcelab.net/rm/software/
## 3) chron -- available on CRAN
library(rstan)
library(rethinking)
library(chron)

## This code reads in the data frame.
## There should be 2793 observations of 14 variables.
setwd("/Users/petertea/tennis_analytics/projects/roland_garros_tracking_data/stan_code/mnl")
d <- read.csv (file = "./data.csv") ## Change this path as needed.
str(d)

orig_dat <- read.csv (file = "./data.csv")
## Later in this script, we will use the coerce_index function to turn our
## response variable, currently a factor, into an integer that serves as the
## outcome variable(s) in the STAN model. By changing the "reference" level to "z",
## we ensure that the reference level is last in alphabetical order and therefore
## becomes the reference category by default in integer format. There are 
## many possible alternatives to this approach that would equally ensure that
## the desired reference level is the last among K categories in the response.
levels(d$response)[levels(d$response)=="reference"] <- "z"

## Time as record was in 00:00 format whereas the "times" function in the 
## chron package requires 00:00:00 format. The following nested code
## first appends the seconds to the times, then converts them into times,
## then converts them to a vector of numbers that specific times as the
## proportion of a 24-hour day that has elapsed at the observed time. For
## example, noon yields a value of 0.5 whereas an observation as 6:00 PM
## yields a value of 0.75.
d$time_prop <- as.numeric(times(paste0(d$time,":00")))

## What follows is the preparation of data needed in the STAN model. In general,
## STAN requires categorical variables (including for higher-level random effects)
## to be formatted as sequential integers. It therefore lacks the convenience of
## packages such as lme4, which makes the necessary conversions of categorical 
## variables without effort by the applied researcher. From the rethinking package,
## the coerce_index function is designed to convert vectors into the integer format
## required by STAN.
## In general, we employ z-score transformation of our continuous variables. This
## is primarily to make the sampling more efficient. Note that standardization 
## is based on sample means and standard deviations, where the sample is all 
## observations in the dataset.
## Transformed data that correspond to the observational data are appended to
## the "d' data frame. We also create index variables (N, K, N_id, N_month, N_month)
## that will be necessary for the STAN model code. These latter variables index
## the number of observations, the number of response categories, and the number
## of units in the higher-level classifications (for the random effects).
N <- nrow(d) ## Number of observations
d$y <- coerce_index (d$response) ## Renaming response variable
K <- max(d$y) ## Number of response categories
d$id <- coerce_index (d$subject_id) ## Index of observed individuals
N_id <- max(d$id) ## Number of observed individuals
d$house_id <- coerce_index (d$house) ## Index of observed individuals' households
N_house <- max(d$house_id) ## Number of households
d$month_id <- d$month ## Index of months in which observations occurred
N_month <- max(d$month) ## Number of months (12)
d$age_z <- (d$age-mean(d$age))/sd(d$age) ## Standardized age of observed individuals
d$age_zq <-d$age_z^2 ## Quadratic transformation of standardized age
d$log_wealth <- log(d$wealth) ## This is an intermediate step prior to z-score transform.
d$wz <- (d$log_wealth-mean(d$log_wealth))/sd(d$log_wealth) ## Standardized wealth
d$sunday <- ifelse(d$day == "Sunday", 1, 0) ## Indicator variable for observations on Sunday
d$saturday <- ifelse(d$day == "Saturday", 1, 0) ## Indicator for obserations on Saturday
d$time_z <- (d$time_prop - mean(d$time_prop))/sd(d$time_prop) ## Standardized time of day
d$time_zq <- d$time_z^2 ## Quadratic transformation of standardized time of day
d$house_size_z <- (d$dynamic_house_size - mean(d$dynamic_house_size))/sd(d$dynamic_house_size) ## Standardized house size
d$rain_z <- (d$rain - mean(d$rain))/sd(d$rain) ## Standardized monthly rainfall

d %>% 
  group_by(subject_id) %>%
  summarise(avg_age = mean(age),
            age_unique = unique(age))


#######################################################################################################
## Data lists
## In the call to STAN, there is a "data' argument, which calls for a list of data to be used.
## We find it helpful to prepare lists in advance of the call to STAN, and what follows is
## data lists for each of the four models that we implement in this script.
## Note that we use suffixes to distinguish our models:
## (i) is the suffix for models with only individual-level random effects
## (ihm) is the suffix for models with random effects for individuals (i), household (h), and month (m)
## (iF) has individual-level random effects and the fixed effects
## (ihmF) has all three aforementioned random effects and the fixed effects
#######################################################################################################

dat_list_i <- list(
    K = K, # Number of response categories
    N = N, # Number of observations
    N_id = N_id, # Number of observed individuals
    y = d$y,
    id = d$id
)

dat_list_ihm <- list(
    K = K,
    N = N,
    N_id = N_id,
    N_house = N_house,
    N_month = N_month,
    y = d$y,
    id = d$id,
    house_id = d$house_id,
    month_id = d$month_id
)

dat_list_iF <- list(
    K = K,
    N = N,
    N_id = N_id,
    y = d$y,
    id = d$id,
    age_z = d$age_z,
    age_zq = d$age_zq,
    wz = d$wz,
    sunday = d$sunday,
    saturday = d$saturday,
    time_z <- d$time_z,
    time_zq <- d$time_zq,
    house_size_z <- d$house_size_z,
    rain_z <- d$rain_z
)

dat_list_ihmF <- list(
    K = K,
    N = N,
    N_id = N_id,
    N_house = N_house,
    N_month = N_month,
    y = d$y,
    id = d$id,
    house_id = d$house_id,
    month_id = d$month_id,
    age_z = d$age_z,
    age_zq = d$age_zq,
    wz = d$wz,
    sunday = d$sunday,
    saturday = d$saturday,
    time_z <- d$time_z,
    time_zq <- d$time_zq,
    house_size_z <- d$house_size_z,
    rain_z <- d$rain_z
)

#######################################################################################################
## The call to STAN also requires model code. Using the same suffixes as above, we generate the model
## code for the four models. We begin by stating the data, distinguishing between integers and "real"
## continuous variables. Those variables originating in the "d" data frame are appended with [N] in
## brackets to denote that there is a data point corresponding to each observation in our dataset.
## We note that STAN model code can be somewhat idiosyncratic and the order in which variables are
## defined in a block of code can be somewhat rigid. In the "generated quantities" block, for instance,
## all matrices must be defined prior to the generation of the correlations (i.e., the rhos for the
## random effects).
## Further annotations in the model code follow the dual slashes "//"
#######################################################################################################

model_code_i <- "
data{
    int N;
    int N_id;
    int y[N];
    int id[N];
    int K;
}
parameters{
    real a[K-1]; 						// intercepts for each behavior, minus reference category
    matrix[K-1,N_id] z_id;      		// matrix of standardized random effects
    vector<lower=0>[K-1] sigma_id;   	// stddev of random effects
    cholesky_factor_corr[K-1] L_Rho_id; // correlation matrix of random effects, Choleskey decomposition
}
transformed parameters{
    matrix[N_id,K-1] v_id;     					// matrix of scaled random effects
    v_id = (diag_pre_multiply(sigma_id,L_Rho_id) * z_id)';   // note transpose in this transformation
}
model{
	
    // priors for fixed effects, mean followed by standard deviation
    a ~ normal(0,1);

	// hyper-priors
    to_vector(z_id) ~ normal(0,1);
    sigma_id ~ exponential(1);
    L_Rho_id ~ lkj_corr_cholesky(2);

    // Likelihood function
    // This code sets up a function for each of the K-1 responses.
    // For each function (k), an intercept (a) is paramaterized along with 
    // a subject-level varying intercept (v_id). We use STAN's built-in categorical_logit
    // function for multinomial logistic regression.
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = a[k] +
            v_id[id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}

	// In this block, we generate the variance-covariance matrix of individual-level
	// random effects for the K-1 responses. We then calculate the correlation between
	// these effects, Rho_id, via a recomposition from the Cholesky matrix.
	// We also define a vector of length N for the log likelihood values, subsequently calling
	// STAN's categorical_logit_lpmf to generate the likelihood of each observation, conditional
	// on the model. Note that this step requires a repetition of the likelihood function, as above.
generated quantities{
    matrix[K-1,K-1] Rho_id;
    vector[N] log_lik;
    Rho_id = L_Rho_id * L_Rho_id';

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = a[k] +
            v_id[id[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

## The following models are a straigthforward extension of the model code, adapted to include
## two additional random effects, house and month.

model_code_ihm <- "
data{
    int N;
    int N_id;
    int N_house;
    int N_month;
    int y[N];
    int id[N];
    int house_id[N];
    int month_id[N];
    int K;
}
parameters{
    real a[K-1];                  // intercepts for each behavior
    matrix[K-1,N_id] z_id;      		// matrix of indiv-level standardized random effects
    vector<lower=0>[K-1] sigma_id;  	 // stddev of indiv-level random effects
    cholesky_factor_corr[K-1] L_Rho_id;  // correlation matrix of indiv-level random effects
    matrix[K-1,N_house] z_house;     		 // matrix of house-level standardized random effects
    vector<lower=0>[K-1] sigma_house;   // stddev of house-level random effects
    cholesky_factor_corr[K-1] L_Rho_house;         // correlation matrix of house-level random effects
    matrix[K-1,N_month] z_month;      // matrix of house-level standardized random effects
    vector<lower=0>[K-1] sigma_month;   // stddev of house-level random effects
    cholesky_factor_corr[K-1] L_Rho_month;         // correlation matrix of house-level random effects   
}
transformed parameters{
    matrix[N_id,K-1] v_id;
    matrix[N_house,K-1] v_house;
    matrix[N_month,K-1] v_month;
    v_id = (diag_pre_multiply(sigma_id,L_Rho_id) * z_id)';
	v_house = (diag_pre_multiply(sigma_house,L_Rho_house) * z_house)';
	v_month = (diag_pre_multiply(sigma_month,L_Rho_month) * z_month)';
}
model{
    // priors
    a ~ normal(0,1);
    
    // hyper-priors
    to_vector(z_id) ~ normal(0,1);
    sigma_id ~ exponential(1);
    L_Rho_id ~ lkj_corr_cholesky(2);
    to_vector(z_house) ~ normal(0,1);
    sigma_house ~ exponential(1);
    L_Rho_house ~ lkj_corr_cholesky(2);
    to_vector(z_month) ~ normal(0,1);
    sigma_month ~ exponential(1);
    L_Rho_month ~ lkj_corr_cholesky(2);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] <- a[k] + v_id[id[i],k] + v_house[house_id[i],k] + v_month[month_id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    matrix[K-1,K-1] Rho_id;
    matrix[K-1,K-1] Rho_house;
    matrix[K-1,K-1] Rho_month;
    vector[N] log_lik;
    Rho_id = L_Rho_id * L_Rho_id';
    Rho_house = L_Rho_house * L_Rho_house';
    Rho_month = L_Rho_month * L_Rho_month';
    
        for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
           p[k] <- a[k] + v_id[id[i],k] + v_house[house_id[i],k] + v_month[month_id[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

## The following model adds fixed effects. The inclusion of additional parameters requires
## the naming of the beta parameters. We follow a convention of denoting the beta parameters
## by appending a lower-case "b" prefix to an upper-case letter that denotes the predictor.

model_code_iF <- "
data{
    int N;
    int N_id;
    int y[N];
    int id[N];
    real age_z[N];
    real age_zq[N];
    real wz[N];
    real sunday[N];
    real saturday[N];
    real time_z[N];
    real time_zq[N];
    real house_size_z[N];
    real rain_z[N];
    int K;
}
parameters{
    real a[K-1];                // intercepts for each behavior
    real bA[K-1];				// fixed effect for age
    real bQ[K-1];				// fixed effect for age squared
    real bW[K-1];				// fixed effect for wealth
    real bN[K-1];				// fixed effect for Sunday
    real bR[K-1];				// fixed effect for Saturday
    real bT[K-1];				// fixed effect for time of day
    real bTQ[K-1];				// fixed effect for time of day squared
    real bH[K-1];				// fixed effect for house size
    real bL[K-1];				// fixed effect for rain
    matrix[K-1,N_id] z_id;      // matrix of standardized random effects
    vector<lower=0>[K-1] sigma_id;   // stddev of random effects
    cholesky_factor_corr[K-1] L_Rho_id;         // correlation matrix of random effects
}
transformed parameters{
    matrix[N_id,K-1] v_id;      // matrix of scaled random effects
    v_id = (diag_pre_multiply(sigma_id,L_Rho_id) * z_id)';
}
model{
    
    // priors
    a ~ normal(0,1);
    bA ~ normal(0,1);
    bQ ~ normal(0,1);
    bW ~ normal(0,1);
    bN ~ normal(0,1);
    bR ~ normal(0,1);
    bT ~ normal(0,1);
    bTQ ~ normal(0,1);
    bH ~ normal(0,1);
    bL ~ normal(0,1);

    // hyper-prior
    to_vector(z_id) ~ normal(0,1);
    sigma_id ~ exponential(1);
    L_Rho_id ~ lkj_corr_cholesky(2);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = a[k] + bA[k] * age_z[i] + bQ[k] * age_zq[i] + bW[k] * wz[i] + bN[k] * sunday[i] + bR[k] * saturday[i] + bT[k] * time_z[i] + bTQ[k] * time_zq[i] + bH[k] * house_size_z[i] + bL[k] * rain_z[i] +
        v_id[id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    matrix[K-1,K-1] Rho_id;
    vector[N] log_lik;
    Rho_id = L_Rho_id * L_Rho_id';
    
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = a[k] + bA[k] * age_z[i] + bQ[k] * age_zq[i] + bW[k] * wz[i] + bN[k] * sunday[i] + bR[k] * saturday[i] + bT[k] * time_z[i] + bTQ[k] * time_zq[i] + bH[k] * house_size_z[i] + bL[k] * rain_z[i] +
        v_id[id[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

model_code_ihmF <- "
data{
    int N;
    int N_id;
    int N_house;
    int N_month;
    int y[N];
    int id[N];
    int house_id[N];
    int month_id[N];
    real age_z[N];
    real age_zq[N];
    real wz[N];
    real sunday[N];
    real saturday[N];
    real time_z[N];
    real time_zq[N];
    real house_size_z[N];
    real rain_z[N];
    int K; // number of categories
}
parameters{
    real a[K-1];                  // intercepts for each behavior
    real bA[K-1];				  // fixed effect for age
    real bQ[K-1];				// fixed effect for age squared
    real bW[K-1];				// fixed effect for wealth
    real bN[K-1];				// fixed effect for Sunday
    real bR[K-1];				// fixed effect for Saturday
    real bT[K-1];				// fixed effect for time of day
    real bTQ[K-1];				// fixed effect for time of day squared
    real bH[K-1];				// fixed effect for house size
    real bL[K-1];				// fixed effect for rain
    matrix[K-1,N_id] z_id;      		// matrix of indiv-level standardized random effects
    vector<lower=0>[K-1] sigma_id;  	 // stddev of indiv-level random effects
    cholesky_factor_corr[K-1] L_Rho_id;  // correlation matrix of indiv-level random effects
    matrix[K-1,N_house] z_house;     		 // matrix of house-level standardized random effects
    vector<lower=0>[K-1] sigma_house;   // stddev of house-level random effects
    cholesky_factor_corr[K-1] L_Rho_house;         // correlation matrix of house-level random effects
    matrix[K-1,N_month] z_month;      // matrix of house-level standardized random effects
    vector<lower=0>[K-1] sigma_month;   // stddev of house-level random effects
    cholesky_factor_corr[K-1] L_Rho_month;         // correlation matrix of house-level random effects   
}
transformed parameters{
    matrix[N_id,K-1] v_id;      // matrix of scaled random effects
    matrix[N_house,K-1] v_house;
    matrix[N_month,K-1] v_month;
    v_id = (diag_pre_multiply(sigma_id,L_Rho_id) * z_id)';
	v_house = (diag_pre_multiply(sigma_house,L_Rho_house) * z_house)';
	v_month = (diag_pre_multiply(sigma_month,L_Rho_month) * z_month)';
}
model{
    // priors
    a ~ normal(0,1);
    bA ~ normal(0,1);
    bQ ~ normal(0,1);
    bW ~ normal(0,1);
    bN ~ normal(0,1);
    bR ~ normal(0,1);
    bT ~ normal(0,1);
    bTQ ~ normal(0,1);
    bH ~ normal(0,1);
    bL ~ normal(0,1);
    
    // hyper-prior
    to_vector(z_id) ~ normal(0,1);
    sigma_id ~ exponential(1);
    L_Rho_id ~ lkj_corr_cholesky(2);
    to_vector(z_house) ~ normal(0,1);
    sigma_house ~ exponential(1);
    L_Rho_house ~ lkj_corr_cholesky(2);
    to_vector(z_month) ~ normal(0,1);
    sigma_month ~ exponential(1);
    L_Rho_month ~ lkj_corr_cholesky(2);

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] <- a[k] + bA[k] * age_z[i] + bQ[k] * age_zq[i] + bW[k] * wz[i] + bN[k] * sunday[i] + bR[k] * saturday[i] + bT[k] * time_z[i] + bTQ[k] * time_zq[i] + bH[k] * house_size_z[i] + bL[k] * rain_z[i] +  v_id[id[i],k] + v_house[house_id[i],k] + v_month[month_id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}
generated quantities{
    matrix[K-1,K-1] Rho_id;
    matrix[K-1,K-1] Rho_house;
    matrix[K-1,K-1] Rho_month;
    vector[N] log_lik;
    Rho_id = L_Rho_id * L_Rho_id';
    Rho_house = L_Rho_house * L_Rho_house';
    Rho_month = L_Rho_month * L_Rho_month';
    
        for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
           p[k] <- a[k] + bA[k] * age_z[i] + bQ[k] * age_zq[i] + bW[k] * wz[i] + bN[k] * sunday[i] + bR[k] * saturday[i] + bT[k] * time_z[i] + bTQ[k] * time_zq[i] + bH[k] * house_size_z[i] + bL[k] * rain_z[i] + v_id[id[i],k] + v_house[house_id[i],k] + v_month[month_id[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
"

#######################################################################################################
## What follows is the call to STAN for each of the four models. Prior to the call, we define
## (1) starting values for the fixed effects, (2) the variance of the random effects for each of the
## K-1 responses, (3) the variance-covariance matrix of the random effects for the K-1 responses, and
## (4) the unit-by-unit matrix of standardized random effects.
## In each case, we opt for uninformative starting values, which are then indexed by the init object
## that is supplied to the model call.
## We also supply values for the number of chains -- see McElreath's Statistical Rethinking for
## guidance on the number of chains and other considerations in calls to STAN, including the
## adapt_delta argument, which dictates the proportion of the proposed samples that must be within
## acceptable bounds. Raising this number toward 1 has benefits of refinining the estimator during
## warmup, but the tradeoff is that warmup requires more time as a result.
## Note that we retain the use of suffixes to distinguish between models for these auxiliary objects
## and quantities.
#######################################################################################################

################################################
## Model i

start_i <- list (
	a = rep(0,K-1),
	sigma_id = rep(1,K-1),
	L_Rho_id = diag(K-1),
	z_id = matrix(0,nrow=K-1,ncol=N_id)
)

n_chains_i <- 3
init_i <- list()
for ( i in 1:n_chains_i ) init_i[[i]] <- start_i

## We define a model fit object (mfit_i in this case), as is common with other model fitting functions
## in R.
mfit_i <- stan ( model_code=model_code_i , data=dat_list_i , chains=n_chains_i , cores= n_chains_i , warmup=1000, iter=2000, init=init_i , control = list(adapt_delta = 0.95))

## The precis function is from the rethinking package, and it provides summary information about
## the posterior samples. We specify 96% credibility intervals as an acknowledgement of the 
## historical circumstance that led 0.05 to become the accepted threshold for statistical 
## significance.
precis(mfit_i, depth = 2, prob = .96)

## Model results can be exported by creating an object from the precis output, then
## exporting the object, which we accomplish via the write.csv function. We include a file
## path as a possible example of the destination folder.
mfit_i_out <- precis(mfit_i, depth = 2, prob = .96)
write.csv(mfit_i_out@output, file = "~/Downloads/mfit.i.csv")


################################################
## Model ihm

start_ihm <- list (
	a = rep(0,K-1),
	sigma_id = rep(1,K-1),
	L_Rho_id = diag(K-1),
	z_id = matrix(0,nrow=K-1,ncol=N_id),
	sigma_house = rep(1,K-1),
	L_Rho_house = diag(K-1),
	z_house = matrix(0,nrow=K-1,ncol=N_house),
	sigma_month = rep(1,K-1),
	L_Rho_month = diag(K-1),
	z_month = matrix(0,nrow=K-1,ncol=N_month)
)

n_chains_ihm <- 3
init_ihm <- list()
for ( i in 1:n_chains_ihm ) init_ihm[[i]] <- start_ihm

mfit_ihm <- stan ( model_code=model_code_ihm , data=dat_list_ihm , chains=n_chains_ihm , cores= n_chains_ihm , warmup=1000, iter=2000, init=init_ihm , control = list(adapt_delta = 0.95))

precis(mfit_ihm, depth = 2, prob = .96)

mfit_ihm_out <- precis(mfit_ihm, depth = 2, prob = .96)
write.csv(mfit_ihm_out@output, file = "~/Downloads/mfit.ihm.csv")


################################################
## Model iF

start_iF <- list(
	a = rep(0,K-1),
	bA = rep(0,K-1),
	bQ = rep(0,K-1),
	bW = rep(0,K-1),
	bN = rep(0,K-1),
	bR = rep(0,K-1),
	bT = rep(0,K-1),
	bTQ = rep(0,K-1),
	bH = rep(0,K-1),
	bL = rep(0,K-1),
	sigma_id = rep(1,K-1),
	L_Rho_id = diag(K-1),
	z_id = matrix(0,nrow=K-1,ncol=N_id)
)

n_chains_iF <- 3
init_iF <- list()
for ( i in 1:n_chains_iF ) init_iF[[i]] <- start_iF

mfit_iF <- stan( model_code=model_code_iF , data=dat_list_iF , chains=n_chains_iF , cores= n_chains_iF , warmup=1000, iter=2000, init=init_iF , control = list(adapt_delta = 0.95))

precis(mfit_iF, depth = 2, prob = .96)

mfit_iF_out <- precis(mfit_iF, depth = 2, prob = .96)
write.csv(mfit_iF_out@output, file = "~/Downloads/mfit.iF.csv")


################################################
## Model ihmF

start_ihmF <- list(
	a = rep(0,K-1),
	bA = rep(0,K-1),
	bQ = rep(0,K-1),
	bW = rep(0,K-1),
	bN = rep(0,K-1),
	bR = rep(0,K-1),
	bT = rep(0,K-1),
	bTQ = rep(0,K-1),
	bH = rep(0,K-1),
	bL = rep(0,K-1),
	sigma_id = rep(1,K-1),
	L_Rho_id = diag(K-1),
	z_id = matrix(0,nrow=K-1,ncol=N_id),
	sigma_house = rep(1,K-1),
	L_Rho_house = diag(K-1),
	z_house = matrix(0,nrow=K-1,ncol=N_house),
	sigma_month = rep(1,K-1),
	L_Rho_month = diag(K-1),
	z_month = matrix(0,nrow=K-1,ncol=N_month)
)

n_chains_ihmF <- 3 
init_ihmF <- list()
for ( i in 1:n_chains_ihmF ) init_ihmF[[i]] <- start_ihmF

mfit_ihmF <- stan( model_code=model_code_ihmF , data=dat_list_ihmF , chains=n_chains_ihmF , cores= n_chains_ihmF , warmup=1000, iter=2000, init=init_ihmF , control = list(adapt_delta = 0.95))

precis(mfit_ihmF, depth = 2, prob = .96)

mfit_ihmF_out <- precis(mfit_ihmF, depth = 2, prob = .96)
write.csv(mfit_ihmF_out@output, file = "~/Downloads/mfit.ihmF.csv")


#######################################################################################################
## Model comparison using WAIC
## Employing the compare function from the rethinking package, we compare the fit of our four models
## using the Watanabe-Akaike information criterion.
#######################################################################################################
models <- compare (mfit_i, mfit_ihm, mfit_iF, mfit_ihmF)
plot(models)

#######################################################################################################
## The first step toward plotting predicted probabilities of our fixed effects is to extract the
## posterior samples using the extract.samples function in the STAN package.
## Note that we extract samples from our first fixed effects model, mfit_iF
#######################################################################################################

post <- extract.samples(mfit_iF)

## We can visually inspect the individual-level random effects by creating a new object, v_est.
## The pairs function in the base R package allows us to visualize the pairwise correlation of the
## random effects. Because our examination of the model output (using the precis function) showed
## a positive correlation between gold panning (the 6th response category) and hunting (the 7th
## response category), we plot those two sets of random effects for a closer examination.
## Subsequently, the dens function allows us to see the posterior distribution of the correlation
## between those categories.
v_est <- apply(post$v_id,2:3,median)
pairs(v_est)
plot(v_est[,6],v_est[,7])
dens(post$Rho[,6,7])

#######################################################################################################
## Multinomial version of link function for model iF
## Much like the link function in the rethinking package, the following function can be used to
## generate predictions for a customized data frame. Conventionally, researchers allow one of the
## predictors to vary while holding other predictors at a constant value (often the sample mean).
## The script begins by defining a sequence length that should correspond to the dimensions of
## data frame that is created to generate the predictions. We use 100 in this case, but other
## values are possible.
## We define additional quantities at the beginning of the function, and the function derives
## the values from the posterior. For example, K is the number of response categories (14), as before.
## The quantity, ns, is the number of samples.
## The function relies on an intermediate step, the creation of ptemp, which is tied to the likelihood
## function. Essentially, the function works by taking the supplied values for each of the i rows
## in the newly created data frame, then multipying those values by the value of the parameter in each
## sample from the posterior. In other words, for each of the samples from the posterior, the 
## parameters vary, which are used to generate predicted probabilities for each of the supplied
## combinations of values in the new data frame that we will create. We can subsequently 
## use the distribution of predicted values to understand the effets of the values we supply.
## It is common for researchers to average over the random effects. In other words, predicted
## probabilies are based only on the fixed effects, not accounting for the random effects.
## To accomplish that, our function requires users to supply a 0 for the random effects. By contrast,
## one can include random effects by supplying the integer that corresponds to the unit of interest
## in the higher-level classification. The varying intercept for that unit will then be incorporated
## as part of the predicted probability.
#######################################################################################################

seq.length <- 100

link.mn <- function( data ) {
    K <- dim(post$v_id)[3] + 1
    ns <- dim(post$v_id)[1]
    if ( missing(data) ) stop( "BOOM: Need data argument" )
	n <- seq.length

    softmax2 <- function(x) {
        x <- max(x) - x
        exp(-x)/sum(exp(-x))
    }

    p <- list()

    for ( i in 1:n ) {
        p[[i]] <- sapply( 1:K , function(k) {
            if ( k < K ) {
                ptemp <- post$a[,k] + 
                         post$bA[,k] * data$age_z[i] + 
                         post$bQ[,k] * data$age_zq[i] + 
                         post$bW[,k] * data$wz[i] + 
                         post$bN[,k] * data$sunday[i] + 
                         post$bR[,k] * data$saturday[i] + 
                         post$bT[,k] * data$time_z[i] + 
                         post$bTQ[,k] * data$time_zq[i] + 
                         post$bH[,k] * data$house_size_z[i] +
                         post$bL[,k] * data$rain_z[i]
                if ( data$id[i]>0 ) ptemp <- ptemp + post$v_id[,data$id[i],k]
            } else {
                ptemp <- rep(0,ns)
            }
            return(ptemp)
        })
        ## The values are converted to probabilities using the softmax function
        ## which ensures that the predicted values across categories sum to
        ## 100% probabilities.
        for ( s in 1:ns ) p[[i]][s,] <- softmax2( p[[i]][s,] )
    }
    return(p)
}


#######################################################################################################
## Generating predictions across the range of ages in the sample

## In addition to the posterior predictions, we develop scatterplots that include the empirical data.
## That is, we present the aggregated proportions for each individual subject -- the proportion of
## observations in which they were conducting the activities that comprise our response variable.
## To facilitate that calculation of those proportions, we use the response vector in our
## data frame, d. The for loop creates an additional vector in the data frame for each of the 
## categories in our response variable. Then for each observation in the data set, a 1 is added to
## the vector if the individual is conducting the designated activity. Otherwise, a zero is added.
## We include a prefix, "r_", for these new vectors as a reminder to ourselves that these are 
## based on the response variable. The prefix is not necessary. An examination of the vectors
## created by this for loop should make it clear what is being accomplished.
for(t in unique(d$response)) {d[paste("r_",t,sep="")] <- ifelse(d$response==t,1,0)}

## To create the aggregated proportions, we use the aggregate functions from the R base package.
agg <- aggregate ( cbind (r_agriculture, r_domestic, r_finca, r_firewood, r_fishing, r_gold, r_hunting, r_livestock, r_manufacture, r_other_work, r_school, r_steady_job, r_wage, r_z) ~ id + age_z, data = d, FUN = mean)

## The following generates a vector of ages which we use to generate predicted probabilities.
## We use the seq function to generate a sequence across the range of ages in the empirical data.
## Note that the length of this vector is seq.length, as defined above.
age_seq <- seq (from= min(d$age_z), to= max(d$age_z), length.out = seq.length)

## We create a data frame from age_seq and its second order polynomial, holding most of predictors
## at their sample mean. The exception to that is we specify a time of 8:00 for time of day
## in order to effect a more sensible alignment with the empirical data. In other words, because
## the mean value for time of day is standardized at mid-day, which is the peak time for labor,
## the predicted values when time is set to its standardized mean (0) are considerably higher
## than the empirically observed allocation of time to labor, which included many observation
## at non-peak times.
## Also, as we noted earlier in the notes about the multinomial link function, we set "id" to 
## zero in order to average over the random effects.
pred_dat <- data.frame(
    id = 0 ,
    age_z = age_seq,
    age_zq = age_seq^2,
    wz = 0,
    sunday = 0,
    saturday = 0,
    time_z = ((1/3)-mean(d$time_prop))/sd(d$time_prop),
    time_zq = (((1/3)-mean(d$time_prop))/sd(d$time_prop))^2,
    house_size_z = 0,
    rain_z = 0
)

## The following an object with 100 predicted probabilities (as in seq.length) corresponding
## to the different values of standardized ages that were supplied in the pred_dat data frame,
## as repeated for each of the K response categories.
p <- link.mn ( pred_dat )

## The following calculates the mean of the predicted samples.
p_mean <- sapply( 1:length(p) , function(i) apply(p[[i]],2,mean) )

## Note that the PI function embedded in the plot calls below is what generates
## the prediction intervals around the means. The PI function is from the rethinking package.
## We leave the prediction intervals at their default probability of 0.89, but we note that
## an argument call be supplied that changes this value. See help(PI) for details.

## We intend to place tick marks on the x-axis at 15-year intervals across our age range,
## so we define those ages and then calculate their values on the scale of standardized ages.
preferred.ages <- c(15,30,45,60)
labels.at <- (preferred.ages - mean(d$age))/sd(d$age)

## The quartz device on Mac operating systems can be supplied with preferred heights
## and widths for the plotting region. This code would need to be modified for other plot devices.
quartz(height = 6 , width = 10)
par(mfrow=c(4,4), mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

## The following plots on the multi-panel quartz device display the means and prediction intervals
## for each of the K response categories. The intervals are displayed with the shade function
## from the rethinking package. Other arguments are from the graphics functionality in the base R
## package.
plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.48) , xaxt = "n", main = "1) Agriculture", ylab = "Probability", xlab = "", cex.main = .9)
for ( k in 1:1 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_agriculture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim = c(0,0.12) , xaxt = "n", main = "2) Domestic chores", ylab = "", xlab = "", cex.main = .9)
for ( k in 2:2 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_domestic )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.40) , xaxt = "n", main = "3) Staying at finca", ylab = "", xlab = "", cex.main = .9)
for ( k in 3:3 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_finca )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.10) , xaxt = "n", main = "4) Firewood", ylab = "", xlab = "", cex.main = .9)
for ( k in 4:4 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_firewood )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.12) , xaxt = "n" , main = "5) Fishing" , ylab = "Probability", xlab = "", cex.main = .9)
for ( k in 5:5 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_fishing )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.35) , xaxt = "n" , main = "6) Gold panning" , ylab = "", xlab = "", cex.main = .9)
for ( k in 6:6 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_gold )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.12) , xaxt = "n" , main = "7) Hunting" , ylab = "", xlab = "", cex.main = .9)
for ( k in 7:7 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_hunting )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.06) , xaxt = "n" , main = "8) Livestock" , ylab = "", xlab = "", cex.main = .9)
for ( k in 8:8 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_livestock )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.28) , xaxt = "n" , main = "9) Manufacture" , ylab = "Probability", xlab = "", cex.main = .9)
for ( k in 9:9 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_manufacture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.15) , xaxt = "n" , main = "10) Other Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 10:10 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_other_work )
axis( side=1 , at=labels.at , labels=F )

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.20) , xaxt = "n" , main = "11) School" , ylab = "", xlab = "", cex.main = .9)
for ( k in 11:11 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_school )
axis( side=1 , at=labels.at , labels=preferred.ages )

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.4) , xaxt = "n" , main = "12) Steady Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 12:12 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_steady_job )
axis( side=1 , at=labels.at , labels=preferred.ages )

plot( NULL , xlim=c(-1.25, 2.4) , ylim=c(0,.22) , xaxt = "n" , main = "13) Wage Labor" , ylab = "Probability", xlab = "", cex.main = .9)
for ( k in 13:13 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_wage )
axis( side=1 , at=labels.at , labels=preferred.ages )

plot( NULL, xlim = c(-1.25, 2.4), ylim = c(0,.9), xaxt = "n", main = "14) Non-Work Reference Level", cex.main = .9, xlab = "Age", ylab = "Proportion of Time")
for ( k in 14:14 ) {
    lines( age_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , age_seq )
}
points(agg$age_z, agg$r_z)
axis( side=1 , at=labels.at , labels=preferred.ages )

mtext(text="Probability of Behavior",side=2,line=1,outer=TRUE)


## We turn off the quartz plotting device prior to starting the next multi-panel figure.
dev.off()

## Because the script uses the the same link.mn function to plot predictions by time of day,
## as a precautionary measure, we remove the objects that we will create using a different 
## data frame of supplied values.
rm(p)
rm(p_mean)
rm(p_PI)
rm(labels.at)

#######################################################################################################
## Generating predictions by day -- weekdays versus Saturday and Sunday
## The following (verbose) code displays posterior predictions for a data frame in which
## we hold most covariates at their mean values while examining mid-morning predictions on
## weekdays, Saturday, and Sunday, respectively. The time is set for 8:00 AM.
#######################################################################################################
pred_dat_day <- data.frame(
    id = 0 ,
    age_z = 0,
    age_zq = 0,
    wz = 0,
    sunday = c(0,0,1),
    saturday = c(0,1,0),
    time_z = ((1/3)-mean(d$time_prop))/sd(d$time_prop),
    time_zq = (((1/3)-mean(d$time_prop))/sd(d$time_prop))^2,
    house_size_z = 0,
    rain_z = 0
)

seq.length <- 3 ## We need to alter the value for seq.length to match the size of the new data frame
	## We will need to change this value back to the value above (100) when again specifying
	## data frames across a range of a continuous predictor variable.

p <- link.mn (pred_dat_day)

p_mean <- sapply( 1:length(p) , function(i) apply(p[[i]],2,mean) )

labels.at <- c(0,1,2)

# Multi-panel figure showing each behavior
quartz(height = 6 , width = 10)
par(mfrow=c(4,4), mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.3) , xaxt = "n", main = "1) Agriculture", ylab = "", xlab = "", cex.main = .9)
for ( k in 1:1 ) {
    p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim = c(0,0.05) , xaxt = "n", main = "2) Domestic chores", ylab = "", xlab = "", cex.main = .9)
for ( k in 2:2 ) {
    p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.16) , xaxt = "n", main = "3) Staying at finca", ylab = "", xlab = "", cex.main = .9)
for ( k in 3:3 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.07) , xaxt = "n", main = "4) Firewood", ylab = "", xlab = "", cex.main = .9)
for ( k in 4:4 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.05) , xaxt = "n" , main = "5) Fishing" , ylab = "", xlab = "", cex.main = .9)
for ( k in 5:5 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.10) , xaxt = "n" , main = "6) Gold panning" , ylab = "", xlab = "", cex.main = .9)
for ( k in 6:6 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.3) , xaxt = "n" , main = "7) Hunting" , ylab = "", xlab = "", cex.main = .9)
for ( k in 7:7 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.04) , xaxt = "n" , main = "8) Livestock" , ylab = "", xlab = "", cex.main = .9)
for ( k in 8:8 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.16) , xaxt = "n" , main = "9) Manufacture" , ylab = "", xlab = "", cex.main = .9)
for ( k in 9:9 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.05) , xaxt = "n" , main = "10) Other Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 10:10 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.008) , xaxt = "n" , main = "11) School" , ylab = "", xlab = "", cex.main = .9)
for ( k in 11:11 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = c ("Weekday", "Saturday", "Sunday"))

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.05) , xaxt = "n" , main = "12) Steady Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 12:12 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = c ("Weekday", "Saturday", "Sunday"))

plot( NULL , xlim=c(-.5,2.5) , ylim=c(0,.12) , xaxt = "n" , main = "13) Wage Labor" , ylab = "", xlab = "", cex.main = .9)
for ( k in 13:13 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = c ("Weekday", "Saturday", "Sunday"))
	
plot(NULL, xlim=c(-.5,2.5) , ylim = c(0,.9), xaxt = "n", main = "14) Non-Work Reference Level", cex.main = .9, xlab = "Time of day", ylab = "")
for ( k in 14:14 ) {
p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) ) 
	segments(0,p_PI[1,1],0,p_PI[2,1])
	segments(1,p_PI[1,2],1,p_PI[2,2])
	segments(2,p_PI[1,3],2,p_PI[2,3])
    points( c(0,1,2) , p_mean[k,], pch = 21, bg = "white")
    }
	axis( side = 1, at = labels.at, labels = c ("Weekday", "Saturday", "Sunday"))

mtext(text="Probability of Behavior",side=2,line=1,outer=TRUE)

dev.off()

#######################################################################################################
## Distribution of differences of "other work" by day of the week"
## As noted in the paper, researchers are often interested in testing for differences among multiple
## categorical predictor variables. The example in the focus on the category, "other work," which
## seems to show increases in frequency on Sunday. To test for differences, we calculate the
## differences between the predicted probabilities for each sample in the posterior. Then we examine
## the distribution of those differences.
## As a reminder, all other covariates are held constant at the values above. Because all parameters
## in generalized linear models are dependent on each other, it is possible that supplying alternative
## values for the other covariates would result in different 
#######################################################################################################

## The "p" object created by the link.mn function contains three matrices, one for each categorical
## distinction: weekdays, Saturday, and Sunday. The following code extracts a separate matrix for
## each category.
p_weekday <- p[[1]]
p_saturday <- p[[2]]
p_sunday <- p[[3]]

## Each matrix contains the predicted values for each of the 14 behavioral categories. We are
## specifically interested in the tenth category, "other work." So we extract that vector from the 
## matrix for each contrast.
p_weekday_10 <- p_weekday[,10]
p_saturday_10 <- p_saturday[,10]
p_sunday_10 <- p_sunday[,10]

## The following code first calculates the differences between the predicted probabilities of
## "other work" on Sunday and Saturday (we subtract Saturday from Sunday because we saw above
## that the mean probability for Sunday was higher than Saturday). The second function, PI,
## is from the rethinking package, and it computes the highest posterior intervals from the
## simulated probabilities. We again set the probability mass at 0.96, but this is arbitrary.
sunday_saturday_diff <- p_sunday_10 - p_saturday_10
PI(sunday_saturday_diff, prob = .96) ## Does not encompass zero

## We repeat for other contrasts, first comparing Sunday to weekdays:
sunday_weekday_diff <- p_sunday_10 - p_weekday_10
PI(sunday_weekday_diff, prob = .96) # Encompasses zero

## And finally, we contrast weekdays to Saturdays:
weekday_saturday_diff <- p_weekday_10 - p_saturday_10
PI(weekday_saturday_diff, prob = .96) # Encompasses zero

## (Note that the usual caveats about conducting multiple post-hoc tests apply in this case, too)

## A visualization of the density of differences potentially provides a better sense of the
## contrasts:

quartz(height = 3.5, width = 10)
par(mfrow=c(1,3))
dens(sunday_saturday_diff, show.zero = T, main = "Sunday - Saturday")
dens(sunday_weekday_diff, show.zero = T, main = "Sunday - Weekday")
dens(weekday_saturday_diff, show.zero = T, main = "Weekday - Saturday")

## This figure shows that we can be confident that the probability of "other work" is greater
## on Sunday than on Saturday. The difference is not great, roughly 1.5% to 2% greater, but it
## is consistent across the posterior samples. Other contrasts reveal marginally less support,
## but although the differences do not meet conventional thresholds, we see little evidence to
## conclude that there is not distinction between weekdays and weekend days.

dev.off ()

rm(seq.length)
rm(p)
rm(p_mean)
rm(p_PI)
rm(labels.at)



#######################################################################################################
## Generating predictions by time of day
## The following set of plots follows the code that we used for the age plots above. For the sake
## of plotting empirical points, we bin the observations into half-hour intervals (called time_slot in
## the original data frame), then standardize this variable to put it on the same scale as the
## standardized time_z variable created above.
#######################################################################################################

d$time_slot_z <- (d$time_slot - mean(d$time_slot)) / sd(d$time_slot)

aggtime <- aggregate ( cbind (r_z, r_school, r_agriculture, r_wage, r_hunting, r_fishing, r_steady_job, r_manufacture, r_domestic, r_livestock, r_gold, r_other_work, r_firewood, r_finca) ~ time_slot_z, data = d, FUN = mean)

seq.length <- 100 #Restoring a sequence length appropriate for continuous variables

time_seq <- seq(from= min(d$time_z) , to= max(d$time_z), length.out = seq.length)

pred_dat_t <- data.frame(
    id = 0,
    age_z = 0,
    age_zq = 0,
    wz = 0,
    sunday = 0,
    saturday = 0,
 	time_z = time_seq,
    time_zq = time_seq^2 ,
    house_size_z = 0,
    rain_z = 0
)

p <- link.mn( pred_dat_t )

# posterior means, cases in columns
p_mean <- sapply( 1:length(p) , function(i) apply(p[[i]],2,mean) )

# plot time on horizontal, a trend for each behavior

# Define preferred time
preferred.time <- c(
((1/3)-mean(d$time_prop))/sd(d$time_prop),
((1/2)-mean(d$time_prop))/sd(d$time_prop),
((2/3)-mean(d$time_prop))/sd(d$time_prop)
)
labels.at <- preferred.time

# Multi-panel figure showing each behavior
quartz(height = 6 , width = 10)
par(mfrow=c(4,4), mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.42) , xaxt = "n", main = "1) Agriculture", ylab = "", xlab = "", cex.main = .9)
for ( k in 1:1 ) {
    lines( time_seq , p_mean[k,] )
    p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_agriculture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim = c(0,0.06) , xaxt = "n", main = "2) Domestic chores", ylab = "", xlab = "", cex.main = .9)
for ( k in 2:2 ) {
    lines( time_seq , p_mean[k,] )
    p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_domestic )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.20) , xaxt = "n", main = "3) Staying at finca", ylab = "", xlab = "", cex.main = .9)
for ( k in 3:3 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_finca )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.10) , xaxt = "n", main = "4) Firewood", ylab = "", xlab = "", cex.main = .9)
for ( k in 4:4 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_firewood )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.05) , xaxt = "n" , main = "5) Fishing" , ylab = "", xlab = "", cex.main = .9)
for ( k in 5:5 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_fishing )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.2) , xaxt = "n" , main = "6) Gold panning" , ylab = "", xlab = "", cex.main = .9)
for ( k in 6:6 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_gold )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.10) , xaxt = "n" , main = "7) Hunting" , ylab = "", xlab = "", cex.main = .9)
for ( k in 7:7 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_hunting )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.06) , xaxt = "n" , main = "8) Livestock" , ylab = "", xlab = "", cex.main = .9)
for ( k in 8:8 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_livestock )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.16) , xaxt = "n" , main = "9) Manufacture" , ylab = "", xlab = "", cex.main = .9)
for ( k in 9:9 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_manufacture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.10) , xaxt = "n" , main = "10) Other Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 10:10 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_other_work )
axis( side=1 , at=labels.at , labels= F)

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.06) , xaxt = "n" , main = "11) School" , ylab = "", xlab = "", cex.main = .9)
for ( k in 11:11 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_school )
axis( side=1 , at=labels.at , labels= c ("8:00", "12:00", "16:00") )

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.1) , xaxt = "n" , main = "12) Steady Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 12:12 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_steady_job )
axis( side=1 , at=labels.at , labels= c ("8:00", "12:00", "16:00") )

plot( NULL , xlim=c(-1.7,1.7) , ylim=c(0,.20) , xaxt = "n" , main = "13) Wage Labor" , ylab = "", xlab = "", cex.main = .9)
for ( k in 13:13 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_wage )
axis( side=1 , at=labels.at , labels= c ("8:00", "12:00", "16:00") )

plot(NULL, xlim = c(-1.7, 1.7), ylim = c(0,.9), xaxt = "n", main = "14) Non-Work Reference Level", cex.main = .9, xlab = "Time of day", ylab = "")
for ( k in 14:14 ) {
    lines( time_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , time_seq )
}
points(aggtime$time_slot_z, aggtime$r_z)
axis( side=1 , at=labels.at , labels= c ("8:00", "12:00", "16:00"))

mtext(text="Probability of Behavior",side=2,line=1,outer=TRUE)

dev.off()

rm(p)
rm(p_mean)
rm(p_PI)
rm(labels.at)


#######################################################################################################
## Extracting posterior samples for model ihmF
## For an additional set of plots, we examine the predicted effects of household wealth on behavior.
## In the paper, we noted that including the corresponding higher-level random effects (e.g., house
## effects) when there are predictors at that level is particularly important, so we first extract
## samples from model mfit_ihmF, which included random effects for house and month along with the 
## subject-level random effects.
## We show how the methods used above can also be used to examine the pairwise relationships
## among the random effects across the different response categories.
#######################################################################################################

post_ihmF <- extract.samples(mfit_ihmF)
v_est_ihmF <- apply(post_ihmF$v_id,2:3,median)
pairs(v_est_ihmF)
v_est_house_ihmF <- apply(post_ihmF$v_house,2:3,median)
pairs(v_est_house_ihmF)
v_est_month_ihmF <- apply(post_ihmF$v_month,2:3,median)
pairs(v_est_month_ihmF)

#######################################################################################################
## We present a modified multinomial link function that accommodates the additional random effects.
## To avoid confusion, we name this function after the model to which it corresponds. The primary
## differences involve the additional if statements.
## As a general point, note that this function could be further modifed to be suitable for models
## with a reduced set of fixed effect covariates by deleting them from the formula.
#######################################################################################################

seq.length <- 100

link.mn.ihmF <- function( data ) {
    K <- dim(post_ihmF$v_id)[3] + 1
    ns <- dim(post_ihmF$v_id)[1]
    if ( missing(data) ) stop( "BOOM: Need data argument" )
	n <- seq.length

    softmax2 <- function(x) {
        x <- max(x) - x
        exp(-x)/sum(exp(-x))
    }

    p_ihmF <- list()

    for ( i in 1:n ) {
        p_ihmF[[i]] <- sapply( 1:K , function(k) {
            if ( k < K ) {
                ptemp_ihmF <- post_ihmF$a[,k] + 
                         post_ihmF$bA[,k] * data$age_z[i] + 
                         post_ihmF$bQ[,k] * data$age_zq[i] + 
                         post_ihmF$bW[,k] * data$wz[i] + 
                         post_ihmF$bN[,k] * data$sunday[i] + 
                         post_ihmF$bR[,k] * data$saturday[i] + 
                         post_ihmF$bT[,k] * data$time_z[i] + 
                         post_ihmF$bTQ[,k] * data$time_zq[i] + 
                         post_ihmF$bH[,k] * data$house_size_z[i] +
                         post_ihmF$bL[,k] * data$rain_z[i]
                if ( data$id[i]>0 ) ptemp_ihmF <- ptemp_ihmF + post_ihmF$v_id[,data$id[i],k]
                if ( data$house_id[i]>0 ) ptemp_ihmF <- ptemp_ihmF + post_ihmF$v_house[,data$house_id[i],k]
                if ( data$month_id[i]>0 ) ptemp_ihmF <- ptemp_ihmF + post_ihmF$v_month[,data$month_id[i],k]
            } else {
                ptemp_ihmF <- rep(0,ns)
            }
            return(ptemp_ihmF)
        })
        
        for ( s in 1:ns ) p_ihmF[[i]][s,] <- softmax2( p_ihmF[[i]][s,] )
    }#i
    
    return(p_ihmF)
}


#######################################################################################################
## Predicted probabilities by household wealth
#######################################################################################################

## One consideration when using the aggregate function for a household-level variable is that one
## one of the males in our dataset spent time in two separate households (respectively occupied by
## his divorced mother and father). In the estimation above, it is not problematic to work with
## the separate values of wealth, depending on the household where he was staying at the time of the 
## observation (our house size variable also fluctuates over time). The aggregate function, however, 
## essentailly treats this male as two separate individuals. In order to keep the number of aggregated
## units equal to the number of observed individuals, we employ a workaround where we duplicate
## the data frame and then change the value of wealth for this individual to the average wealth
## of the two households he occupied during the study period. This change affects only the depiction
## of this individual's data on the scatterplot, not the predicted effects of wealth.
d.w <- d
d.w$wz_modified <- ifelse(d.w$subject_id == 2, -1.47027, d.w$wz)

agg.w <- aggregate ( cbind (r_agriculture, r_domestic, r_finca, r_firewood, r_fishing, r_gold, r_hunting, r_livestock, r_manufacture, r_other_work, r_school, r_steady_job, r_wage, r_z) ~ id + wz_modified, data = d.w, FUN = mean)

wealth_seq <- seq(from= min(d$wz) , to= max(d$wz), length.out = seq.length)

pred_dat_w_ihmF <- data.frame(
    id = 0,
    house_id = 0,
    month_id = 0,
    age_z = 0,
    age_zq = 0,
    wz = wealth_seq,
    sunday = 0 ,
    saturday = 0,
    time_z = ((1/3)-mean(d$time_prop))/sd(d$time_prop),
    time_zq = (((1/3)-mean(d$time_prop))/sd(d$time_prop))^2,
    house_size_z = 0,
    rain_z = 0
)

p_ihmF <- link.mn.ihmF(pred_dat_w_ihmF)

p_mean_ihmF <- sapply( 1:length(p_ihmF) , function(i) apply(p_ihmF[[i]],2,mean) )

preferred.wealth <- c(-2, -1, 0, 1, 2)
labels.at <- (preferred.wealth - mean(d$wz))/sd(d$wz)

quartz(height = 6 , width = 10)
par(mfrow=c(4,4), mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.48) , xaxt = "n", main = "1) Agriculture", ylab = "", xlab = "", cex.main = .9)
for ( k in 1:1 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_agriculture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim = c(0,0.12) , xaxt = "n", main = "2) Domestic chores", ylab = "", xlab = "", cex.main = .9)
for ( k in 2:2 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg$r_domestic )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.40) , xaxt = "n", main = "3) Staying at finca", ylab = "", xlab = "", cex.main = .9)
for ( k in 3:3 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_finca )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.10) , xaxt = "n", main = "4) Firewood", ylab = "", xlab = "", cex.main = .9)
for ( k in 4:4 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_firewood )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.12) , xaxt = "n" , main = "5) Fishing" , ylab = "", xlab = "", cex.main = .9)
for ( k in 5:5 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_fishing )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.36) , xaxt = "n" , main = "6) Gold panning" , ylab = "", xlab = "", cex.main = .9)
for ( k in 6:6 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_gold )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.16) , xaxt = "n" , main = "7) Hunting" , ylab = "", xlab = "", cex.main = .9)
for ( k in 7:7 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_hunting )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.06) , xaxt = "n" , main = "8) Livestock" , ylab = "", xlab = "", cex.main = .9)
for ( k in 8:8) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_livestock )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.28) , xaxt = "n" , main = "9) Manufacture" , ylab = "", xlab = "", cex.main = .9)
for ( k in 9:9 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_manufacture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.15) , xaxt = "n" , main = "10) Other Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 10:10 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_other_work )
axis( side=1 , at=labels.at , labels=F )

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.20) , xaxt = "n" , main = "11) School" , ylab = "", xlab = "", cex.main = .9)
for ( k in 11:11 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_school )
axis( side=1 , at=labels.at , labels=preferred.wealth )

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.4) , xaxt = "n" , main = "12) Steady Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 12:12 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_steady_job )
axis( side=1 , at=labels.at , labels=preferred.wealth )

plot( NULL , xlim=c(-2.5, 2.6) , ylim=c(0,.22) , xaxt = "n" , main = "13) Wage Labor" , ylab = "", xlab = "", cex.main = .9)
for ( k in 13:13 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_wage )
axis( side=1 , at=labels.at , labels=preferred.wealth )

plot(NULL, xlim = c(-2.5, 2.6), ylim = c(0,.9), xaxt = "n", main = "14) Non-Work Reference Level", cex.main = .9, xlab = "Wealth", ylab = "")
for ( k in 14:14 ) {
    lines( wealth_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , wealth_seq)
}
points(agg.w$wz_modified, agg.w$r_z)
axis( side=1 , at=labels.at , labels=preferred.wealth )

mtext(text="Probability of Behavior",side=2,line=1,outer=TRUE)

dev.off()
rm(p_ihmF)
rm(p_mean_ihmF)
rm(p_PI_ihmF)
rm(labels.at)

#######################################################################################################
## Predicted probabilities by household size
#######################################################################################################

## A consideration when aggregating the data for household size is that household size fluctutates
## dynamically throughout the study period. For the purpose of the scatterplots, we therefore rely
## on an additional variable, "static_house_size," which best represents the size of households,
## focusing on the presence of "core" residents across the study period, including days when
## observations were not conducted.
## We have the same issue to resolve, as noted above, with an individual who switched between
## households. We therefore create a separate data frame in which we can modify the value for
## this individual, converting it to the average house size of the two households.

d.h <- d
d.h$hz <- (d.h$static_house_size - mean(d$dynamic_house_size))/sd(d$dynamic_house_size)
	## Note that we standardized relative to the values of the sample mean and sd for the 
	## dynamic house size, which is the variable in the models.
d.h$hz_modified <- ifelse(d.h$subject_id == 2, -0.2180537, d.h$hz)

agg.h <- aggregate ( cbind (r_agriculture, r_domestic, r_finca, r_firewood, r_fishing, r_gold, r_hunting, r_livestock, r_manufacture, r_other_work, r_school, r_steady_job, r_wage, r_z) ~ id + hz_modified, data = d.h, FUN = mean)

size_seq <- seq(from= min(d$house_size_z) , to= max(d$house_size_z), length.out = seq.length)

pred_dat_h_ihmF <- data.frame(
    id = 0,
    house_id = 0,
    month_id = 0,
    age_z = 0,
    age_zq = 0,
    wz = 0,
    sunday = 0 ,
    saturday = 0,
    time_z = ((1/3)-mean(d$time_prop))/sd(d$time_prop),
    time_zq = (((1/3)-mean(d$time_prop))/sd(d$time_prop))^2,
    house_size_z = size_seq,
    rain_z = 0
)

p_ihmF <- link.mn.ihmF(pred_dat_h_ihmF)

p_mean_ihmF <- sapply( 1:length(p_ihmF) , function(i) apply(p_ihmF[[i]],2,mean) )

preferred.size <- c(-2, -1, 0, 1, 2)
labels.at <- (preferred.size - mean(d$house_size_z))/sd(d$house_size_z)

quartz(height = 6 , width = 10)
par(mfrow=c(4,4), mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.48) , xaxt = "n", main = "1) Agriculture", ylab = "", xlab = "", cex.main = .9)
for ( k in 1:1 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_agriculture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim = c(0,0.12) , xaxt = "n", main = "2) Domestic chores", ylab = "", xlab = "", cex.main = .9)
for ( k in 2:2 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_domestic )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.40) , xaxt = "n", main = "3) Staying at finca", ylab = "", xlab = "", cex.main = .9)
for ( k in 3:3 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_finca )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.10) , xaxt = "n", main = "4) Firewood", ylab = "", xlab = "", cex.main = .9)
for ( k in 4:4 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_firewood )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.12) , xaxt = "n" , main = "5) Fishing" , ylab = "", xlab = "", cex.main = .9)
for ( k in 5:5 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_fishing )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.3) , xaxt = "n" , main = "6) Gold panning" , ylab = "", xlab = "", cex.main = .9)
for ( k in 6:6 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_gold )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.12) , xaxt = "n" , main = "7) Hunting" , ylab = "", xlab = "", cex.main = .9)
for ( k in 7:7 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_hunting )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.06) , xaxt = "n" , main = "8) Livestock" , ylab = "", xlab = "", cex.main = .9)
for ( k in 8:8) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_livestock )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.28) , xaxt = "n" , main = "9) Manufacture" , ylab = "", xlab = "", cex.main = .9)
for ( k in 9:9 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_manufacture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.15) , xaxt = "n" , main = "10) Other Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 10:10 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_other_work )
axis( side=1 , at=labels.at , labels=F )

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.16) , xaxt = "n" , main = "11) School" , ylab = "", xlab = "", cex.main = .9)
for ( k in 11:11 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_school )
axis( side=1 , at=labels.at , labels=preferred.size )

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.4) , xaxt = "n" , main = "12) Steady Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 12:12 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_steady_job )
axis( side=1 , at=labels.at , labels=preferred.size )

plot( NULL , xlim=c(-2, 2) , ylim=c(0,.2) , xaxt = "n" , main = "13) Wage Labor" , ylab = "", xlab = "", cex.main = .9)
for ( k in 13:13 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_wage )
axis( side=1 , at=labels.at , labels=preferred.size )

plot(NULL, xlim = c(-2, 2), ylim = c(0,.9), xaxt = "n", main = "14) Non-Work Reference Level", cex.main = .9, xlab = "Wealth", ylab = "Proportion of Time")
for ( k in 14:14 ) {
    lines( size_seq , p_mean_ihmF[k,])
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , size_seq)
}
points(agg.h$hz_modified, agg.h$r_z)
axis( side=1 , at=labels.at , labels=preferred.size )

mtext(text="Probability of Behavior",side=2,line=1,outer=TRUE)

dev.off()
rm(p_ihmF)
rm(p_mean_ihmF)
rm(p_PI_ihmF)
rm(labels.at)


#######################################################################################################
## Predicted probabilities by average monthly rainfall
## Finally, we show plots of the effect of rainfall on activity levels. Elsewhere, we have noted
## that modeling the effect of a higher-level predictor without including the corresponding
## random effect can substantially inflate the confidence in the predicted beta coefficient.
## We explore that possibility by plotting the predicted effect of rainfall from models that
## either do or do not include the varying intercepts for month. To generate these predictions,
## we use both the initial link function, link.mn, and the link function that we developed
## to generate predictions for model mfit_ihmF. We use suffixes as above to distinguish
## between the two sets of predicted values.
#######################################################################################################
aggrain <- aggregate ( cbind (r_z, r_school, r_agriculture, r_wage, r_hunting, r_fishing, r_steady_job, r_manufacture, r_domestic, r_livestock, r_gold, r_other_work, r_firewood, r_finca) ~ rain_z, data = d, FUN = mean)

rain_seq <- seq (from= min(d$rain_z) , to= max(d$rain_z), length.out = seq.length)

pred_dat_r <- data.frame(
    id = 0,
    age_z = 0,
    age_zq = 0,
    wz = 0,
    sunday = 0,
    saturday = 0,
    time_z = ((1/3)-mean(d$time_prop))/sd(d$time_prop),
    time_zq = (((1/3)-mean(d$time_prop))/sd(d$time_prop))^2,
    house_size_z = 0,
    rain_z = rain_seq
)

pred_dat_r_ihmF <- data.frame(
    id = 0,
    house_id = 0,
    month_id = 0,
    age_z = 0,
    age_zq = 0,
    wz = 0,
    sunday = 0,
    saturday = 0,
    time_z = -1,
    time_zq = 1,
    house_size_z = 0,
    rain_z = rain_seq
)

p <- link.mn( pred_dat_r )
p_ihmF <- link.mn.ihmF(pred_dat_r_ihmF)

p_mean <- sapply( 1:length(p) , function(i) apply(p[[i]],2,mean) )
p_mean_ihmF <- sapply( 1:length(p_ihmF) , function(i) apply(p_ihmF[[i]],2,mean) )

preferred.rain <- c(mean(d$rain_z)-sd(d$rain_z), mean(d$rain_z), mean(d$rain_z)+sd(d$rain_z))
labels.at <- preferred.rain
quartz(height = 6 , width = 10)
par(mfrow=c(4,4), mar=c(1,1.5,1,1.5) + 0.1, oma=c(1,2.5,1,0.5))

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.48) , xaxt = "n", main = "1) Agriculture", ylab = "", xlab = "", cex.main = .9)
for ( k in 1:1 ) {
    lines( rain_seq , p_mean[k,] )
    p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 1:1 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_agriculture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim = c(0,0.08) , xaxt = "n", main = "2) Domestic chores", ylab = "", xlab = "", cex.main = .9)
for ( k in 2:2 ) {
    lines( rain_seq , p_mean[k,] )
    p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 2:2 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_domestic )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.20) , xaxt = "n", main = "3) Staying at finca", ylab = "", xlab = "", cex.main = .9)
for ( k in 3:3 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 3:3 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_finca )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.10) , xaxt = "n", main = "4) Firewood", ylab = "", xlab = "", cex.main = .9)
for ( k in 4:4 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 4:4 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_firewood )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.06) , xaxt = "n" , main = "5) Fishing" , ylab = "", xlab = "", cex.main = .9)
for ( k in 5:5 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 5:5 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_fishing )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.2) , xaxt = "n" , main = "6) Gold panning" , ylab = "", xlab = "", cex.main = .9)
for ( k in 6:6 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 6:6 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_gold )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.10) , xaxt = "n" , main = "7) Hunting" , ylab = "", xlab = "", cex.main = .9)
for ( k in 7:7 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 7:7 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_hunting )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.06) , xaxt = "n" , main = "8) Livestock" , ylab = "", xlab = "", cex.main = .9)
for ( k in 8:8 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 8:8 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_livestock )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.28) , xaxt = "n" , main = "9) Manufacture" , ylab = "", xlab = "", cex.main = .9)
for ( k in 9:9 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 9:9 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_manufacture )
axis( side = 1, at = labels.at, labels = F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.15) , xaxt = "n" , main = "10) Other Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 10:10 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 10:10 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_other_work )
axis( side=1 , at=labels.at , labels= F)

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.08) , xaxt = "n" , main = "11) School" , ylab = "", xlab = "", cex.main = .9)
for ( k in 11:11 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 11:11 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_school )
axis( side=1 , at=labels.at , labels= c ("-1 SD", "mean", "1 SD") )

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.1) , xaxt = "n" , main = "12) Steady Work" , ylab = "", xlab = "", cex.main = .9)
for ( k in 12:12 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 12:12 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_steady_job )
axis( side=1 , at=labels.at , labels= c ("-1 SD", "mean", "1 SD") )

plot( NULL , xlim=c(-1.5, 1.85) , ylim=c(0,.20) , xaxt = "n" , main = "13) Wage Labor" , ylab = "", xlab = "", cex.main = .9)
for ( k in 13:13 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 13:13 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_wage )
axis( side=1 , at=labels.at , labels= c ("-1 SD", "mean", "1 SD") )

plot(NULL, xlim = c(-1.5, 1.85), ylim = c(0,.9), xaxt = "n", main = "14) Non-Work Reference Level", cex.main = .9, xlab = "Time of day", ylab = "")
for ( k in 14:14 ) {
    lines( rain_seq , p_mean[k,] )
  p_PI <- sapply( 1:length(p) , function(i) PI(p[[i]][,k]) )
    shade( p_PI , rain_seq )
}
for ( k in 14:14 ) {
    lines( rain_seq , p_mean_ihmF[k,], lty = 2 )
    p_PI_ihmF <- sapply( 1:length(p_ihmF) , function(i) PI(p_ihmF[[i]][,k]) )
    shade( p_PI_ihmF , rain_seq, col = col.alpha("purple") )
}
points(aggrain$rain_z, aggrain$r_z)
axis( side=1 , at=labels.at , labels= c ("-1 SD", "mean", "1 SD"))

mtext(text="Probability of Behavior",side=2,line=1,outer=TRUE)

dev.off()
