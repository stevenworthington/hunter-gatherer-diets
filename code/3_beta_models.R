
# --------------------------------------------------------------------------------
# setup

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# install and load packages
packages <- c("mice", "tidyverse", "brms")
ipak(packages)

# global options
options(scipen = 20)

# path to working directory
base_path <- "~/Documents/IQSS/hunter-gatherer-diets"

# load imputed data
load(file.path(base_path, "imputed_data.Rdata"))
imputed_dat

# models directory
output_dir <- file.path(base_path, "models")
if (dir.exists(output_dir)){
  print("Directory already exists!")
} else {
  dir.create(output_dir)
}


# --------------------------------------------------------------------------------
# Beta regression models (Bayesian estimation)

# set weakly informative priors
prior_beta <- c(
  set_prior("student_t(3, 0, 1)", class = "b"),
  set_prior("student_t(3, 0, 1)", class = "Intercept"),
  set_prior("gamma(0.01, 0.01)", class = "phi")
)

# HG weight ~ latitude Bayesian model 
beta_mod1 <- brm_multiple(
    hg_weight_overall ~ latitude_fac, 
    data = imputed_dat, 
    prior = prior_beta,
    family = Beta(link = "logit", link_phi = "log"),
    cores = 4, chains = 2,
    iter = 1e4, warmup = 2e3, thin = 5,
    control = list(adapt_delta = 0.99, 
                   max_treedepth = 20))
summary(beta_mod1)
pp_check(beta_mod1)
  
# HG kcal ~ latitude Bayesian model
beta_mod2 <- brm_multiple(
    hg_kcal_overall ~ latitude_fac, 
    data = imputed_dat, 
    prior = prior_beta,
    family = Beta(link = "logit", link_phi = "log"),
    cores = 4, chains = 2,
    iter = 1e4, warmup = 2e3, thin = 5,
    control = list(adapt_delta = 0.99, 
                   max_treedepth = 20))
summary(beta_mod2)
pp_check(beta_mod2)
  
# HG weight ~ total reliability Bayesian model
beta_mod3 <- brm_multiple(
    hg_weight_overall ~ total_reliability_fac, 
    data = imputed_dat, 
    prior = prior_beta,
    family = Beta(link = "logit", link_phi = "log"),
    cores = 4, chains = 2,
    iter = 1e4, warmup = 2e3, thin = 5,
    control = list(adapt_delta = 0.99, 
                   max_treedepth = 20))
summary(beta_mod3)
pp_check(beta_mod3)
  
# HG kcal ~ total reliability Bayesian model
beta_mod4 <- brm_multiple(
    hg_kcal_overall ~ total_reliability_fac, 
    data = imputed_dat, 
    prior = prior_beta,
    family = Beta(link = "logit", link_phi = "log"),
    cores = 4, chains = 2,
    iter = 1e4, warmup = 2e3, thin = 5,
    control = list(adapt_delta = 0.99, 
                   max_treedepth = 20))
summary(beta_mod4)
pp_check(beta_mod4)

# save beta regression models
save(beta_mod1, beta_mod2, beta_mod3, beta_mod4, 
     file = file.path(base_path, "models", "beta_regression.Rdata"), 
     compress = "gzip")

