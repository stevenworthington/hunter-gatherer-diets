
# --------------------------------------------------------------------------------
# setup

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# install and load packages
packages <- c("mice", "tidyverse", "DirichletReg", "brms")
ipak(packages)

# global options
options(scipen = 20)

# path to working directory
base_path <- "~/Documents/IQSS/hunter-gatherer-diets"

# models directory
output_dir <- file.path(base_path, "models")
if (dir.exists(output_dir)){
  print("Directory already exists!")
} else {
  dir.create(output_dir)
}


# --------------------------------------------------------------------------------
# Dirichlet regression (Bayesian estimation)

# --------------------------------------------------------------------------------
# 1) plant/animal/honey weight ~ reliability

# create matrix for responses
dat$Y1 <- as.matrix(dat[, c("plant_weight_overall", "animal_weight_overall", "honey_weight_overall")])

# transform away from zero and one
dat$Y_tran1 <- DR_data(dat$Y1, trafo = TRUE)

# set weakly informative priors
prior_dirichlet <- c(
  set_prior("student_t(3, 0, 2.5)", class = "b"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
  set_prior("gamma(0.01, 0.01)", class = "phi")
)

# Bayesian model (parameterization with reference group)
Dir_mod1 <- brm(
  Y_tran1 ~ total_reliability_fac, 
  data = dat, 
  prior = prior_dirichlet,
  family = dirichlet(link = "logit", link_phi = "log"),
  cores = 4, chains = 4,
  iter = 1e4, warmup = 2e3)
summary(Dir_mod1)

# --------------------------------------------------------------------------------
# 2) plant/animal/honey weight ~ latitude

# Bayesian model (parameterization with reference group)
Dir_mod2 <- brm(
  Y_tran1 ~ latitude_fac, 
  data = dat, 
  prior = prior_dirichlet,
  family = dirichlet(link = "logit", link_phi = "log"),
  cores = 4, chains = 4,
  iter = 1e4, warmup = 2e3)
summary(Dir_mod2)

# --------------------------------------------------------------------------------
# 3) plant/animal/honey kcal ~ reliability

# create matrix for responses
dat$Y2 <- as.matrix(dat[, c("plant_kcal_overall", "animal_kcal_overall", "honey_kcal_overall")])

# transform away from zero and one
dat$Y_tran2 <- DR_data(dat$Y2, trafo = TRUE)

# Bayesian model (parameterization with reference group)
Dir_mod3 <- brm(
  Y_tran2 ~ total_reliability_fac, 
  data = dat, 
  prior = prior_dirichlet,
  family = dirichlet(link = "logit", link_phi = "log"),
  cores = 4, chains = 4,
  iter = 1e4, warmup = 2e3)
summary(Dir_mod3)

# --------------------------------------------------------------------------------
# 4) plant/animal/honey kcal ~ latitude

# Bayesian model (parameterization with reference group)
Dir_mod4 <- brm(
  Y_tran2 ~ latitude_fac, 
  data = dat, 
  prior = prior_dirichlet,
  family = dirichlet(link = "logit", link_phi = "log"),
  cores = 4, chains = 4,
  iter = 1e4, warmup = 2e3)
summary(Dir_mod4)

# --------------------------------------------------------------------------------
# 5) plant/animal/honey weight ~ reliability

# create matrix for responses
dat$Y3 <- as.matrix(dat[, c("protein_overall", "lipid_overall", "fiber_overall", "sugar_overall", "carb_diff_overall")])

# transform away from zero and one
dat$Y_tran3 <- DR_data(dat$Y3, trafo = TRUE)

# Bayesian model (parameterization with reference group)
Dir_mod5 <- brm(
  Y_tran3 ~ total_reliability_fac, 
  data = dat, 
  prior = prior_dirichlet,
  family = dirichlet(link = "logit", link_phi = "log"),
  cores = 4, chains = 4,
  iter = 1e4, warmup = 2e3)
summary(Dir_mod5)

# --------------------------------------------------------------------------------
# 6) plant/animal/honey weight ~ latitude

# Bayesian model (parameterization with reference group)
Dir_mod6 <- brm(
  Y_tran3 ~ latitude_fac, 
  data = dat, 
  prior = prior_dirichlet,
  family = dirichlet(link = "logit", link_phi = "log"),
  cores = 4, chains = 4,
  iter = 1e4, warmup = 2e3)
summary(Dir_mod6)


# --------------------------------------------------------------------------------
# save beta regression models
save(Dir_mod1, Dir_mod2, Dir_mod3, Dir_mod4, Dir_mod5, Dir_mod6, 
     file = file.path(base_path, "models", "Dirichlet_regression.Rdata"), 
     compress = "gzip")

