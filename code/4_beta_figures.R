
# --------------------------------------------------------------------------------
# setup

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# install and load packages
packages <- c("mice", "tidyverse", "brms", "DescTools")
ipak(packages)

# global options
options(scipen = 20)

# path to working directory
base_path <- "~/Documents/IQSS/hunter-gatherer-diets"

# load fitted models
load(file.path(base_path, "models", "beta_regression.Rdata"))

# figure directory
output_dir <- file.path(base_path, "figures")
if (dir.exists(output_dir)){
  print("Directory already exists!")
} else {
  dir.create(output_dir)
}


# --------------------------------------------------------------------------------
# Beta regression figures 

# HG weight ~ latitude
beta_eff1 <- conditional_effects(beta_mod1a)
beta_eff1_df <- beta_eff1[["latitude_fac"]]
plot(beta_eff1)

# HG kcal ~ latitude
beta_eff2 <- conditional_effects(beta_mod2a)
beta_eff2_df <- beta_eff2[["latitude_fac"]]
plot(beta_eff2)

# HG weight ~ total reliability
beta_eff3 <- conditional_effects(beta_mod3a)
beta_eff3_df <- beta_eff3[["total_reliability_fac"]]
plot(beta_eff3)

# HG kcal ~ total reliability
beta_eff4 <- conditional_effects(beta_mod4a)
beta_eff4_df <- beta_eff4[["total_reliability_fac"]]
plot(beta_eff4)

# save beta regression figures
save(beta_eff1, beta_eff2, beta_eff3, beta_eff4, 
     file = file.path(base_path, "figures", "beta_figures.Rdata"),
     compress = "gzip")
     
     