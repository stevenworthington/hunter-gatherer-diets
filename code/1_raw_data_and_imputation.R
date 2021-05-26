
# --------------------------------------------------------------------------------
# setup

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# install and load packages
packages <- c("mice", "tidyverse", "PerformanceAnalytics")
ipak(packages)

# global options
options(scipen = 20)

# path to working directory
base_path <- "~/Documents/IQSS/hunter-gatherer-diets"

# data directory
output_dir <- file.path(base_path, "data")
if (dir.exists(output_dir)){
  print("Directory already exists!")
} else {
  dir.create(output_dir)
}

# read data from "data" directory (place the data here)
dat <- read_csv(file.path(base_path, "data", "diet_data.csv"))


# --------------------------------------------------------------------------------
# clean data

# convert percentages to proportions and create factors
dat <- dat%>%
  mutate_at(vars(starts_with(c("hg","plant", "animal", "honey", "protein", 
                               "lipid", "fiber", "sugar", "carb"))), function(x) x/100) %>%
  mutate(total_reliability_fac = factor(total_reliability),
         latitude_fac = factor(latitude))

glimpse(dat)


# --------------------------------------------------------------------------------
# imputation model

# missing data patterns
md.pattern(dat)

# find collinear variables
mice:::find.collinear(dat)

# correlations among collinear variables
chart.Correlation(dat[, c("hg_weight_wet", "hg_weight_dry", 
                          "hg_kcal_wet", "hg_kcal_dry", "fiber_dry")])

# frequency distribution of the missing cases per variable
ini <- mice(dat, maxit = 0) 
table(ini$nmis)
ini$nmis

# clean up the methods
meth <- ini$method
meth[names(meth) %in% c("collection_days")] <- ""

# clean up the predictor matrix
pred <- ini$predictorMatrix
pred[, colnames(pred) %in% c("collection_days", 
                             "total_reliability", 
                             "latitude_fac")] <- 0   

# estimate the imputation model
imputed_dat <- mice(
  data = dat, 
  m = 50, 
  maxit = 10,
  seed = 02138, 
  predictorMatrix = pred, 
  method = meth,
  printFlag = FALSE
)

# check the summary
summary(imputed_dat)

# plot summary
stripplot(imputed_dat)

# save the imputation list
save(imputed_dat, 
     file = file.path(base_path, "data", "imputed_data.Rdata"),
     compress = "gzip")

