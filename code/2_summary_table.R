
# --------------------------------------------------------------------------------
# setup

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# install and load packages
packages <- c("mice", "tidyverse")
ipak(packages)

# global options
options(scipen = 20)

# path to working directory
base_path <- "~/Documents/IQSS/hunter-gatherer-diets"

# load imputed data
load(file.path(base_path, "data", "imputed_data.Rdata"))
imputed_dat

# tables directory
output_dir <- file.path(base_path, "tables")
if (dir.exists(output_dir)){
  print("Directory already exists!")
} else {
  dir.create(output_dir)
}


# --------------------------------------------------------------------------------
# summary table

# extract completed datasets 
completed_dat <- complete(imputed_dat, action = "long")

# split completed datasets into a list
split_completed_dat <- with(completed_dat, split(completed_dat, .imp))

# omit irrelevant variables
split_completed_dat <- map(split_completed_dat, ~ select(., -.imp, -.id, -study, -total_reliability_fac, -latitude_fac))

# convert to matrices
split_completed_mat <- lapply(split_completed_dat, as.matrix)

# collapse over matrices
# test <- Reduce("+", split_completed_mat) / length(split_completed_mat)
# apply(simplify2array(split_completed_mat), MARGIN = 1:2, FUN = mean)
# apply(simplify2array(split_completed_mat), MARGIN = 1:2, FUN = sd)

# collapse over matrices
tab <- apply(simplify2array(split_completed_mat), MARGIN = 1:2, FUN = function(x) {
  paste(round(min(x, na.rm=TRUE), 2), 
        round(mean(x, na.rm=TRUE), 2), 
        round(max(x, na.rm=TRUE), 2)
  )
})

tab <- as.data.frame(tab)

# omit repeats if min/mean/max are identical
tab_clean <- function(variable) {
    strsplit(variable, split = " ") %>% 
    map(unique) %>% 
    map(paste, collapse = " ") %>%
    unlist()
}

tab <- map(tab, tab_clean)
tab <- map(tab, function(x) gsub("NA|NaN|Inf|-Inf", "", x))

tab <- as.data.frame(tab)

write.csv(tab, file = file.path(base_path, "tables", "Table1.csv"), row.names = FALSE)


