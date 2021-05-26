
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
load(file.path(base_path, "models", "Dirichlet_regression.Rdata"))

# figure directory
output_dir <- file.path(base_path, "figures")
if (dir.exists(output_dir)){
  print("Directory already exists!")
} else {
  dir.create(output_dir)
}


# --------------------------------------------------------------------------------
# Dirichlet regression figures

# --------------------------------------------------------------------------------
# 1) plant/animal/honey weight ~ reliability

# plot conditional effects
eff1 <- conditional_effects(Dir_mod1, categorical = TRUE)
eff1_df <- eff1[["total_reliability_fac:cats__"]]
eff1_df$cats <- eff1_df$cats__ %>% 
    as.character() %>% 
    gsub("(.*)\\_(.*)\\_.*", "\\1 \\2", x = .) %>% 
    StrCap() %>% 
    factor()
  
plot1 <- ggplot(eff1_df, aes(x = total_reliability_fac, y = estimate__, 
                             color = cats, group = cats)) +
    geom_line(size = 0.7, position = position_dodge(width = 0.3), linetype = "dotted") +
    geom_point(size = 3, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2,
                  position = position_dodge(width = 0.3)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Total reliability", y = "Predicted proportion") +
    theme_classic() +
    theme(legend.position = "top",
          legend.margin = margin(b = -0.2, unit = "cm"),
          panel.grid.major.y = element_line(size = 0.3, color="grey80")
          )
ggsave(plot1, file = file.path(base_path, "figures/Dir_plot1.pdf"), height = 4, width = 6)
    

# 2) plant/animal/honey weight ~ latitude 

# plot conditional effects
eff2 <- conditional_effects(Dir_mod2, categorical = TRUE)
eff2_df <- eff2[["latitude_fac:cats__"]]
eff2_df$cats <- eff2_df$cats__ %>% 
  as.character() %>% 
  gsub("(.*)\\_(.*)\\_.*", "\\1 \\2", x = .) %>% 
  StrCap() %>% 
  factor()
      
plot2 <- ggplot(eff2_df, aes(x = latitude_fac, y = estimate__, 
                             color = cats, group = cats)) +
    geom_line(size = 0.7, position = position_dodge(width = 0.3), linetype = "dotted") +
    geom_point(size = 3, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2,
                  position = position_dodge(width = 0.3)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_x_discrete(breaks = 0:5, 
                     labels = c("0-4 N/S", "5-9 N/S", "10-14 N/S", 
                                "15-19 N/S", "20-24 N/S", "25-29 N/S")) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Latitude", y = "Predicted proportion") +
    theme_classic() +
    theme(legend.position = "top",
          legend.margin = margin(b = -0.2, unit = "cm"),
          panel.grid.major.y = element_line(size = 0.3, color="grey80")
    )
ggsave(plot2, file = file.path(base_path, "figures/Dir_plot2.pdf"), height = 4, width = 6)


# 3) plant/animal/honey kcal ~ reliability

# plot conditional effects
eff3 <- conditional_effects(Dir_mod3, categorical = TRUE)
eff3_df <- eff3[["total_reliability_fac:cats__"]]
eff3_df$cats <- eff3_df$cats__ %>% 
  as.character() %>% 
  gsub("(.*)\\_(.*)\\_.*", "\\1 \\2", x = .) %>% 
  StrCap() %>% 
  factor()

plot3 <- ggplot(eff3_df, aes(x = total_reliability_fac, y = estimate__, 
                             color = cats, group = cats)) +
    geom_line(size = 0.7, position = position_dodge(width = 0.3), linetype = "dotted") +
    geom_point(size = 3, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2,
                  position = position_dodge(width = 0.3)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Total reliability", y = "Predicted proportion") +
    theme_classic() +
    theme(legend.position = "top",
          legend.margin = margin(b = -0.2, unit = "cm"),
          panel.grid.major.y = element_line(size = 0.3, color="grey80")
    )
ggsave(plot3, file = file.path(base_path, "figures/Dir_plot3.pdf"), height = 4, width = 6)


# 4) plant/animal/honey kcal ~ latitude

# plot conditional effects
eff4 <- conditional_effects(Dir_mod4, categorical = TRUE)
eff4_df <- eff4[["latitude_fac:cats__"]]
eff4_df$cats <- eff4_df$cats__ %>% 
  as.character() %>% 
  gsub("(.*)\\_(.*)\\_.*", "\\1 \\2", x = .) %>% 
  StrCap() %>% 
  factor()

plot4 <- ggplot(eff4_df, aes(x = latitude_fac, y = estimate__, 
                             color = cats, group = cats)) +
    geom_line(size = 0.7, position = position_dodge(width = 0.3), linetype = "dotted") +
    geom_point(size = 3, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2,
                  position = position_dodge(width = 0.3)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_x_discrete(breaks = 0:5, 
                     labels = c("0-4 N/S", "5-9 N/S", "10-14 N/S", 
                                "15-19 N/S", "20-24 N/S", "25-29 N/S")) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Latitude", y = "Predicted proportion") +
    theme_classic() +
    theme(legend.position = "top",
          legend.margin = margin(b = -0.2, unit = "cm"),
          panel.grid.major.y = element_line(size = 0.3, color="grey80")
    )
ggsave(plot4, file = file.path(base_path, "figures/Dir_plot4.pdf"), height = 4, width = 6)


# 5) plant/animal/honey weight ~ reliability

# plot conditional effects
eff5 <- conditional_effects(Dir_mod5, categorical = TRUE)
eff5_df <- eff5[["total_reliability_fac:cats__"]]
eff5_df$cats <- eff5_df$cats__ %>% 
  as.character() %>% 
  gsub("(.*)\\_(.*)\\_.*", "\\1 \\2", x = .) %>% 
  StrCap() %>% 
  factor()

plot5 <- ggplot(eff5_df, aes(x = total_reliability_fac, y = estimate__, 
                             color = cats, group = cats)) +
    geom_line(size = 0.7, position = position_dodge(width = 0.4), linetype = "dotted") +
    geom_point(size = 3, position = position_dodge(width = 0.4)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2,
                  position = position_dodge(width = 0.4)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Total reliability", y = "Predicted proportion") +
    # guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(legend.position = "top",
          legend.margin = margin(b = -0.2, unit = "cm"),
          panel.grid.major.y = element_line(size = 0.3, color="grey80")
    )
ggsave(plot5, file = file.path(base_path, "figures/Dir_plot5.pdf"), height = 4, width = 6)


# 6) plant/animal/honey weight ~ latitude

# plot conditional effects
eff6 <- conditional_effects(Dir_mod6, categorical = TRUE)
eff6_df <- eff6[["latitude_fac:cats__"]]
eff6_df$cats <- eff6_df$cats__ %>% 
  as.character() %>% 
  gsub("(.*)\\_(.*)\\_.*", "\\1 \\2", x = .) %>% 
  StrCap() %>% 
  factor()

plot6 <- ggplot(eff6_df, aes(x = latitude_fac, y = estimate__, 
                             color = cats, group = cats)) +
    geom_line(size = 0.7, position = position_dodge(width = 0.5), linetype = "dotted") +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2,
                  position = position_dodge(width = 0.5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    scale_x_discrete(breaks = 0:5, 
                     labels = c("0-4 N/S", "5-9 N/S", "10-14 N/S", 
                               "15-19 N/S", "20-24 N/S", "25-29 N/S")) +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Latitude", y = "Predicted proportion") +
    # guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(legend.position = "top",
          legend.margin = margin(b = -0.2, unit = "cm"),
          panel.grid.major.y = element_line(size = 0.3, color="grey80")
    )
ggsave(plot6, file = file.path(base_path, "figures/Dir_plot6.pdf"), height = 4, width = 6)  


#---------------------------------------------------------------------------------------
# save Dirichlet figures
 
save(plot1, plot2, plot3, plot4, plot5, plot6,
     file = file.path(base_path, "figures", "Dirichlet_figures.Rdata"),
     compress = "gzip")


