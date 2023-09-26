# clear
rm(list=ls())


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,cobalt,
               panelView, tikz, ggplot, usmap, statebins) 

# set path for output to manuscript 
output.path <- "/Users/kaylynsanbower/Dropbox/Apps/Overleaf/Hospital Amenities (2023)/Tables_and_Figures/"

# Import cleaned data via "source" ----------------------------------------

source(here("Data-Code/1-Create_Medicaid_Expansion_Panel.R"))


# Visualize  --------------------------------------------------------------

# reformat for visualization
df_mcaid <- df_mcaid %>% mutate(year_qtr = str_c(year, qtr, sep="Q"))

df_mcaid_annual <- df_mcaid %>% filter(qtr==4)

df_mcaid_map <- df_mcaid_annual %>% mutate(exp_group = as.factor(ifelse(is.na(exp_year) | exp_year > 2018, 0, exp_year))) %>%
                                    subset(select=c(fips, abbreviation, exp_group)) %>%
                                    unique() %>%
                                    filter(abbreviation!="DC ")

df_mcaid_map$exp_group <- factor(df_mcaid_map$exp_group, levels = c("2014", "2015", "2016", "0"),
                                 labels = c("2014", "2015", "2016", "No Expansion"))

# create maps
map_colors <- c("#08306B", "#3b639e", "#6e96d1","#b9b7b4")

tikz(file = paste(output.path, "mcaid-expansion-map.tex", sep=""), width = 12.5, height = 8)
statebins(state_data = df_mcaid_map,
          state_col = "abbreviation",
          value_col = "exp_group",
          ggplot2_scale_function = scale_fill_brewer,
          round = T,
          dark_label = "white",
          font_size=5) +
  scale_fill_manual(values = map_colors, name = NULL) +
  theme_statebins(legend_position = c(0.21, 0.9)) +
  theme(legend.text=element_text(size=15),
        legend.key.size = unit(1, 'cm'),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.5, 'cm'))
dev.off()
