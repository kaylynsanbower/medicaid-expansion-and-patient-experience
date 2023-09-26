# clear
rm(list=ls())


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,cobalt) 

# Import cleaned data via "source" ----------------------------------------

source(here("Data-Code/1-Create_Medicaid_Expansion_Panel.R"))
source(here("Data-Code/1-Combine_Hospital_Data.R"))
source(here("Data-Code/1-Create_Yelp_Panel.R"))

# only keep what we need
rm(list= ls()[!(ls() %in% c('df_mcaid_annual','df_hosp_chars', 'df_revs'))])


df_hosp_chars$st_fips <- as.integer(df_hosp_chars$st_fips)
df_hosp_chars$mcrnum <- as.integer(df_hosp_chars$mcrnum)


# Combine all and add necessary variables 
chars_mcaid <- left_join(df_hosp_chars, df_mcaid_annual, by = c("year" = "year", "st_fips" = "fips"))



all_data <- left_join(df_revs, chars_mcaid, by = c("year" = "year", "mcrnum" = "mcrnum")) %>%
  filter(year>=2012 & year<=2018) %>%  
  subset(select=-c(sysid, mlocstcd, commty, mloczip, npinum, dtbeg, dtend, fisyr, divname)) %>% 
  drop_na(abbreviation) %>% 
  group_by(mcrnum) %>%
  mutate(min_year = min(year), 
         max_year = max(year), 
         relative_year = ifelse(is.na(exp_year), 2014, exp_year), 
         keep_in_samp = ifelse((min_year==2012|min_year==2013) & max_year>=relative_year,1,0)) %>%
  filter(keep_in_samp==1) %>% 
  replace_na(list(rv = 0, count = 0, ann_avg = 0)) %>% 
  mutate(not_rated = ifelse(count == 0, 1, 0))
  
# Write files to rds ------------------------------------------------------

write_rds(all_data, file = "data/output/combined-hospital-variables-cs.rds")

