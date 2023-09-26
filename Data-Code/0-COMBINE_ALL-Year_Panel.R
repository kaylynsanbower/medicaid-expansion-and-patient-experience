# clear
rm(list=ls())


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,cobalt) 

# Import cleaned data via "source" ----------------------------------------

source(here("Data-Code/1-Create_Medicaid_Expansion_Panel.R"))
source(here("Data-Code/1-Combine_Hospital_Data.R"))
source(here("Data-Code/1-Create_Yelp_Panel.R"))

# only keep what we need
rm(list= ls()[!(ls() %in% c('df_mcaid_annual','df_hosp_chars', 'df_revs', 'df_hosp_revs_year'))])


df_hosp_chars$st_fips <- as.integer(df_hosp_chars$st_fips)
df_hosp_chars$mcrnum <- as.integer(df_hosp_chars$mcrnum)

# add cpi data
# https://fred.stlouisfed.org/series/CPIMEDSL
# 2012-01-01 -- 2018-12-31
# Index (scale value to 100 for chosen date)
# Frequency: annual
# Aggregation method: average 
df_cpi <- data.frame (year  = c(2012:2018), 
                      cpi   = c(100, 102.5, 104.9, 107.7, 111.8, 114.6, 116.8))

df_mcaid_annual <- left_join(df_mcaid_annual, df_cpi, by="year") %>% mutate(cpi = cpi/100)

# Combine all for "complete sample" analyses ------------------------------

# only keep observations with valid uncomp care data and valid cap ass data 
chars_mcaid <- left_join(df_hosp_chars, df_mcaid_annual, by = c("year" = "year", "st_fips" = "fips")) %>% 
  filter(year>=2012 & year<=2018) %>% ungroup() %>%
  mutate(uncomp_care = uncomp_care/cpi, 
         valid_uncomp = ifelse(uncomp_care >= quantile(uncomp_care, 0.05, na.rm=T) & uncomp_care <= quantile(uncomp_care, 0.95,na.rm=T,), 1, 0),
         valid_uncomp = ifelse(is.na(uncomp_care), 0, 1), 
         uncomp_care = uncomp_care/(1000),
         net_pat_rev = (net_pat_rev/admtot)/cpi,
         valid_pat_rev = ifelse(net_pat_rev >= quantile(net_pat_rev, 0.05, na.rm=T) & net_pat_rev <= quantile(net_pat_rev, 0.95,na.rm=T,), 1, 0),
         valid_pat_rev = ifelse(is.na(net_pat_rev), 0, 1)) %>% 
  filter(valid_uncomp==1 & valid_pat_rev==1)


# only keep observations that have data for each year in the panel 
chars_mcaid <- chars_mcaid %>%  
  subset(select=-c(sysid, mlocstcd, commty, mloczip, npinum, dtbeg, dtend, fisyr, divname)) %>% 
  group_by(mcrnum) %>%
  mutate(min_year = min(year), 
         max_year = max(year), 
         relative_year = ifelse(is.na(exp_year), 2014, exp_year), 
         keep_in_samp = ifelse((min_year==2012|min_year==2013) & max_year>=relative_year,1,0)) %>%
  filter(keep_in_samp==1) %>% 
  mutate(group_count = n()) %>% 
  filter(group_count ==7) %>% 
  subset(select=c(mcrnum, year, bdtot, admtot, ipdtot, mcddc, Government, Nonprofit, 
                  Teaching_Hospital1, Teaching_Hospital2, System, Labor_Phys, Labor_Nurse, 
                  uncomp_care, net_pat_rev, exp_year, mcaid_expansion, st_fips))

# create df to analyze average pre-treatement characteristics by treatment v control
df_pre_treat_chars <- chars_mcaid %>% group_by(mcrnum) %>% 
                      mutate(exp_year = ifelse(mcaid_expansion==1, exp_year, 2020)) %>%
                      filter(year < exp_year) %>% 
                      subset(select=-c(year, exp_year)) %>% 
                      summarise(across(bdtot:mcaid_expansion, mean, na.rm=T, .names = "ti_{.col}")) %>% 
                      mutate(ti_mcaid_expansion = ifelse(ti_mcaid_expansion==1, "Expansion States", "Non-Expansion States"))

df_export <- left_join(chars_mcaid, df_pre_treat_chars, by="mcrnum") %>% 
             subset(select=-c(ti_mcaid_expansion))


# output for panel data before adding in Yelp data 
write_rds(df_export, file = "data/output/combined-hospital-variables-panel-all.rds")



# Connect data to Yelp reviews --------------------------------------------

# create cross-sectional data 
yelp_cs <- left_join(df_revs,df_export, by = c("mcrnum" = "mcrnum", "year" = "year")) %>% 
            arrange(mcrnum, year) %>% 
            filter(!is.na(bdtot))

# output for panel data before adding in Yelp data 
write_rds(yelp_cs, file = "data/output/combined-hospital-variables-cs-yelp.rds")


# create panel data 
yelp_panel <- left_join(df_export, df_hosp_revs_year, by = c("mcrnum" = "mcrnum", "year" = "year")) %>% 
            replace_na(list(rv=0, count=0, ann_avg=0))

# output for panel data with Yelp data 
write_rds(yelp_panel, file = "data/output/combined-hospital-variables-panel-yelp.rds")



# Summarize Data  ---------------------------------------------------------
# Use the data that are loaded here to create table
source(here("Data-Code/2-Create_Summary_Stats_Table.R"))
