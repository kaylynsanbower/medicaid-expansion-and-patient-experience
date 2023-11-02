# clear
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,cobalt) 



###########################################################################
# google data -------------------------------------------------------------


# --- create df of number of hospital on the platform per year
# import google reviews 
df <- read.csv(file="data/input/google_reviews_mcrnum.csv", header=T, skipNul = T, na.strings=c(""," ","NA")) %>% 
  subset(select=c(year, MCRNUM)) %>% 
  unique() %>% 
  drop_na(MCRNUM) %>% 
  group_by(MCRNUM) %>% 
  mutate(min_year_google = min(year))

yrs <- data.frame(year = c(min(df$year):max(df$year)))

hosp <- df %>% 
  subset(select=c(MCRNUM)) %>%
  unique() 

# year x hospital df 
yxh <- full_join(yrs, hosp, by=character())

# year x hospital plus reviews df 
yxhpr <- left_join(yxh, df, by=c("year", "MCRNUM")) %>% 
  group_by(MCRNUM) %>% 
  fill(min_year_google, .direction = "downup") %>% 
  mutate(rated_google = ifelse(year>=min_year_google, 1, 0))


google_profiles <- yxhpr



###########################################################################
# YELP DATA ---------------------------------------------------------------

# reviews 
df <- read.csv(file="data/input/Yelp-Reviews-for-choice.csv", header=T) %>% 
  mutate(year = year(date)) %>% 
  subset(select=c(year, MCRNUM)) %>% 
  unique() %>% 
  drop_na(MCRNUM) %>% 
  group_by(MCRNUM) %>% 
  mutate(min_year_yelp = min(year))

yrs <- data.frame(year = c(min(df$year):max(df$year)))

hosp <- df %>% 
  subset(select=c(MCRNUM)) %>%
  unique() 

# year x hospital df 
yxh <- full_join(yrs, hosp, by=character())

# year x hospital plus reviews df 
yxhpr <- left_join(yxh, df, by=c("year", "MCRNUM")) %>% 
  group_by(MCRNUM) %>% 
  fill(min_year_yelp, .direction = "downup") %>% 
  mutate(rated_yelp = ifelse(year>=min_year_yelp, 1, 0)) 

yelp_profiles <- yxhpr



# combine -----------------------------------------------------------------


yrs <- data.frame(year = c(2008:2018))

hosp <- data.frame(MCRNUM = c(google_profiles$MCRNUM, yelp_profiles$MCRNUM)) %>% 
  unique()

# year x hospital df 
yxh <- full_join(yrs, hosp, by=character())

# year x hospital plus reviews df 
yxhpr <- left_join(yxh, yelp_profiles, by=c("year", "MCRNUM")) %>% 
  left_join(google_profiles, by=c("year", "MCRNUM")) %>% 
  replace_na(list(rated_yelp=0, rated_google=0))%>%
  mutate(presence = 0, 
         presence = ifelse(rated_google==1 & rated_yelp==1, "Both", presence), 
         presence = ifelse(rated_google==1 & rated_yelp==0, "Only Google", presence), 
         presence = ifelse(rated_google==0 & rated_yelp==1, "Only Yelp", presence)) %>% 
  filter(presence != 0) %>% 
  subset(select=c(year, presence)) %>% 
  group_by(year, presence) %>% 
  mutate(obs = n()) %>% 
  unique()

ggplot(yxhpr, aes(fill=presence, y=obs, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_minimal()











