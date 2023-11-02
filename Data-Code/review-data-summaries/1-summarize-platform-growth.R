# clear
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,cobalt) 



###########################################################################
# GOOGLE DATA  ------------------------------------------------------------

# --- create df of number of hospital on the platform per year
# import google reviews 
df <- read.csv(file="data/input/google_reviews_mcrnum.csv", header=T, skipNul = T, na.strings=c(""," ","NA")) %>% 
  subset(select=c(year, MCRNUM)) %>% 
  unique() %>% 
  drop_na(MCRNUM) %>% 
  group_by(MCRNUM) %>% 
  mutate(min_year = min(year))

yrs <- data.frame(year = c(min(df$year):max(df$year)))

hosp <- df %>% 
  subset(select=c(MCRNUM)) %>%
  unique() 

# year x hospital df 
yxh <- full_join(yrs, hosp, by=character())

# year x hospital plus reviews df 
yxhpr <- left_join(yxh, df, by=c("year", "MCRNUM")) %>% 
  group_by(MCRNUM) %>% 
  fill(min_year, .direction = "downup") %>% 
  mutate(rated = ifelse(year>=min_year, 1, 0)) %>% 
  subset(select=c(year, rated)) %>% 
  group_by(year) %>% 
  summarise(rated = sum(rated))


google_profiles <- yxhpr



# --- create a df of number of reviews on the platform over time. 
# import google reviews again
df <- read.csv(file="data/input/google_reviews_mcrnum.csv", header=T, skipNul = T, na.strings=c(""," ","NA")) %>% 
  subset(select=c(year, MCRNUM)) %>% 
  drop_na(MCRNUM) %>% 
  group_by(MCRNUM, year) %>% 
  mutate(ann_revs = n()) %>% 
  unique() %>%
  arrange(MCRNUM, year) %>% 
  group_by(MCRNUM) %>%
  mutate(cumu_revs = cumsum(ann_revs)) %>% 
  subset(select=-c(MCRNUM)) %>% 
  group_by(year) %>%
  summarise(ann_revs=sum(ann_revs), cumu_revs=sum(cumu_revs))

google_reviews <- df






###########################################################################
# YELP DATA ---------------------------------------------------------------

# reviews 
df <- read.csv(file="data/input/Yelp-Reviews-for-choice.csv", header=T)


# Prep variables  ---------------------------------------------------------

df <- df %>% 
  mutate(year = year(date)) %>% 
  subset(select=c(year, MCRNUM)) %>% 
  unique() %>% 
  drop_na(MCRNUM) %>% 
  group_by(MCRNUM) %>% 
  mutate(min_year = min(year))

yrs <- data.frame(year = c(min(df$year):max(df$year)))

hosp <- df %>% 
  subset(select=c(MCRNUM)) %>%
  unique() 

# year x hospital df 
yxh <- full_join(yrs, hosp, by=character())

# year x hospital plus reviews df 
yxhpr <- left_join(yxh, df, by=c("year", "MCRNUM")) %>% 
  group_by(MCRNUM) %>% 
  fill(min_year, .direction = "downup") %>% 
  mutate(rated = ifelse(year>=min_year, 1, 0)) %>% 
  subset(select=c(year, rated)) %>% 
  group_by(year) %>% 
  summarise(rated = sum(rated))


yelp_profiles <- yxhpr

  
  
# --- create a df of number of reviews on the platform over time. 

# import reviews again
df <-  read.csv(file="data/input/Yelp-Reviews-for-choice.csv", header=T) %>% 
  mutate(year = year(date)) %>% 
  subset(select=c(year, MCRNUM)) %>% 
  drop_na(MCRNUM) %>% 
  group_by(MCRNUM, year) %>% 
  mutate(ann_revs = n()) %>% 
  unique() %>%
  arrange(MCRNUM, year) %>% 
  group_by(MCRNUM) %>%
  mutate(cumu_revs = cumsum(ann_revs)) %>% 
  subset(select=-c(MCRNUM)) %>% 
  group_by(year) %>%
  summarise(ann_revs=sum(ann_revs), cumu_revs=sum(cumu_revs))

yelp_reviews <- df

  
  

###########################################################################
# PLOT BOTH ---------------------------------------------------------------

google_profiles <- google_profiles %>%
  mutate(platform = "google") %>% 
  filter(year>=2008 & year<=2018)
  
yelp_profiles <- yelp_profiles %>%
  mutate(platform = "yelp") %>% 
  filter(year>=2008 & year<=2018)


profiles <- bind_rows(google_profiles, yelp_profiles)

profiles %>%
  ggplot( aes(x=year, y=rated, group=platform, color=platform)) +
  geom_line() +
  theme_minimal() + 
  ggtitle("Hospital Google, Yelp Profiles over Time") +
  ylab("Number of Profiles")





google_reviews <- google_reviews %>%
  mutate(platform = "google") %>% 
  filter(year>=2008 & year<=2018)

yelp_reviews <- yelp_reviews %>%
  mutate(platform = "yelp") %>% 
  filter(year>=2008 & year<=2018)


reviews <- bind_rows(google_reviews, yelp_reviews)

reviews %>%
  ggplot(aes(x=year, y=ann_revs, group=platform, color=platform)) +
  geom_line() +
  theme_minimal() + 
  ggtitle("New Reviews per Year") +
  ylab("Number of Reviews")

reviews %>%
  ggplot(aes(x=year, y=cumu_revs, group=platform, color=platform)) +
  geom_line() +
  theme_minimal() + 
  ggtitle("Cumulative Hospital Reviews per Year") +
  ylab("Number of Reviews")














  






  