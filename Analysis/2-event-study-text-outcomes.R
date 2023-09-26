# clear
rm(list=ls())

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,
               fixest, Hmisc, tikzDevice, ggplot2, modelsummary) 

# path to overleaf folder for output of code
results.path <- "/Users/kaylynsanbower/Dropbox/Apps/Overleaf/Hospital Amenities (2023)/Tables_and_Figures/"


# Import data  ------------------------------------------------------------
df <- read.csv("/Users/kaylynsanbower/Dropbox/Research-Projects/hospital-amenities-nlp/Data/Yelp/Output/2022_3_24-revs_and_polarity.csv", header = T) %>% 
  rename(rating = "rating_ind") %>% 
  rowwise() %>%
  mutate(ncq_words = sum(c_across("aesthetic":"wireless")),
         ncq_word_ind = ifelse(ncq_words>0, 1, 0))

# update main outcome of interest
df <- df %>% mutate(amen_proxy = ifelse(polarity>=0 & ncq_word_ind>=0 & rating !=1, 1, 0))

# adding back in the covariates
df_chars <- read_rds("data/output/combined-hospital-variables-cs-yelp.rds") %>% clean_names() %>% arrange(mcrnum, year) %>% 
  subset(select=c(mcrnum, year, bdtot, admtot, government, nonprofit, teaching_hospital1, teaching_hospital2,
                  system, labor_phys, labor_nurse)) %>% unique()

# combine DFs
df <- left_join(df, df_chars, by=c("mcrnum" = "mcrnum","year" = "year"))




# Estimation --------------------------------------------------------------

# sa 
sa <- feols(amen_proxy ~ sunab(exp_year, year) |     
              year + mcrnum ,   
            cluster = ~ mcrnum,
            data = df)

summary(sa)

# sa + covs
sa_covs <- feols(amen_proxy ~ sunab(exp_year, year) +
                   bdtot + admtot + government + nonprofit + 
                   teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                   labor_nurse |     
                   year + mcrnum ,   
                 cluster = ~ mcrnum,
                 data = df)

summary(sa_covs)


tikz(file = paste(results.path, "sa_amen_proxy_results.tex", sep=""), width = 7, height = 5)
iplot(list(sa, sa_covs), sep = 0.15, ref.line = -1,
      xlab = 'Time to treatment',
      ylab = 'Estimates',
      main = "", 
      col=c(1,4)) 
legend("top",  col = c(1, 4), pch = c(20, 17), horiz = T, xpd = T, inset = c(0, -0.1),
       legend = c( "SA", "SA +"), cex = 0.75)
dev.off()
