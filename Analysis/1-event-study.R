
# event study estimates -- main text --------------------------------------

# clear
rm(list=ls())

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, lubridate, zoo, DescTools, psych, here, janitor,
               fixest, Hmisc, tikzDevice) 

# path to overleaf folder for output of code
results.path <- "/Users/kaylynsanbower/Dropbox/Apps/Overleaf/hospital-amenities-manuscript/Expansion_and_Experience/Tables_and_Figures/"


# Event study with hospital wide outcomes  --------------------------------

## Import data 
df <- read_rds("data/output/combined-hospital-variables-panel-all.rds") %>% clean_names() %>% arrange(mcrnum, year) %>%
      mutate(time_to_treat = ifelse(mcaid_expansion==1, year - exp_year, 0),
             exp_year = ifelse(is.na(exp_year), Inf, exp_year)) %>% filter(time_to_treat>-4)

## Models

#--Medicaid discharges
# twfe 
twfe_mcddc <- feols(mcddc ~ i(time_to_treat, mcaid_expansion, ref = -1) |     
                            year + mcrnum ,   
                         cluster = ~ mcrnum,
                         data = df)

summary(twfe_mcddc)

# twfe with covariates 
twfe_mcddc_covs <- feols(mcddc ~ i(time_to_treat, mcaid_expansion, ref =-1) +
                    bdtot + admtot + government + nonprofit + 
                    teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                    labor_nurse |     
                   year + mcrnum ,   
                 cluster = ~ mcrnum,
                 data = df)

summary(twfe_mcddc_covs)

# sa 
sa_mcddc <- feols(mcddc ~ sunab(exp_year, year) |     
                      year + mcrnum ,   
                    cluster = ~ mcrnum,
                    data = df)

summary(sa_mcddc)

# twfe with covariates 
sa_mcddc_covs <- feols(mcddc ~ sunab(exp_year, year) +
                           bdtot + admtot + government + nonprofit + 
                           teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                           labor_nurse |     
                           year + mcrnum ,   
                         cluster = ~ mcrnum,
                         data = df)

summary(sa_mcddc_covs)


tikz(file = paste(results.path, "sa_mcddc_results.tex", sep=""), width = 7, height = 5)
iplot(list(sa_mcddc, sa_mcddc_covs), sep = 0.15, ref.line = -1,
      xlab = 'Time to treatment',
      ylab = 'Estimates',
      main = "", 
      col=c(1,4)) 
legend("top",  col = c(1, 4), pch = c(20, 17), horiz = T, xpd = T, inset = c(0, -0.1),
       legend = c( "SA", "SA +"), cex = 0.75)
dev.off()


#--Uncompensated Care
# twfe 
twfe <- feols(uncomp_care ~ i(time_to_treat, mcaid_expansion, ref = -1) |     
                      year + mcrnum ,   
                    cluster = ~ mcrnum,
                    data = df)

summary(twfe)

# twfe with covariates 
twfe_covs <- feols(uncomp_care ~ i(time_to_treat, mcaid_expansion, ref =-1) +
                           bdtot + admtot + government + nonprofit + 
                           teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                           labor_nurse |     
                           year + mcrnum ,   
                         cluster = ~ mcrnum,
                         data = df)

summary(twfe_covs)

# sa 
sa <- feols(uncomp_care ~ sunab(exp_year, year) |     
                    year + mcrnum ,   
                  cluster = ~ mcrnum,
                  data = df)

summary(sa)

# twfe with covariates 
sa_covs <- feols(uncomp_care ~ sunab(exp_year, year) +
                         bdtot + admtot + government + nonprofit + 
                         teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                         labor_nurse |     
                         year + mcrnum ,   
                       cluster = ~ mcrnum,
                       data = df)

summary(sa_covs)


tikz(file = paste(results.path, "sa_uncomp_care_results.tex", sep=""), width = 7, height = 5)
iplot(list(sa, sa_covs), sep = 0.15, ref.line = -1,
      xlab = 'Time to treatment',
      ylab = 'Estimates',
      main = "", 
      col=c(1,4)) 
legend("top",  col = c(1, 4), pch = c(20, 17), horiz = T, xpd = T, inset = c(0, -0.1),
       legend = c( "SA", "SA +"), cex = 0.75)
dev.off()



#--Net Patient Revenue
# twfe 
twfe <- feols(net_pat_rev ~ i(time_to_treat, mcaid_expansion, ref = -1) |     
                year + mcrnum ,   
              cluster = ~ mcrnum,
              data = df)

summary(twfe)

# twfe with covariates 
twfe_covs <- feols(net_pat_rev ~ i(time_to_treat, mcaid_expansion, ref =-1) +
                     bdtot  + government + nonprofit + 
                     teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                     labor_nurse |     
                     year + mcrnum ,   
                   cluster = ~ mcrnum,
                   data = df)

summary(twfe_covs)

# sa 
sa <- feols(net_pat_rev ~ sunab(exp_year, year) |     
              year + mcrnum ,   
            cluster = ~ mcrnum,
            data = df)

summary(sa)

# twfe with covariates 
sa_covs <- feols(net_pat_rev ~ sunab(exp_year, year) +
                   bdtot  + government + nonprofit + 
                   teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                   labor_nurse |     
                   year + mcrnum ,   
                 cluster = ~ mcrnum,
                 data = df)

summary(sa_covs)


tikz(file = paste(results.path, "sa_net_pat_rev_results.tex", sep=""), width = 7, height = 5)
iplot(list(sa, sa_covs), sep = 0.15, ref.line = -1,
      xlab = 'Time to treatment',
      ylab = 'Estimates',
      main = "", 
      col=c(1,4)) 
legend("top",  col = c(1, 4), pch = c(20, 17), horiz = T, xpd = T, inset = c(0, -0.1),
       legend = c( "SA", "SA +"), cex = 0.75)
dev.off()



# Import data  ------------------------------------------------------------
df <- read_rds("data/combined-hospital-variables-cs-yelp.rds") %>% clean_names() %>% arrange(mcrnum, year) %>% 
      mutate(time_to_treat = ifelse(mcaid_expansion==1, year - exp_year, 0), 
             exp_year = ifelse(is.na(exp_year), 0, exp_year)) %>% 
      filter(time_to_treat > -4) %>% group_by(mcrnum) %>% mutate(first_rev = min(year)) %>% 
  filter(first_rev ==2012)

# # create list of hospitals to include in sample to compare topic models and export
# df_sample <- df %>% subset(select=c(mcrnum, year, mcaid_expansion, time_to_treat, exp_year)) %>% unique() %>% 
#   write_csv(file = "~/Dropbox/research-projects/hospital-amenities-nlp/Data/Yelp/Input/sa_hosp_samp.csv")


#--Net Patient Revenue
# twfe 
twfe <- feols(rating ~ i(time_to_treat, mcaid_expansion, ref = -1) |     
                year + mcrnum ,   
              cluster = ~ mcrnum,
              data = df)

summary(twfe)

# twfe with covariates 
twfe_covs <- feols(rating ~ i(time_to_treat, mcaid_expansion, ref =-1) +
                     bdtot + admtot + government + nonprofit + 
                     teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                     labor_nurse |     
                     year + mcrnum ,   
                   cluster = ~ mcrnum,
                   data = df)

summary(twfe_covs)

# sa 
sa <- feols(rating ~ sunab(exp_year, year) |     
              year + mcrnum ,   
            cluster = ~ mcrnum,
            data = df)

summary(sa)

# twfe with covariates 
sa_covs <- feols(rating ~ sunab(exp_year, year) +
                   bdtot + admtot + government + nonprofit + 
                   teaching_hospital1 + teaching_hospital2 + system + labor_phys + 
                   labor_nurse |     
                   year + mcrnum ,   
                 cluster = ~ mcrnum,
                 data = df)

summary(sa_covs)


tikz(file = paste(results.path, "sa_ratings_results.tex", sep=""), width = 7, height = 5)
iplot(list(sa, sa_covs), sep = 0.15, ref.line = -1,
      xlab = 'Time to treatment',
      ylab = 'Estimates',
      main = "", 
      col=c(1,4)) 
legend("top",  col = c(1, 4), pch = c(20, 17), horiz = T, xpd = T, inset = c(0, -0.1),
       legend = c( "SA", "SA +"), cex = 0.75)
dev.off()



