#-- Add medicaid expansion data
mcaid <- read.csv(file="data/input/medicaid_expansion.csv", header=T) %>%
  clean_names() %>% 
  mutate(expansion_date = as_date(expansion_date, format = "%m/%d/%y"), 
         coverage_date = as_date(coverage_date, format = "%m/%d/%y"),
         exp_year = ifelse(year(expansion_date)<=2018, year(expansion_date), NA), 
         mcaid_expansion = ifelse(is.na(exp_year), 0, 1), 
         forqtr = as.yearqtr(expansion_date, format = "%Y-%m-%d"), 
         exp_qtr = str_sub((forqtr), -1), 
         exp_year_qtr = format(forqtr, format = "%Y_%q")) %>% 
  subset(select=c(abbreviation, fips, mcaid_expansion, expansion_date, exp_year_qtr, exp_year)) 

#-- create year-quarter df 
fips <- mcaid %>% subset(select=c(fips))
year_qtr <- full_join(data.frame(year = c(2010:2018)), data.frame(qtr = c(1:4)), by=character()) %>% 
            mutate(year_qtr = str_c(year, qtr, sep="_"))
year_qtr_fips <- full_join(year_qtr, fips, by=character()) 


# Combine both 
df_mcaid <- full_join(year_qtr_fips, mcaid, by="fips") %>% 
         mutate(post = ifelse(year_qtr>=exp_year_qtr, 1, 0),
                expxpost = ifelse(is.na(post), 0, mcaid_expansion*post))

df_mcaid_annual <- df_mcaid %>% filter(qtr==4) %>% subset(select=-c(qtr, year_qtr))

