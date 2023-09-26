# Import data -------------------------------------------------------------

#-- AHA data
aha17 <- read.csv(file="data/input/AHA-2005_2017-CLEAN.csv", header=T)
aha18 <- read.csv(file="data/input/AHA-2018-CLEAN.csv", header=T)
aha <- bind_rows(aha17, aha18) %>% drop_na(mcrnum) %>%
  filter(serv==10, bdtot>=30) %>% 
  mutate(mcrnum = stringr::str_pad(mcrnum, 6, pad = "0")) %>% 
  subset(select=-c(mlocaddr,mloccity,serv)) %>% 
  separate(fips, c("st_fips", "cty_fips"))

rm(aha17, aha18)

#-- CMI data
cmi <- read.csv(file="data/input/CMI-FINAL.csv", header=T) %>% subset(select = -c(X))

#-- HCRIS data
hcris <- read.delim(file="data/input/extra_vars_HCRIS_Data_thru2019.txt", sep="\t", header=T) %>% 
                  rename(uncomp_care_v96 = "uncomp_care") %>% 
                  mutate(payermix = 1-((mcare_discharges+mcaid_discharges)/tot_discharges),
                         mcrnum = stringr::str_pad(provider_number, 6, pad = "0"), 
                         tot_uncomp_care_partial_pmts = ifelse(is.na(tot_uncomp_care_partial_pmts), 0, tot_uncomp_care_partial_pmts),
                         bad_debt = ifelse(is.na(bad_debt), 0, bad_debt),
                         uncomp_care_v10 = tot_uncomp_care_charges - tot_uncomp_care_partial_pmts + bad_debt,
                         uncomp_care = ifelse(is.na(uncomp_care_v96), uncomp_care_v10, uncomp_care_v96),
                         uncomp_care = uncomp_care*cost_to_charge) %>% 
                  subset(select=c(mcrnum, year, mcare_discharges, mcaid_discharges, payermix, 
                                  uncomp_care, new_cap_ass, cash, tot_pat_rev, allowance, net_pat_rev))

#-- AHRF data
ahrf <- read.csv(file="data/input/ahrf-panel-2011-2018.csv", header=T) %>% subset(select = -c(X)) %>%
        mutate(st_fips = as.character(st_fips),
               cty_fips = as.character(cty_fips))

#-- Hospital Compare clinical quality data 
hc <- read.csv(file="data/input/HC_MORT_READM.csv",header=T) %>%
  clean_names() %>% 
  mutate(mcrnum = provider_id) %>% 
  subset(select=-c(x, provider_id)) %>% 
  relocate(mcrnum)


# Combine all 
df_hosp_chars <- left_join(aha, hcris,  by=c("mcrnum", "year"))
df_hosp_chars <- left_join(df_hosp_chars, cmi,  by=c("mcrnum", "year"))
df_hosp_chars <- left_join(df_hosp_chars, ahrf,  by=c("year", "st_fips", "cty_fips"))
df_hosp_chars <- left_join(df_hosp_chars, hc,  by=c("mcrnum", "year"))


