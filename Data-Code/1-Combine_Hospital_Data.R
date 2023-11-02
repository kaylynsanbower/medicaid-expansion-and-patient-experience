# Import data -------------------------------------------------------------

#-- AHA data
aha <- read.csv(file="data/input/aha_data.csv", header=T) %>%
  clean_names() %>% 
  subset(select=c(id, mcrnum, mname, year, sysid, bdtot, serv,
                  admtot, ipdtot, mcddc, 
                  commty, lat, long, hsacode, hrrcode,
                  mloczip, npinum, dtbeg, dtend, fisyr, fstcd, fcntycd,
                  own_gov, own_nfp, own_profit, teach_major, teach_minor, system, 
                  ftemd, fteres, ftern, ftelpn, fteh)) %>% 
  rename(st_fips = fstcd, 
         cty_fips = fcntycd, 
         labor_phys = ftemd, 
         labor_residents = fteres) %>% 
  mutate(labor_nurse = ftern+ftelpn, 
         labor_other = fteh - labor_phys - labor_residents - labor_nurse) %>% 
  filter(bdtot>=30, serv==10, fteh>0) %>%
  filter(year>=2010 & year <=2018) %>% 
  drop_na(mcrnum) %>% 
  mutate(mcrnum = stringr::str_pad(mcrnum, 6, pad = "0")) %>% 
  replace_na(list(system = 0)) %>%
  subset(select=-c(serv, ftern, ftelpn, fteh)) 

#-- CMI data
cmi <- read.csv(file="data/input/CMI-FINAL.csv", header=T) %>% subset(select = -c(X))

#-- HCRIS data
hcris <- read.delim(file="data/input/HCRIS_Data.txt", sep="\t", header=T) %>% 
  rename(uncomp_care_v96 = "uncomp_care") %>% 
  mutate(payermix = 1-((mcare_discharges+mcaid_discharges)/tot_discharges),
         mcrnum = stringr::str_pad(provider_number, 6, pad = "0"), 
         tot_uncomp_care_partial_pmts = ifelse(is.na(tot_uncomp_care_partial_pmts), 0, tot_uncomp_care_partial_pmts),
         bad_debt = ifelse(is.na(bad_debt), 0, bad_debt),
         uncomp_care_v10 = tot_uncomp_care_charges - tot_uncomp_care_partial_pmts + bad_debt,
         uncomp_care = ifelse(is.na(uncomp_care_v96), uncomp_care_v10, uncomp_care_v96),
         uncomp_care = uncomp_care*cost_to_charge) %>% 
  filter(year>=2004 & year<=2019) %>%
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

