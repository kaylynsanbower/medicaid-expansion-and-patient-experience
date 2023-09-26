
# Balance Plot ------------------------------------------------------------


# identify what hospitals remain in the sample
hosps_in_samp <- all_data %>% subset(select= c('mcrnum')) %>% unique()

hosps_all <- chars_mcaid %>% subset(select=c(mcrnum)) %>% unique()

hosps_out <- hosps_all$mcrnum[is.na(pmatch(hosps_all$mcrnum,hosps_in_samp$mcrnum))]
hosps_out <- as.data.frame(hosps_out)
hosps_out <- hosps_out %>% rename(mcrnum = "hosps_out")

# Only keep Yelp reviews from hospitals in the "approved" hospitals list 
df_out <- chars_mcaid[FALSE,]
for (i in hosps_out$mcrnum){
  keep <-chars_mcaid[which(chars_mcaid$mcrnum== i),]
  df_out <- bind_rows(df_out, keep)
}
rm(keep)

# keep the variables that I want to use in the balance plot
df_in <- all_data %>% subset(select=c(mcrnum, year, bdtot, Government, Nonprofit, Teaching_Hospital1, Teaching_Hospital2, 
                                      System, Labor_Phys, Labor_Nurse, mcaid_discharges, payermix, cmi, pop, mcaid_expansion)) %>% unique() %>% 
  mutate(samp_status = 1)

df_out <- df_out %>% subset(select=c(mcrnum, year, bdtot, Government, Nonprofit, Teaching_Hospital1, Teaching_Hospital2, 
                                     System, Labor_Phys, Labor_Nurse, mcaid_discharges, payermix, cmi, pop, mcaid_expansion)) %>% unique()%>% 
  mutate(samp_status = 0) %>% filter(year>=2012 & year<=2018) 

df_bp <- bind_rows(df_in, df_out)

# rename variables 
df_bp <- df_bp %>% rename("MedicaidExpansion" = mcaid_expansion,
                          "LaborPhys" = Labor_Phys, 
                          "LaborNurse" = Labor_Nurse, 
                          "MedicaidDischarges" = mcaid_discharges, 
                          "MajorTeaching" = Teaching_Hospital1, 
                          "AnyTeaching" = Teaching_Hospital2)


# Changing variables into per capita
df_bp <- df_bp %>% mutate(bdtot = bdtot/pop,
                          LaborPhys = LaborPhys/pop,
                          LaborNurse = LaborNurse/pop,
                          MedicaidDischarges = MedicaidDischarges/pop)

# Only keep complete cases 
df_bp <- df_bp[complete.cases(df_bp),]
covh <- subset(df_bp, select=-c(samp_status, mcrnum, year, pop))

h.out <- bal.tab(covh, treat= "samp_status", data=df_bp, m.threshold=.1, s.d.denom="pooled")


setwd("/Users/kaylynsanbower/Dropbox/Apps/Overleaf/hospital-amenities-manuscript/Expansion_and_Experience/tables_and_figures")
tikz(file = "in_out_sample_balance.tex", width = 6, height = 4)
love.plot(h.out,stats = "mean.diffs", title=NULL,
          abs=FALSE, position="NA", stars="raw",
          #limits = c(-0.2, 0.2), 
          shapes = c("circle"),
          colors = c("black")) 
dev.off()


# covariate balance of in-sample hospitals -- expansion v non-expansion state 
df_bp_in <- df_bp %>% filter(samp_status==1)
cov_in <- subset(df_bp_in, select=-c(samp_status, mcrnum, year, pop, MedicaidExpansion))

h.out_in <- bal.tab(cov_in, treat= "MedicaidExpansion", data=df_bp_in, m.threshold=.1, s.d.denom="pooled")


setwd("/Users/kaylynsanbower/Dropbox/Apps/Overleaf/hospital-amenities-manuscript/Expansion_and_Experience/tables_and_figures")
tikz(file = "in_out_sample_balance.tex", width = 6, height = 4)
love.plot(h.out_in,stats = "mean.diffs", title=NULL,
          abs=FALSE, position="NA", stars="raw",
          #limits = c(-0.2, 0.2), 
          shapes = c("circle"),
          colors = c("black")) 
dev.off()







