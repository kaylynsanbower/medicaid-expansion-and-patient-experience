
results.path <- "/Users/kaylynsanbower/Dropbox/Apps/Overleaf/Hospital Amenities (2023)/Tables_and_Figures/"

# create df to analyze average pre-treatement characteristics by treatment v control
df_pre_treat_chars <- chars_mcaid %>% group_by(mcrnum) %>% 
  mutate(exp_year = ifelse(mcaid_expansion==1, exp_year, 2020)) %>%
  filter(year < exp_year) %>% 
  subset(select=-c(year, exp_year)) %>% 
  summarise(across(bdtot:mcaid_expansion, mean, na.rm=T, .names = "{.col}")) %>% 
  mutate(mcaid_expansion = ifelse(mcaid_expansion==1, "Expansion States", "Non-Expansion States"))


devtools::source_gist("c4d1089a501d3567be9fb784b1c5a6ab")

datasets = list("Hosptial Characteristics" = df_pre_treat_chars)
variables = list(c("bdtot", "admtot", "ipdtot", "Government", "Nonprofit", "Teaching_Hospital1", "Teaching_Hospital2", "System",  "Labor_Phys", "Labor_Nurse",
                   "mcddc", "uncomp_care", "net_pat_rev"))
labels = list(c("Total Beds", "Total Admissions", "Total Inpatient Discharges", "Government", "Non-profit", "Major Teaching Hospital", "Any Teaching Hospital", 
                "System Member", "Physicians", "Nurses", "Medicaid Discharges", "Uncompensated Care (1,000s)", "Net Patient Revenue (1,000s)"))
colnames =  c("Mean", "St. Dev.")

# descriptive function
myDescriptives = function(x) {
  x = as.numeric(x)
  m = mean(x, na.rm = TRUE)
  sd = sd(x, na.rm = TRUE)
  return(c(m, sd))
}

# create table
createDescriptiveTable(datasets,
                       summary_function = myDescriptives,
                       column_names = colnames,
                       variable_names = variables,
                       variable_labels = labels,
                       group_variable = "mcaid_expansion",
                       tabcolsep = 20,
                       arraystretch = 1.3,
                       title = "Summary Statistics by State Expansion Status",
                       note = "Notes: This table summarizes average hospital characteristics by state expansion status.  
                       For hospitals in expansion states, i.e., treated hospitals, I present average pre-treatment characteristics. 
                       For hospitals in non-expansion states, i.e., control hospitals, I present average characteristics over the entire sample period (2012 - 2018).",
                       label = "table:summary-stats", 
                       file = paste(results.path, "hosp-sum-stats.tex", sep=""))

