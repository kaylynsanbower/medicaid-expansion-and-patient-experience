# Import Yelp data -- individual reviews 
df_revs <- read.csv(file="data/input/Yelp-Reviews-for-choice.csv", header=T) %>% clean_names() %>%
  subset(select = -c(x,id, rv_last, count_last, rev_minus_rv, observed, cut, rounded))  %>% 
  rename( c(rv="rv_now", count = "count_now"))

# create df of just year-qtr
year_qtr <- full_join(data.frame(year = c(2010:2018)), data.frame(qtr = c(1:4)), by=character()) %>%
  mutate(year_qtr = str_c(year, qtr, sep="_"))


# create df of just hospitals and add it to year-qtr dataframe
hosps <- df_revs %>% subset(select=c(mcrnum)) %>% unique()
year_qtr_hosp <- full_join(year_qtr, hosps, by=character())

year_hosp <- year_qtr_hosp %>% subset(select=-c(year_qtr, qtr)) %>% unique()


# prep year-qtr (beginning of quarter) values for reviews
df_revs_qtr <- df_revs %>% ungroup() %>%
  mutate(qtr = as.integer(str_sub(as.yearqtr(date, format = "%Y-%m-%d"), -1)),
         year_qtr = str_c(year, qtr, sep="_"), #
         next_year = ifelse(as.integer(qtr)==4, year+1, year),
         next_qtr = ifelse(qtr==4, 1, qtr+1),
         next_year_qtr = str_c(next_year, next_qtr, sep="_")) %>%
  relocate(c(qtr, year_qtr, next_year_qtr), .after="year") %>%
  arrange(mcrnum,date) %>%
  group_by(mcrnum, year, qtr) %>%
  filter(row_number()==n()) %>%
  subset(select=-c(date, rating, first, qtr, year, next_year, next_qtr, year_qtr))  %>%
  rename( year_qtr = "next_year_qtr")


# prep annual values
df_revs_year <- df_revs %>% ungroup() %>%
  arrange(mcrnum,date) %>%
  group_by(mcrnum, year) %>%
  mutate(ann_avg = mean(rating)) %>%
  filter(row_number()==n()) %>%
  subset(select=-c(date, rating, first))


# create full panel
df_hosp_revs_qtr <- left_join(year_qtr_hosp, df_revs_qtr, by=c("year_qtr", "mcrnum"))
df_hosp_revs_year <- left_join(year_hosp, df_revs_year, by=c("year", "mcrnum"))


# fill in missing review data
df_hosp_revs_qtr <- df_hosp_revs_qtr %>%
  ungroup() %>% group_by(mcrnum) %>% arrange(mcrnum, year_qtr) %>%
  fill(c(rv, count)) %>%
  replace_na(list(rv=0, count=0)) %>%
  subset(select=-c(qtr))

df_hosp_revs_year <- df_hosp_revs_year %>%
  ungroup() %>% group_by(mcrnum) %>% arrange(mcrnum, year) %>%
  fill(c(rv, count)) %>%
  replace_na(list(rv=0, count=0, ann_avg=0))




