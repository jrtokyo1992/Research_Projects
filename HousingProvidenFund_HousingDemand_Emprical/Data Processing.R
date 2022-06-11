library(dplyr)
library(readstata13)
library(ggplot2)
library(tidyverse)
library(stringr)
#df = reggplot2#df = read.csv('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2015/Data/chfs2015_hh_20201030.txt')



# construct the housing parnel

df_2011 = read.dta13('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2011/chfs2011_hh_20191120.dta')
df_house_2011 = df_2011 %>%
  select (c2001, c2002, c2003_1, c2003_2, c2003_3, c2006_1, c2006_2, c2006_3, c2012_1, c2012_2, c2012_3,hhid) %>%
  mutate (across(c('c2001', 'c2002', 'c2003_1', 'c2003_2', 'c2003_3', 'c2006_1', 'c2006_2', 'c2006_3'), ~as.character(.)))%>%
  mutate (year = 2011) %>%
  mutate (across(c('c2006_1','c2006_2', 'c2006_3'), ~ ifelse (.==1, 'market',
                                                              ifelse(.==3, 'legacy',
                                                                     ifelse(. == 4, 'low-price',
                                                                            ifelse(.== 2, 'economic',
                                                                                   ifelse(. == 5, 'fundraising',
                                                                                          ifelse(.== 6, 'diy',
                                                                                                 ifelse (.== 7,'torndown', NA)))))))))


df_house_2013 = df_2013 %>% select (hhid_2011, hhid_2013, 
                                c2001, 
                                c2002,
                                c2003_1, c2003_2, c2003_3,
                                c2006_1,c2006_2,c2006_3, 
                                c2012_1, c2012_2, c2012_3,
) %>%
  mutate (across(c('hhid_2011', 'hhid_2013'), ~ifelse(.=='',NA, .))) %>%
  rowwise (.) %>%
  mutate (hhid = first(na.omit(c_across(hhid_2011:hhid_2013)))) %>%
  ungroup(.) %>% select (-hhid_2011, -hhid_2013) %>%
  mutate (across(c('c2001', 'c2002', 'c2003_1', 'c2003_2', 'c2003_3', 'c2006_1', 'c2006_2', 'c2006_3'), ~as.character(.))) %>%
  mutate (year = 2013) %>%
  mutate (across(c('c2006_1','c2006_2', 'c2006_3'), ~ ifelse (.%in% c(1,8), 'market',
                                                              ifelse(.==3, 'legacy',
                                                                     ifelse(. == 4, 'low-price',
                                                                            ifelse(.== 2, 'economic',
                                                                                   ifelse(. == 5, 'fundraising',
                                                                                          ifelse(.== 6, 'diy',
                                                                                                 ifelse (.== 7,'torndown', NA)))))))))


df_house_2015 = df_2015 %>% select (hhid_2011, hhid_2013, hhid_2015,
                                    c2001, 
                                    c2002,
                                    c2003_1, c2003_2, c2003_3,
                                    c2006_1,c2006_2,c2006_3, 
                                    c2012_1, c2012_2, c2012_3,
) %>%
  mutate (across(c('hhid_2011', 'hhid_2013', 'hhid_2015'), ~ifelse(.=='',NA, .))) %>%
  rowwise (.) %>%
  mutate (hhid = first(na.omit(c_across(hhid_2011:hhid_2015)))) %>%
  ungroup(.) %>% select (-hhid_2011, -hhid_2013, -hhid_2015)  %>%
  mutate (across(c('c2001', 'c2002', 'c2003_1', 'c2003_2', 'c2003_3', 'c2006_1', 'c2006_2', 'c2006_3'), ~as.character(.))) %>%
  mutate (year = 2015) %>%
  mutate (across(c('c2006_1','c2006_2', 'c2006_3'), ~ ifelse (.%in% c(1,2,9), 'market',
                                                              ifelse(.==4, 'legacy',
                                                                     ifelse(. == 5, 'low-price',
                                                                            ifelse(.== 3, 'economic',
                                                                                   ifelse(. == 6, 'fundraising',
                                                                                          ifelse(.== 7, 'diy',
                                                                                                 ifelse (.== 8,'torndown', NA)))))))))



df_house_2017 = df_2017 %>% select (
  hhid_2011, hhid_2013, hhid_2015, hhid_2017,
  c2001, 
  c2002,
  c2003_1, c2003_2, c2003_3,
  c2006_1,c2006_2,c2006_3, 
  c2012_1, c2012_2, c2012_3,
) %>%
  mutate (across(c('hhid_2011', 'hhid_2013', 'hhid_2015', 'hhid_2017'), ~ifelse(.=='',NA, .))) %>%
  rowwise (.) %>%
  mutate (hhid = first(na.omit(c_across(hhid_2011:hhid_2017)))) %>%
  ungroup(.) %>% select (-hhid_2011, -hhid_2013, -hhid_2015, -hhid_2017) %>%
  mutate (across(c('c2001', 'c2002', 'c2003_1', 'c2003_2', 'c2003_3', 'c2006_1', 'c2006_2', 'c2006_3'), ~as.character(.))) %>%
  mutate (year = 2017) %>%
  mutate (across(c('c2006_1','c2006_2', 'c2006_3'), ~ ifelse (.%in% c(1,2,9), 'market',
                                                              ifelse(.==4, 'legacy',
                                                                     ifelse(. == 5, 'low-price',
                                                                            ifelse(.== 3, 'economic',
                                                                                   ifelse(. == 6, 'fundraising',
                                                                                          ifelse(.== 7, 'diy',
                                                                                                 ifelse (.== 8,'torndown', NA)))))))))



df_house_2019 = df_2019 %>% select (
  c2001, 
  c2002,
  c2003_1, c2003_2, c2003_3,
  c2006_1,c2006_2,c2006_3, 
  c2012a_1, c2012a_2, c2012a_3,hhid # seems that c2012_1 , c2012_2, c2012_3 are filled with null
) %>%
  mutate (across(c('c2001', 'c2002', 'c2003_1', 'c2003_2', 'c2003_3', 'c2006_1', 'c2006_2', 'c2006_3'), ~as.character(.))) %>%
  mutate (year = 2019) %>%
  mutate (across(c('c2006_1','c2006_2', 'c2006_3'), ~ ifelse (.%in% c(1,2,9), 'market',
                                                              ifelse(.==4, 'legacy',
                                                                     ifelse(. == 5, 'low-price',
                                                                            ifelse(.== 3, 'economic',
                                                                                   ifelse(. == 6, 'fundraising',
                                                                                          ifelse(.== 7, 'diy',
                                                                                                 ifelse (.== 8,'torndown', NA)))))))))



df_house = bind_rows (df_house_2011, df_house_2013)%>%
  bind_rows(., df_house_2015)%>%
  bind_rows(., df_house_2017)%>%
  bind_rows(., df_house_2019) %>%
  arrange (hhid, year) %>%
  rename ( ownership = c2001, amt = c2002, new_src_1 = c2006_1, new_src_2 = c2006_2,new_src_3 = c2006_3,
             new_yr_1 = c2012_1, new_yr_2 = c2012_2, new_yr_3 = c2012_3,
             new_area_1 = c2003_1, new_area_2 = c2003_2, new_area_3 = c2003_3) %>%
 mutate (new_house1 = paste0(new_src_1,'_', new_yr_1,'_', new_area_1),
         new_house2 = paste0(new_src_2,'_', new_yr_2,'_', new_area_2),
         new_house3 = paste0(new_src_3,'_', new_yr_3,'_', new_area_3),
         ) %>% select (hhid,ownership, amt, year, new_house1, new_house2, new_house3)

## get the housig purchase history for each household
df_house_history = df_house %>% select (hhid, new_house1, new_house2, new_house3) %>%
  pivot_longer (-c(hhid),names_to = 'a', values_to = 'house' ) %>%
  group_by(hhid, house) %>% summarize (count = n()) %>%
  ungroup(.) %>% #filter (house != 'NA_NA_NA') %>%
  mutate (house_split = str_split(house, '_')) %>%
  unnest(house_split) %>%
  group_by(hhid, house) %>% mutate (key = row_number ()) %>% ungroup(.) %>%
  mutate (key = ifelse (key == 1, 'src', ifelse(key ==2, 'yr', 'area'))) %>%
  select (-count) %>%
  pivot_wider (names_from = key, values_from = house_split)



################################################################################
# create the individual table


df_ind_2011 = read.dta13('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2011/chfs2011_ind_20191120.dta') %>%
  inner_join (df_master_2011, by = 'hhid') %>%
  mutate (new_ind_gender = NA, new_ind_birthyear = NA, pab = NA) %>%
  mutate (flg = 1*(a3000==1 & a3003!=3)) %>%
  mutate (hpf = ifelse(flg ==0 | is.na(flg), 2, f4001)) %>%
  mutate (hpf_time = f4007, hpf_contribution = f4005, aorb = NA) %>%
  select (hhid, hhcid,  pline, hpf, hpf_time, hpf_contribution, aorb,
          a3000, a3014, a3016,a3003, a3020,
          new_ind_gender, new_ind_birthyear, a2003, a2005, hhead) %>% # do not need aorb
  rename ( new_hh_gender = a2003, new_hh_birthyear = a2005,
           have_job = a3000, job_sector = a3014, employer_type = a3016,
           # job_contract = a3132h, 
           job_emply = a3003, job_income = a3020) %>% mutate ( year = 2011) %>%
  mutate (#pension =  ifelse(f1001a %in% c(1,2,3,4,5,7777),1,0  ),
    job_sector = ifelse (is.na(job_sector), 'other',
                         ifelse  ( job_sector %in% c(1,2), 'gov',
                                   ifelse(job_sector == 3 & employer_type %in% c(1,2), 'soe',
                                                 ifelse(job_sector == 3 & employer_type %in% c(3), 'pvt',
                                                        ifelse(job_sector == 3 & employer_type %in% c(4,5,6), 'frg',
                                                               ifelse(job_sector == 7777 , 'other',
                                                                      'other')))  )))) %>%
  select (-employer_type)



df_ind_2013 = read.dta13('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2013/chfs2013_ind_20191120.dta') %>%
  inner_join (df_master_2013, by = 'hhid_2013') %>%
  mutate (new_ind_gender = NA, new_ind_birthyear = NA, pab = NA) %>%
  mutate (flg = 1*(a3000==1 & a3003!=3)) %>%
  mutate (hpf = ifelse(flg ==0 | is.na(flg), 2, f4001)) %>%
  mutate (hpf_time = f4007, hpf_contribution = f4005, aorb = NA) %>%
  select (hhid, hhcid,  pline,hpf, hpf_time, hpf_contribution, aorb,
          a3000, a3014, a3003, a3020,
          new_ind_gender, new_ind_birthyear, a2003, a2005, hhead) %>% # do not need aorb
  rename ( new_hh_gender = a2003, new_hh_birthyear = a2005,
           have_job = a3000, job_sector = a3014, 
           # job_contract = a3132h, 
           job_emply = a3003, job_income = a3020) %>% mutate ( year = 2013) %>%
  mutate (#pension =  ifelse(f1001a %in% c(1,2,3,4,5,7777),1,0  ),
          job_sector = ifelse (is.na(job_sector), 'other',
                               ifelse  ( job_sector == 1, 'gov',
                                         ifelse(job_sector == 2, 'soe',
                                                ifelse(job_sector == 6, 'ind',
                                                       ifelse(job_sector == 3, 'pvt',
                                                              ifelse(job_sector == 6, 'frg',
                                                                     ifelse(job_sector == 7777 , 'other',
                                                                            'other')))  )))) )


df_ind_2015 = read.dta13('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2015/Data/chfs2015_ind_20191120.dta') %>%
  inner_join (df_master_2015, by = 'hhid') %>% 
  mutate (flg = ifelse(a3000 ==1 & a3003b !=3,1,0)) %>% 
  mutate (hpf = ifelse(flg ==0 | is.na(flg), 2, f4001)) %>%
  mutate (hpf_time = f4007, hpf_contribution = f4005) %>%
  select (hhid, hhcid, pline, hpf, hpf_time, hpf_contribution, aorb,
          a3000, a3014a, a3003b, a3020, #a3020it,
          a1113, a1114,a2003, a2005, hhead) %>%
  rename (
    new_ind_gender = a1113, ## new ind in existing hh
    new_ind_birthyear = a1114,
    new_hh_gender = a2003, ## new hh
    new_hh_birthyear = a2005,
    have_job = a3000, job_sector = a3014a, 
    # job_contract = a3132h, 
    job_emply = a3003b, job_income = a3020, #job_income_itvl = a3020it
    ) %>% mutate (year = 2015) %>%## no information on area propensity
  mutate ( #pension =  ifelse(f1001a %in% c(1,2,3,4,5,7777),1,0  ),
    job_sector = ifelse (is.na(job_sector), 'others',
                         ifelse  ( job_sector %in% c(1,2), 'gov',
                                   ifelse(job_sector %in% c(4,3), 'soe',
                                          ifelse(job_sector == 9, 'ind',
                                                 ifelse(job_sector == 5, 'pvt',
                                                        ifelse(job_sector == 6, 'frg',
                                                               'others'))  )))))

df_ind_2017 = read.dta13('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2017/chfs2017_ind_202104.dta') %>%
  inner_join (df_master_2017, by = c('hhid','pline')) %>%
  mutate (flg = 1*(a3107==1|a3132a %in% c(1,2)|a3140 %in% c(1,2,3,4,5,6,7,7777)) ) %>%
  mutate (hpf = ifelse(flg == 0 | is.na(flg), 2, f4001)) %>%
  mutate  (hpf_time = f4007, hpf_contribution = f4005 ) %>%
  select (hhid, hhcid,pline, hpf, hpf_time, hpf_contribution,  pab,
          a3100, a3106, a3107, ##a3108, 
          a3136,#a3136it,
          a1113, a1114, a2003, a2005, hhead) %>%
  rename ( aorb = pab,new_ind_gender = a1113, new_ind_birthyear = a1114,
           new_hh_gender =  a2003, new_hh_birthyear = a2005,
           have_job = a3100, job_sector = a3106, #job_contract = a3108, 
           job_emply = a3107, job_income = a3136, #job_income_itvl = a3136it
           ) %>% mutate (year = 2017) %>%
  mutate ( #pension =  ifelse(f1001a %in% c(1,2,3,4,5,7777),1,0  ),
    job_sector = ifelse (is.na(job_sector), 'others',
                         ifelse  ( job_sector == 1, 'gov',
                                   ifelse(job_sector %in% c(2,3), 'soe',
                                          ifelse(job_sector == 4, 'ind',
                                                 ifelse(job_sector == 5, 'pvt',
                                                        ifelse(job_sector == 6, 'frg',
                                                               'others'))  )))))

df_ind_2019 = read.dta13('C:/Users/shufe/Dropbox/Doctoral Thesis/CHFS/2019/chfs2019_ind_202112.dta') %>%
  inner_join (df_master_2019, by = c('hhid','pline')) %>%
  mutate (flg = 1*(a3132d %in% c(1,2,3,4,5,6)|f1001a==2|f2001a==1 )) %>%
  mutate (hpf = ifelse ( flg == 0 | is.na(flg), 2, f4001)) %>%
  mutate (hpf_time = f4007, hpf_contribution = f4005) %>%
  select (hhid,hhcid,pline,hpf, hpf_time, hpf_contribution
          ,  pab,
          a3132b, a3132d, a3132c,##a3132h,
          a3136, #a3136it,
          a1113, a1114, a2003, a2005,hhead,
          #f1001a
          ) %>%
  rename (aorb = pab, new_ind_gender = a1113, new_ind_birthyear = a1114,
          new_hh_gender =  a2003, new_hh_birthyear = a2005,
          have_job = a3132b, job_sector = a3132c, #job_contract = a3132h,
          job_emply = a3132d, job_income = a3136, #job_income_itvl = a3136it 
          ) %>% mutate (year = 2019) %>%
  mutate (#pension =  ifelse(f1001a %in% c(1,2,3,4,5,7777),1,0  ),
          job_sector = ifelse (is.na(job_sector), 'others',ifelse  ( job_sector == 1, 'gov',
                                                                     ifelse(job_sector == 2, 'soe',
                                                                            ifelse(job_sector == 4, 'ind',
                                                                                   ifelse(job_sector == 5, 'pvt',
                                                                                          ifelse(job_sector == 6, 'frg',
                                                                                                 ifelse(job_sector == 7777 , 'others',
                                                                                                        'others')))  )))) )
# combine all the waves ind info and filter some unnecessary rows
# It not not be a good idea excluding 
df_ind = bind_rows (df_ind_2011, df_ind_2013) %>%
  bind_rows(.,df_ind_2015) %>%  bind_rows(.,df_ind_2017) %>% bind_rows(.,df_ind_2019) %>%
  arrange(hhid, pline, year) %>%
  mutate (gender = ifelse(!is.na(new_hh_gender), new_hh_gender, 
                          ifelse(new_ind_gender, new_ind_gender, NA)),
          birthyear = ifelse(!is.na(new_hh_birthyear), new_hh_birthyear, 
                             ifelse(new_ind_birthyear, new_ind_birthyear, NA)) ,
          age = year - birthyear) %>% mutate (hhead = as.character(hhead)) %>%
  select (-new_hh_gender, -new_ind_gender, -new_hh_birthyear, -new_ind_birthyear) %>%
  filter (hhead == 1) %>%  # only want hhead
  mutate (hpf = ifelse(is.na(hpf) | hpf == 2,0, hpf)) %>% # turn na hpf to 0
 # filter (age<=65) %>% # Exclude old people
 # filter (have_job ==1 ) %>%
 # filter(!(job_sector %in% c('others','ind')) & !is.na(job_income)) %>% # these sectors almost have no hpf participation
  mutate (hpf = as.factor(hpf), year = as.factor(year)) %>%
 # mutate (hsng_prchs_prpsty = ifelse(hsng_prchs_prpsty ==4, 0, 1)) %>%
  mutate (hpf_time = ifelse(hpf == 0, 0, hpf_time)) %>%
  mutate (hpf_contribution = ifelse(hpf == 0, 0, hpf_contribution)) 

b = df_ind  %>% group_by(hhid) %>% mutate (hhead_change = length(unique(pline))) %>%
  ungroup(.) %>% filter (hhead_change >1) %>% arrange(hhid, year) %>% select (hhid, year, pline, birthyear, age)

################################################################################


## find those household who had not purchased a house when they entered the interview
## also, we want to correct the incorrect purchasing recordd.
df_house_history_refined = df_house_history %>%
  group_by(hhid, area) %>% 
  mutate (min_yr_same_area = min (yr)) %>%
  ungroup(.) %>%  
  filter (yr != min_yr_same_area) %>%
  select (- min_yr_same_area) %>%
  ## in the data, house purchase records have the same area but different pruchase year. 
  ## regard the ones with larger years as incorrect records. 
  mutate (area = ifelse (src %in% c('legacy','torndown'),0, area)) %>% # if the house is obtained by torndown or legacy, then set area = 0
  mutate (join_survey_yr = substr(hhid, 1,4)) %>%  ## get survey joining yr for each household
  group_by(hhid) %>%
  mutate (record_num = n(), house_purchase_times = sum(yr != 'NA'), min_yr = min(yr)) %>%
  ungroup(.) #%>%
 # filter (min_yr>join_survey_yr)  # select out those household whose first house purchasing happens later than the survey joining year

## the size of such household?
length(unique(df_house_history_new_refined$hhid)) ## 27278, seems large



## check the completeness of house purchase info. seems fine
df_house_history_new_buyer%>%
  filter ( (src != 'NA'& area !='NA' & yr!='NA') |  (src == 'NA'& area =='NA' & yr=='NA')) %>% nrow(.)

## see for these households, do they experience the change of hpf status during the window of investigation
df_ind %>%
  filter (hhid %in% df_house_history_new_buyer$hhid) %>%
  group_by(hhid) %>%
  summarize (unique_hpf_time = length(unique(hpf)) ) %>%
  ungroup(.) %>% group_by(unique_hpf_time) %>% summarize (num = n())

## also see the ratio of hpf participation and 
df_ind %>%
  filter (hhid %in% df_house_history_new_buyer$hhid) %>%
   group_by(hpf) %>% summarize (num = n())

## next is to create a panel, by joining df_house_history_new_buyer with df_ind
## This panel have year 2011, 2013, 2015, 2017, 2019





