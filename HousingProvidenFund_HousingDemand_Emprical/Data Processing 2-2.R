library(dplyr)
library(readstata13)
library(ggplot2)
library(tidyverse)
library(stringr)
# Data Processing 2 - 2
# consider the changing of household head.

df_final_headchange = df_house_history_refined %>%
  # mutate (area = as.numeric(area)) %>%
  rowwise(.) %>%
  mutate (area = ifelse(area == 'NA', 0, as.numeric(area))) %>%
  ungroup(.) %>%
  mutate (h2013 = ifelse(yr<=2013, 1,0),
          h2015 = ifelse(yr<=2015, 1, 0),
          h2017 = ifelse(yr<=2017, 1, 0),
          h2019 = ifelse(yr<=2019, 1, 0)) %>%
  group_by (hhid) %>%
  mutate (across(h2013:h2019, ~sum(area*.) )) %>%
  ungroup(.) %>%
  select (hhid, h2013:h2019) %>% 
  distinct(.) %>%
  pivot_longer (-c(hhid), names_to = 'year', values_to = 'area') %>%
  mutate(year = substr(year, 2,5)) %>% # we have create a panel recording housing 
  right_join (df_ind, by = c('hhid', 'year')) %>%
  mutate (area = ifelse (is.na(area), 0, area), year = as.numeric(year)) %>%
  mutate (hhid_new = paste0(hhid,'_', pline)) %>% 
  group_by (hhid_new) %>% mutate (min_yr = min(year)) %>% 
  ungroup(.) %>% select (-hhid)


### investigation on window.
#### those enter interview in year 2011. 
df_final_2011_panel_headchng = df_final_headchange %>% group_by (hhid_new) %>% mutate (min_yr = min(year)) %>% ungroup(.) %>%
  filter (min_yr == 2011)%>%
  filter (year %in% c(2011,2013,2015)) %>%
  group_by (hhid_new) %>% mutate (record_num = length(year)) %>% 
  ungroup(.) %>%
  filter (record_num == 3) 


df_final_2013_panel_headchng = df_final_headchange %>% group_by (hhid_new) %>% mutate (min_yr = min(year)) %>% ungroup(.) %>%
  filter (min_yr == 2013) %>%
  filter (year %in% c(2013,2015,2017)) %>%
  group_by (hhid_new) %>% mutate (record_num = length(year)) %>% 
  ungroup(.) %>%
  filter (record_num == 3) 


df_final_2015_panel_headchng = df_final_headchange %>% group_by (hhid_new) %>% mutate (min_yr = min(year)) %>% ungroup(.) %>%
  filter (min_yr == 2015) %>%
  filter (year %in% c(2015,2017, 2019)) %>%
  group_by (hhid_new) %>% mutate (record_num = length(year)) %>% 
  ungroup(.) %>%
  filter (record_num == 3) 

df_final_panel_headchng = bind_rows(df_final_2011_panel_headchng, df_final_2013_panel_headchng) %>%
  bind_rows(df_final_2015_panel_headchng) %>%
  group_by (hhid_new) %>% 
  mutate (min_age = age[year == min(year)], 
          initial_have_job = have_job[year == min(year)],
          initial_area = area[year == min(year)],
          hhead_change = length (unique (pline))) %>% 
  ungroup(.)%>% filter (min_age<=60, # exclude those who start with 60 
                        initial_have_job == 1, 
                        initial_area ==0, # only include those who has no house.
                      #  hhead_change == 1 # only include those who did not experience hhead change
  ) #%>% # this excluded those people who are very old


a = df_final_panel_headchng %>% group_by(hhid_new) %>%
  mutate (na_hpf_time_num = sum(is.na(hpf_time))) %>% ungroup(.) %>% 
  filter (na_hpf_time_num >0) %>% arrange (hhid_new, year)

## here we need some investigation on the hhead_change == 1 filtering. 
df_final_panel_headchng %>% group_by(hhid_new) %>% summarize (num = n()) %>% ungroup(.) %>%
  group_by(num) %>% summarize (num_num = n())

## check the ownership rate 
df_final_panel_headchng %>% group_by(hhid_new) %>% summarize(max_house = max(area)) %>% ungroup(.) %>%
  summarize (ownership = sum(max_house>0)/n())

## HPF status change during. Consider it as an predicted???? endogeneous.......
df_final_panel_headchng %>%
  group_by (hhid_new) %>% summarize (hpf_end = hpf[year == max(year)],
                                     hpf_initial = hpf[year == min(year)]) %>%
  ungroup(.) %>% mutate (hpf_change = as.numeric(hpf_end) - as.numeric(hpf_initial) )%>%
  group_by (hpf_change) %>% summarize (num = n())


# from the df_final_panel  table, create a cross-section data 

df_final_cs_headchng = df_final_panel_headchng %>% 
  ##group_by (hhid_new, job_sector) %>%
  ##mutate (job_sector_freq = n() ) %>% ungroup (.) %>% 
  ##group_by (hhid_new, job_emply) %>%
  ##mutate (job_emply_freq = n() ) %>% ungroup (.) %>% 
  group_by(hhid_new) %>%
  summarize (initial_hpf_time = hpf_time[year == min(year)], 
             house= max(area), 
             age = age[year == min(year)],
             initial_sector = job_sector[year == min(year)],
             initial_hukou = hukou[year == min(year)],
             enter_year = min(year),
             initial_hpf = hpf[year == min(year)],
             initial_income = job_income[year == min(year)],
             hpf_change = as.numeric (hpf[year == max(year)]) - as.numeric(hpf[year == min(year)])
            # job_sector = job_sector[job_sector_freq == max(job_sector_freq)],
       #      job_emply = job_emply[job_emply_freq == max(job_emply_freq)]
  ) %>%
  mutate (initial_hpf_time = ifelse(initial_hpf_time> 300, 300, initial_hpf_time)) %>%
  filter( initial_hukou != 'rural') # get rid of unrealistic hpf_time 
  ## also get rid of the rural resident. 
   


df_final_cs_headchng %>% group_by(initial_hpf) %>% summarize (avg_house = mean(house))

################################################################Some EDA: 

# home ownership (ratio of people who purchased a house later.)
df_final_cs_headchng %>% summarize (sum(house>0)/n())

# The distribution of hpf and house by age.
b = df_final_cs_headchng %>% group_by(age) %>% summarize (
  num = n(),
  ratio_hpf = sum(initial_hpf ==1)/n(),
  avg_house = mean(house)) %>%
  ungroup(.) %>% arrange(age)

## Check the correlation between hpf_time and age?
df_final_cs_headchng %>% 
  filter (!is.na(hpf_time) & !is.na(age) ) %>%
  summarize (cor_hpftime_age = cor(hpf_time, age))


