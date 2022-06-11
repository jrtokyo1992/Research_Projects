
# Data Processing 2 - 1
# Do not consider the changing of household head.

df_final = df_house_history_refined %>%
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
  #  mutate (hhid_new = paste0(hhid,'_', pline)) %>% 
  group_by (hhid) %>% mutate (min_yr = min(year)) %>% 
  ungroup(.) #%>% filter (!(year = min_yr& area >0))


### investigation on window.
#### those enter interview in year 2011. 
df_final_2011_panel = df_final %>% group_by (hhid) %>% mutate (min_yr = min(year)) %>% ungroup(.) %>%
  filter (min_yr == 2011)%>%
  filter (year %in% c(2011,2013,2015)) %>%
  group_by (hhid) %>% mutate (record_num = length(year)) %>% 
  ungroup(.) %>%
  filter (record_num == 3) %>% arrange(hhid)


df_final_2013_panel = df_final %>% group_by (hhid) %>% mutate (min_yr = min(year)) %>% ungroup(.) %>%
  filter (min_yr == 2013) %>%
  filter (year %in% c(2013,2015,2017)) %>%
  group_by (hhid) %>% mutate (record_num = length(year)) %>% 
  ungroup(.) %>%
  filter (record_num == 3) %>% arrange(hhid)


df_final_2015_panel = df_final %>% group_by (hhid) %>% mutate (min_yr = min(year)) %>% ungroup(.) %>%
  filter (min_yr == 2015) %>%
  filter (year %in% c(2015,2017, 2019)) %>%
  group_by (hhid) %>% mutate (record_num = length(year)) %>% 
  ungroup(.) %>%
  filter (record_num == 3) %>% arrange(hhid)

df_final_panel = bind_rows(df_final_2011_panel, df_final_2013_panel) %>%
  bind_rows(df_final_2015_panel) %>%
  group_by (hhid) %>% 
  mutate (min_age = age[year == min(year)], 
          initial_have_job = have_job[year == min(year)],
          initial_area = area[year == min(year)],
          hhead_change = length (unique (pline))) %>% 
  ungroup(.)%>% filter (min_age<=60, # exclude those who start with 60 
                        initial_have_job == 1, 
                        initial_area ==0, # only include those who has no house.
                        hhead_change == 1 # only include those who did not experience hhead change
  ) #%>% # this excluded those people who are very old

## here we need some investigation on the hhead_change == 1 filtering. 

## check the ownership rate 
df_final_panel %>% group_by(hhid) %>% summarize(max_house = max(area)) %>% ungroup(.) %>%
  summarize (ownership = sum(max_house>0)/n())

## check the change of hh head 

### hpf_time

df_final_panel %>%
  filter (!is.na(hpf_time)) %>% ggplot(aes(x = hpf_time)) + geom_density()

df_final_panel %>% 
  filter (!is.na(hpf_time)) %>% summarize (max = max(hpf_time), min = min(hpf_time))

a = df_final_panel %>% filter (hpf_time == 9999)
df_final_panel %>% filter (hhid == '2013024290')

# from the df_final_panel  table, create a cross-section data 

df_final_cs = df_final_panel %>% group_by (hhid, job_sector) %>%
  mutate (job_sector_freq = n() ) %>% ungroup (.) %>% 
  group_by (hhid, job_emply) %>%
  mutate (job_emply_freq = n() ) %>% ungroup (.) %>% 
  group_by(hhid) %>%
  summarize (hpf_time = hpf_time[year == min(year)], 
             house= max(area), 
             job_sector = job_sector[job_sector_freq == max(job_sector_freq)],
             job_emply = job_emply[job_emply_freq == max(job_emply_freq)]
  )

df_final_panel %>% filter (hhid == '201100018')
