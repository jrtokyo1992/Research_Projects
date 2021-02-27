library (openxlsx)
library (dplyr)
library (tidyr)
library(purrr)
library(rlang)

# the function for standardization.
std_df = function (df, reg){ 
  
  var_max = df %>% select (reg)%>%
    map (max) %>% as_tibble()
  var_min = df %>% select (reg)%>%
    map (min) %>% as_tibble()
  max_min_diff = var_max - var_min
  var_name_std = names(var_max)
  
  eqs = map(var_name_std, 
            ~parse_expr(paste0 ('(',.,'-', var_min[[.]],
             ')/(',max_min_diff[[.]],')')))
  
  names(eqs)= var_name_std
  
  df %>%
    mutate (!!! eqs)  }

# input the gdp data.

df_gdp = read.xlsx('./gdp.xlsx')%>%
  pivot_longer(-c('Country.Name','Country.Code' ),
               names_to = 'year', values_to = 'gdp')%>%
  select (-c('Country.Name'))


# input the inequality data and do some simple transformation.

df_inequality =  read.xlsx('./inequality.xlsx') %>%
  mutate(gini_corrected = 0.8*(-1 + 2*q5 + 1.5*q4 + q3 + 0.5*q2)) %>%
  mutate ( latin = ifelse (region_wb =="Latin America and the Caribbean",
                          1, 0)) %>%
  mutate (africa = ifelse (region_wb %in% c("Middle East and North Africa",
          "Sub-Saharan Africa"), 1,0 )) %>%
  mutate (asia = ifelse (region_wb %in% c( "South Asia" ,
          "East Asia and the Pacific" ), 1,0 )) %>%
  mutate (europe = ifelse (region_un_sub %in% 
          c("Southern Europe","Northern Europe","Western Europe" ),1,0))%>%
  mutate (q5q1 = q5/q1) %>% # the ratio of the income share of q5 to q1
  mutate (q4q2 = q4/q2) %>% # the ratio of the income share of q4 to q2
  mutate (q234 = q2 + q3 +q4) # the total share of q2,q3,q4
  

# ------the function to generate the inequality measurement we want. 
# var_name can be any measurement of inequality
inequality_generate = function (var_name, lagyear){
  df_inequality %>%
    rename( Country.Code = c3, year_original = year) %>%
    #filter (scale == 'Equivalized') %>%
    filter (! is.na ( !! sym(var_name) ) ) %>%
    group_by (Country.Code, year_original ) %>%
    summarize (!! sym(var_name) := mean (!! sym(var_name)),
               latin = mean(latin),
               africa = mean (africa),
               asia = mean (asia),
               europe = mean (europe)
    ) %>%
    ungroup(.) %>%
    mutate (year = year_original + lagyear) }


# -----the function to generate the data for covariates
# var_name can be gov, capital, urban and etc...
generate_covariate = function(var_name, lagyear) {
  
  read_name = paste0 ('./', var_name,'.xlsx') 
    
  read.xlsx(read_name) %>%
    pivot_longer(-c('Country.Name', 'Country.Code'), 
                 names_to = 'year', 
                 values_to = var_name) %>%
    mutate (year_original = as.numeric(year)) %>%
    mutate (year = year_original + lagyear) %>%
    select (-c(Country.Name, year_original))
}

# test
#df_gov = generate_covariate ('gov')

#-------function to generate dependent variables

generate_dependent = function (var_name){
  
  read_name = paste0 ('./', var_name,'.xlsx') 
  
  df_temp = read.xlsx(read_name) %>%
    pivot_longer(-c('Country.Name', 'Country.Code'), 
                 names_to = 'year', 
                 values_to = var_name) %>%
    mutate (year = as.numeric(year)) %>%
    select (-c(Country.Name))
  }


df_edu = read.xlsx ('./edu.xlsx') #%>%
  rename (Country.Code = Code, year = Year) %>%
  mutate(diff := Edu - shift(Edu, LAGYEAR), by = Country.Code)%>%
  select (-c(by, Edu) ) %>%
  rename (Edu = diff)

df_edu%>% ggplot(aes(x = edu)) + geom_density()
# also plot the distribution for those huma capital measurement
# all seems to have a censored distribution 
# may be good calcualate the difference  

  
