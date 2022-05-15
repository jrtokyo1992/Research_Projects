

# load the data cleaning process
source('./data_prep_0310.R')

year_point = seq(1912, 1967, by = 5)
#year_point = seq(1915, 1980, by = 5)

df_reg = df_final %>%
  filter (birthyear <=1967 & birthyear > 1912) %>%
  #filter (birthyear<1980 & birthyear > 1915) %>%
  mutate(cohort=cut(birthyear, breaks = year_point )) %>%
  group_by ( cohort, datayear) %>%
  summarize ( house = mean (house), 
              age = mean (age),
              hhsize = mean (hhsize),
              cons = mean (log(cons+1)), # for consumption, do log
              liquid = mean (liquid))


estimation = function (dep_name, df) {

df_test = df %>%
  dummy_cols(.data = ., select_columns = 'datayear',
             remove_first_dummy = TRUE) %>%
  dummy_cols(.data = ., select_columns = 'cohort',
             remove_first_dummy = TRUE) %>%
  select (- c(cohort, datayear)) 

cohort_name = df_test %>% select (starts_with ('cohort'))%>% colnames(.)
cohort_name_new = map( letters[1:length(cohort_name)], ~paste0('cohort_', .)) %>%
  unlist(.)  

s_matrix = matrix (0, nrow(df_test), nrow(df_test))
for (i in 1: nrow(df_test)) {
  for (j in 1: nrow(df_test)){
    s_matrix[i,j] = kernel_func (df_test$age[i]-df_test$age[j], 6.25) 
  }
  s_matrix[i,]= s_matrix[i,]/sum(s_matrix[i,])
}

dep_resid = paste0(dep_name,'_resid')

df_test_1 = df_test %>%
  rename_at (all_of(cohort_name), ~cohort_name_new)%>%
  #mutate (house_resid = house - s_matrix %*% house) %>%
  mutate (!!sym(dep_resid):= !!sym(dep_name) - s_matrix %*% !!sym(dep_name)) %>%
  mutate (across(starts_with(c('cohort','datayear')), ~ .- s_matrix %*% .,
                 .names ='{col}_resid')) 

  eqs = df_test_1 %>% 
    select (starts_with (c('cohort', 'datayear'))) %>%
    select (ends_with ('resid')) %>%
    colnames (.) %>%
    Reduce (function(a,b) {paste0(a,'+',b)}, .)%>%
  paste0(dep_resid,'~',.)%>%
  parse_expr(.) %>%
  eval(.)

# do the regression to get coefficient 
  coef = lm(data = df_test_1, formula = eqs)$coefficients
  #coeftest(., vcov = sandwich) 

  X_beta = df_test_1 %>% 
    mutate (const = 1) %>%
    select (- ends_with ('resid')) %>% # remove the residual var
    select ( const, starts_with (c('cohort','datayear'))) %>%
    # get the original x var
    as.matrix.data.frame (.) %>%
    sweep (2,coef, '*') %>%
    rowSums (.)  # get x* beta

  # now we have the y-x*beta
  # we want to evaluate from age 20 to age 90

  resid_new = df_test_1[dep_name] - X_beta

  age_eval_vct = 21:90
 
  res = NULL
  for (i in 1: length(age_eval_vct)){
    kernel_vct  = map(df_test_1$age, ~kernel_func(age_eval_vct[i]-., 6.25))%>%unlist(.)
    res = c(res, sum(kernel_vct * resid_new )/sum (kernel_vct))
  }

  age_eval_itvl = map(seq(21, 90, by = 5), ~rep(.,5))%>%unlist(.)
  cali_res = read.table(paste0('./life_',dep_name, '.txt')) %>%
    cbind(age_eval_vct, res , age_eval_itvl) %>%
    filter (age_eval_vct <=85) %>%
    rename (model = 1, age = 2, data =3, age_interval = age_eval_itvl) %>%
    group_by (age_interval) %>%
    summarize (model = mean (model),
               data = mean (data)) %>%
    rename (age = age_interval) %>%
    na.omit(.)

  cali_dep = cali_res %>% 
    # scaling the data
    mutate (across(c('model', 'data'), ~(.-min(.))/(max(.)-min(.))  )) %>%
    pivot_longer( c('model', 'data'), 
                  values_to = dep_name, names_to = 'type') %>%
    ggplot (aes (x = age, y = !!sym(dep_name), linetype = type)) +
    geom_line (size = 1) +
    labs (x = 'Age',y = dep_name) +
    theme(axis.text = element_text (size = 30) ,
        axis.title = element_text (size = 30, hjust =0.5),
        legend.text = element_text (size = 30),
        legend.title = element_text(size = 30))


  ggsave(paste0('cali_',dep_name, '.jpg'), plot = cali_dep, 
       height = 9, width = 16, dpi = 'retina')  
}

# Here comes the output
estimation('cons',df_reg)
estimation('house',df_reg)
estimation('liquid',df_reg)

