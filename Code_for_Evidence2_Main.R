library (openxlsx)
library (dplyr)
library (tidyr)
library (ggplot2)
library (methods)
library (tidymodels)
library (plm)
library(rlang)
library(foreign)
library(sandwich)
library(lmtest)
library(VGAM)
library(censReg)
library (data.table)
library (stargazer)

# Run this function first !
reg_hc_ineq = function (key_x, lagyear,target, cov_expr, crs_term,
                        is_panel ){
  
  # key_x : name of inequality variable
  # lagyear : the lag of years
  # target : the name of dependent variable
  # cov_expr: expression for covariate
  
  # generate dependent variable
  if (target != 'edu') {
    df_dep = generate_dependent (target)%>%
      # calculate the diff
      mutate(diff := !!sym(target) - shift(!!sym(target), lagyear), by = Country.Code)%>%
      select (-c(by, !!sym(target)) ) %>%
      rename (!!sym(target) := diff)
  }else{
    df_dep = read.xlsx ('./edu.xlsx') %>%
      rename (Country.Code = Code, year = Year) %>%
      # calculate the diff
      mutate(diff := edu - shift(edu, lagyear), by = Country.Code)%>%
      select (-c(by, edu) ) %>%
      rename (edu = diff)
  }
 
  # generate the data for inequality
  df_ineq = inequality_generate (key_x, lagyear)

  # want to
  df_ineq_gdp = df_gdp %>%
    mutate (year_original = as.numeric(year)) %>%
    mutate (year = year_original + lagyear) %>%
    select (-c(year_original)) %>%
    inner_join (.,df_ineq,by = c('Country.Code','year'))%>%
    mutate (gdp = log(gdp)) %>% 
    mutate (gdp2 = gdp^2)%>%
    na.omit(.)

  # since gini and gdp may be highly correlated, 
  # first regress gini on gdp to extract residual
  eqs1 = paste0(key_x, '~gdp + gdp2')%>%parse_expr(.)%>% eval(.)

  resid_ineq = lm ( eqs1, data = df_ineq_gdp)%>%
    .[['residuals']] %>% as.data.frame(.)

  colnames(resid_ineq) = 'residual' 
  # specify the variable to be standardized
  std_list = c(target, key_x, 'gdp','gov','t','urban')
  
  # generate some covariates
  df_capital = generate_covariate ('capital', lagyear)
  df_gov = generate_covariate ('gov', lagyear)
  df_urban = generate_covariate ('urban', lagyear)
 
  df_reg = df_ineq_gdp %>%
    select (-!! sym(key_x)) %>%
    cbind(resid_ineq) %>%
    rename ( !! sym(key_x) := residual ) %>%
    # join the dependent var data
    inner_join (., df_dep, by = c('Country.Code','year'))%>%
    # join some covariate
    inner_join (., df_gov, by = c('Country.Code','year')) %>%
    inner_join (., df_urban, by = c('Country.Code','year'))%>%
    na.omit(.) %>%
    mutate ( t = year - 1960) %>%  # generate a time variable 
    std_df (., all_of (std_list))  # standardization
    #mutate (gdp2 = gdp**2)

 
  if (crs_term==1) { # need gdp*ineq term
    eqs = paste0( target,'~','gdp','+', key_x, 
              '+t ',cov_expr ,'+gdp2','+gdp*', key_x )%>%
    parse_expr(.)%>% eval(.)
  }else{ 
    eqs = paste0( target,'~','gdp','+', key_x,
                  '+t ',cov_expr ,'+gdp2')%>%
    parse_expr(.)%>% eval(.)
  }


  if (is_panel == 1) { # do panel regression ! 
    reg_res = df_reg %>%
      select (-c(year_original)) %>%
      pdata.frame(., index = c('Country.Code','year'))%>%
      plm (eqs, data = ., model = 'within') 

    list ( n = length(reg_res[['residuals']]), 
        coef = coeftest(reg_res, vcov.=vcovHC(reg_res, type='HC1')) )
      # report the robust standard error
  }else{ # although we provide the choice for ols, we aolmost always use panel.
    reg_res = df_reg %>%
      select (-c(year_original)) %>%
      lm (eqs, data = .) 
   
    list (n = length(reg_res[['residuals']]), 
     coef = coeftest(reg_res, vcov = sandwich) )
  }
  
}

reg_hc_ineq ('gini_corrected',5,'edu',
             '+urban +gov + urban', 1, 1)[['coef']]

#--------------------------------------
# Here comes the output
list (
reg1 = reg_hc_ineq ('gini_corrected',5,'edu',
                 '+urban +gov + urban', crs_term = 1, is_panel =1)[['coef']],
reg2 = reg_hc_ineq ('gini_corrected',5,'edu',
                  '+urban +gov + urban', crs_term = 0, is_panel =1)[['coef']],
reg1 = reg_hc_ineq ('gini_reported',5,'edu',
                    '+urban +gov + urban', crs_term = 1, is_panel =1)[['coef']],
reg2 = reg_hc_ineq ('gini_reported',5,'edu',
                    '+urban +gov + urban', crs_term = 0, is_panel =1)[['coef']]
)%>%stargazer(.)


list (
 reg1 = reg_hc_ineq ('q5',5,'edu',
                     '+urban +gov ', crs_term = 1, is_panel =1)[['coef']],
 reg2 = reg_hc_ineq ('q5',5,'edu',
                     '+urban +gov ', crs_term = 0, is_panel =1)[['coef']],
 reg3 = reg_hc_ineq ('q234',5,'edu',
              '+urban +gov + urban', crs_term = 1, is_panel =1)[['coef']],
 reg4 = reg_hc_ineq ('q234',5,'edu',
                     '+urban +gov ', crs_term = 0, is_panel =1)[['coef']],
 reg5 = reg_hc_ineq ('q1',5,'edu',
              '+urban +gov ', crs_term = 1, is_panel =1)[['coef']],
 reg6 = reg_hc_ineq ('q1',5,'edu',
                     '+urban +gov ', crs_term = 0, is_panel =1)[['coef']]
)%>%stargazer(.)

 
 