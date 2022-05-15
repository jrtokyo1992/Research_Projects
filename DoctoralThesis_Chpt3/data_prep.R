library(readstata13)
library(dplyr) 
library(purrr)
library(broom) 
library(ggplot2) 
library('fastDummies')
library(rlang)
library (tidyr)
library(lmtest)
library(sandwich)
theme_set ( theme_minimal (base_size = 11))

# compared to 'data_pre_0306', we deflate consumption by cpi
# we also calculate equivalence scale.

#------------for 1988------------
df1988_ind=read.dta13("./Data/1988ind.dta",nonint.factors=TRUE,generate.factors=T)%>%
  mutate (age = as.numeric(V105)) %>%
  mutate (hhhead = V103) %>% 
  filter (hhhead == 'Self') %>%
  mutate (hhcode = as.character(UCODE ))

df1988_hh=read.dta13("./Data/1988hh.dta",nonint.factors=TRUE,generate.factors=T)%>%
  mutate (hhcode = as.character(UCODE))%>%
  mutate (house = ifelse( V413 ==5 | V413 == 2, V403, 0)) %>%
  select (-ends_with(c('JA','JB','YA'))) %>%
  mutate (across (c(V501YB: V520YB), ~ifelse(. > 999, 0, .))) %>% # food 
  mutate (across (c(V521,V524,V525,V538), ~ifelse(. > 999, 0, .))) %>%# others
  mutate (across (c(V301:V303), ~ifelse (. == 'Missing', 0, .))) %>% # deposit income
  mutate (debt = ifelse (V322 == 'Missing', 0, V322)) %>%
  rowwise (.)%>%
  mutate (cons_food = sum(c_across ( c(V501YB: V520YB))) *12, # original data is monthly
          finasset = sum (c_across( c(V301:V303)))/0.08) %>% # 0.1 is the interest rate
  ungroup(.) %>%
  mutate (cons = cons_food + ( V521+V524+V525)*12 +V538) %>%
  mutate (liquid = finasset - debt)

df1988_hhsize = read.dta13("./Data/1988ind.dta",nonint.factors=TRUE,generate.factors=T)%>%
  mutate (hhcode = as.character(UCODE), hhhead = V103) %>%
  mutate (size_weight = ifelse (hhhead == 'Self',1, 
                                ifelse (hhhead %in% c('Child','Grandchild'), 0.5,0.7)))%>%
  group_by(hhcode) %>%
  summarize( hhsize = sum(size_weight) )%>%
  ungroup(.)


df1988_final = df1988_hh%>%
  inner_join (df1988_ind, ., by = 'hhcode') %>%
  inner_join (df1988_hhsize, ., by = 'hhcode')%>%
  mutate (datayear = 1988) %>%
  mutate (birthyear = datayear - age) %>%
  select (c(hhcode, age, hhsize, house, liquid, cons, datayear, birthyear))
#-----------for 1995-------------

df1995_ind=read.dta13("./Data/1995ind.dta",nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhhead=A3, hhcode = N1)%>%
  rename(age=A5)%>%
  filter(hhhead=="self")%>%
  select(c(hhcode,age))


df1995_hh=read.dta13("./Data/1995hh.dta",nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhcode=N1)%>%
  rename(finasset=H1)%>%
  mutate(liquid=finasset-H13+H14)%>%
  mutate(house = ifelse (
  H63 %in% c('self-purchased private house','inherited old private house'), H56,0) )%>%
  mutate(cons=H53)#%>%
  
 
df1995_hhsize = read.dta13("./Data/1995ind.dta",nonint.factors=TRUE,generate.factors=T)%>%
  mutate (hhhead = A3,hhcode = N1) %>%
  mutate (size_weight = ifelse (hhhead == 'Self', 1, 
                                ifelse (hhhead %in% c('child','child in law','grandchild'), 0.5, 0.7) ))%>%
  group_by(hhcode) %>%
  summarize( hhsize = sum(size_weight)) %>%
  ungroup(.)

df1995_final = df1995_hh %>%
  inner_join(df1995_ind,.,by="hhcode") %>%
  inner_join (df1995_hhsize,., by = 'hhcode')%>%
  mutate(datayear=1995) %>%
  mutate (birthyear = datayear - age) %>%
  select (c(hhcode, age, hhsize, house, liquid, cons, datayear, birthyear))%>%
  na.omit(.)
#----------for 1999--------------

df1999_ind=read.dta13("./Data/UHIP1999ind.dta",
                  nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhcode=a100a)%>%
  filter(a104==1 | a104==3)%>% # filter the urban
  filter(a103 == 1)%>% # filter the the household head
  rename(age=a106) %>%
  select(c(hhcode, age)) 
  

df1999_hh = read.dta13("./Data/UHIP1999hh.dta",
                          nonint.factors=TRUE,generate.factors=T)%>%
  rename (hhcode = a100a) %>%
  mutate(finasset=a401-a409)%>%
  mutate(liquid=finasset-(a416-a417))%>%
  mutate(house=ifelse ( a609 == 4 | a609 == 3,  a601, 0))%>% # house area
  mutate(cons=a500) #%>%
 

df1999_hhsize = read.dta13("./Data/UHIP1999ind.dta",
           nonint.factors=TRUE,generate.factors=T) %>%
  rename(hhcode=a100a, hhhead = a103)%>%
  mutate (size_weight =ifelse (hhhead == 1, 1, 
                               ifelse (hhhead == 3| hhhead ==4 | hhhead ==5,0.5,0.7)))%>%
  group_by (hhcode) %>%
  summarise (hhsize = sum(size_weight)) %>%
  ungroup(.)

df1999_final = df1999_hh %>%
  select (c(hhcode, liquid, house, cons)) %>%
  inner_join (df1999_ind, ., by = 'hhcode') %>%
  inner_join (df1999_hhsize, ., by = 'hhcode')%>%
  mutate (datayear = 1999)%>%
  mutate (birthyear = datayear - age) %>%
  select (c(hhcode, age, hhsize, house,liquid,  cons, datayear, birthyear))%>%
  na.omit(.)
  
  

#----------for 2002 ------
df2002_4=read.dta13("./Data/2002_4.dta",nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhcode=PCODE)%>%
  mutate(cons=E)%>%
  mutate(house =ifelse ( B24 == 4 | B24 ==5 | B24 ==3,B23,0)) %>%
  select (c(hhcode, cons, house))
  

df2002_1=read.dta13("./Data/2002_1.dta",nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhcode=PCODE)%>%
  rename(hhhead=P103)%>%
  rename(age=P106)%>%
  filter(hhhead=="Self") %>%
  select (c(hhcode, age))
 
df2002_2=read.dta13("./Data/2002_2.dta",nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhcode=PCODE)%>%
  rename(mortgage=H417)%>%
  mutate(finasset=H401-H409-H411)%>%
  mutate(liquid=finasset-H416+mortgage) %>%
  select (c(hhcode, liquid))

df2002_hhsize = read.dta13("./Data/2002_1.dta",nonint.factors=TRUE,generate.factors=T)%>%
  rename(hhcode=PCODE, hhhead = P103)%>%
  mutate(size_weight = ifelse ( hhhead == 'Self', 1, 
                           ifelse( hhhead %in% c('Child','Grandchild','Child in law')
                                   , 0.5, 0.7 )))%>%
  select (hhcode, hhhead, size_weight) %>%
  group_by (hhcode) %>%
  summarize (hhsize = sum(size_weight))

df2002_final=inner_join(df2002_1,df2002_2,by="hhcode")%>%
  inner_join (., df2002_4, by = 'hhcode') %>%
  inner_join (., df2002_hhsize, by = 'hhcode') %>%
  mutate(datayear = 2002) %>%
  mutate (birthyear = datayear -age) %>%
  select (c(hhcode, age, hhsize, house, liquid, cons, datayear, birthyear))%>%
  na.omit(.)

df_cpi = data.frame (c(1988,1995,1999,2002),
                     c(33.187, 74.08, 80.689, 80.955)) 
colnames(df_cpi) = c('datayear','cpi')

df_final = rbind (df1988_final, df1995_final, 
                  df1999_final, df2002_final)%>%
  inner_join (df_cpi, by = 'datayear') %>%
  mutate (cons = cons/(cpi*hhsize), # deflate 
          house = house/hhsize,
          liquid = liquid/(cpi*hhsize) )
  

rm(df1988_hh,df1988_ind, df1988_hhsize,
   df1995_hh,df1995_ind, df1995_hhsize,
   df1999_hh,df1999_ind, df1999_hhsize,
   df2002_1,df2002_2,df2002_4,df2002_hh,df2002_ind, df2002_hhsize,
   df2002_final, df1999_final, df1995_final, df1988_final)

kernel_func = function (u,h){
  (0.75/h)*(1-(u/h)**2 )*(abs(u/h)<=1)
}





