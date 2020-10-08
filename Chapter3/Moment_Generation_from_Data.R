library(readstata13)
library(dplyr) # it allows you to do generation work like in stata.
library(tidyr)
library(purrr) # it contains the map function
library(broom) # this is for you to use broom
library(ggplot2) 
library(sampleSelection)
library("readxl")



df1999=read.dta13("UHIP1999ind.dta",nonint.factors=TRUE,generate.factors=T)

df1999=df1999%>%
  mutate(hhcode=a100a)%>%
  #mutate(income=a202+a208+a211+a211+a212+a213+a214+a211+a212+a213++a231-a213)%>%
  mutate(income=a201)%>%
  mutate(wage=ifelse(is.na(a202),0,a202))%>%
  mutate(wagetransfer=wage+a221)%>%
  mutate(hpfhead=ifelse(a239>0,1,0))%>%
  filter(a104==1 | a104==3)%>%
  mutate(hhhead=a103)%>%
  mutate(age=a106)%>%
  mutate(gender=ifelse(a105==1,1,ifelse(a105==2,0,NA)))%>%
  mutate(retire=ifelse(a107==2 | a107==3 | a107==6,1,0)) %>%
  mutate(minzoku=ifelse(a108==1,1,ifelse(a108==2,0,NA)))%>%
  mutate(marriage=ifelse(a109==1,1,ifelse(a109!=1 & !is.na(a109),0,NA)))%>%
  mutate(party=ifelse(a110==1,1,ifelse(a110==2,0,NA)))%>%
  mutate(zhiqing=ifelse(a124==1,1,ifelse(a124==2,0,NA)))%>%
  mutate(edu=a111)%>%
  mutate(eduyear=a112)  # here I mainly mutate some of the variables


df1999_hhcode=df1999%>%
  group_by(hhcode)%>%
  summarize( hhsize = n(), totalincome = sum(income),totalwage=sum(wage),totalwt=sum(wagetransfer))%>%ungroup() # I create a dataframe that sum the totalincome and count the population within each hh.

df1999=df1999%>%filter(hhhead==1)%>%
  select (c(hhcode,age,retire,gender,minzoku,marriage,party,zhiqing,edu,eduyear,hpfhead)) # I only want these columns

df1999ind=inner_join(df1999,df1999_hhcode,by="hhcode") # I combine two data set with key variable hhcode.

df1999hh=read.dta13("UHIP1999hh.dta",nonint.factors=TRUE,generate.factors=T)

df1999hh=df1999hh%>%
  # in 1999 data, a609 have value from 1 to 6. 5 is rent. 6 is others. 
  mutate(hhcode=a100a)%>%
  mutate(finasset=a401-a409)%>%
  #mutate(finasset_new=a401-a409-a411)%>%
  mutate(liquid=finasset-(a416-a417))%>%
  mutate(house=a414)%>%
  filter(!is.na(house))%>%
  mutate(rent=a519)%>%
  mutate(ownership=ifelse(house>0,1,0))%>%
  mutate(mortgage=a417)%>%
  mutate(area=a601)%>%
  mutate(hpf=a409)%>%
  mutate(cons=a501+a506+a507+a510+a511+a520+a521)%>%
  mutate(networth=liquid+house-mortgage)%>%
  mutate(purchase=a523)


df1999hh=df1999hh%>%
  select (c(hhcode,finasset,house,liquid,mortgage,area,hpf,ownership,cons,purchase,rent,province,networth)) # I only want these columns

df1999final=inner_join(df1999ind,df1999hh,by="hhcode")

df1999final=df1999final%>%
  select(c(hhcode,province,age,retire,edu,eduyear,gender,hhsize,minzoku,marriage,totalwage,cons,networth,finasset,liquid,mortgage,house,ownership))%>%
  mutate(datayear=1999)

############################1995
df1995=read.dta13("1995ind.dta",nonint.factors=TRUE,generate.factors=T)
df1995=mutate(df1995,hhcode=N1)
# before calculate each inds income you need to turn the NA value to 0
df1995=df1995%>%replace_na(list(A87=0,A52 = 0, A62=0,A64=0,A65=0,A66=0,A67=0,A68=0,A69=0,A76=0,A85=0,A86=0,A87=0,A88=0))
# now you can calculat the income
df1995=df1995%>%
  mutate(income=A52+A62+A64+A65+A66+A67+A68+A69+A76+A85)%>%
  mutate(wage=ifelse(is.na(A52),0,A52))%>%
  mutate(wagetransfer=A52+A76)%>%
  mutate(hpfhead=ifelse(A87>0,1,0))

df1995=df1995%>%
  mutate(hhhead=A3)%>%
  mutate(age=A5)%>%
  mutate(gender=ifelse(A4=="male",1,ifelse(A4=="female",0,NA)))%>%
  mutate(minzoku=ifelse(A8=="yes",1,ifelse(A8=="no",0,NA)))%>%
  mutate(marriage=ifelse(A7=="married",1,ifelse(is.na(A7),NA,0)))%>%
  mutate(party=ifelse(A9=="yes",1,ifelse(A9=="no",0,NA)))%>%
  mutate(zhiqing=ifelse(A19=="yes",1,ifelse(A19=="no",0,NA)))%>%
  mutate(retire=ifelse(A6=="retire",1,0))%>%
  mutate(edu=A11)%>%
  mutate(hpf=A87)%>%
  mutate(eduyear=A12)  # here I mainly mutate some of the variables


df1995_hhcode=df1995%>%
  group_by(hhcode)%>%
  summarize( hhsize = n(), totalincome = sum(income),totalwage=sum(wage),totalwt=sum(wagetransfer))%>%ungroup() # I create a dataframe that sum the totalincome and count the population within each hh.

df1995=df1995%>%filter(hhhead=="self")%>%
  select (c(hhcode,age,retire,gender,minzoku,marriage,party,zhiqing,edu,eduyear,hpf,hpfhead)) # I only want these columns

df1995ind=inner_join(df1995,df1995_hhcode,by="hhcode") # I combine two data set with key variable hhcode.



df1995hh=read.dta13("1995hh.dta",nonint.factors=TRUE,generate.factors=T)
df1995hh=df1995hh%>%
  mutate(hhcode=N1)%>%
  mutate(finasset=H1)%>%
  mutate(house=H11)%>%
  filter(!is.na(house))%>%
  mutate(liquid=finasset-H13+H14)%>%
  mutate(mortgage=H14)%>%
  mutate(area=H56)%>%
  mutate(rent=H64*12)%>%
  mutate(ownership=ifelse(house>0,1,0))%>%
  mutate(province=PROVINCE)%>%
  mutate(cons=H27+H30+H32+H33+H34+H36+H37+H38+H41+H44+H45+H46+H47)%>%
  mutate(networth=liquid-mortgage+house)%>%
  mutate(purchase=H65)

df1995hh=df1995hh%>%
  select (c(hhcode,finasset,house,liquid,mortgage,area,ownership,cons,purchase,rent,province,networth)) # I only want these columns

df1995final=inner_join(df1995ind,df1995hh,by="hhcode")

df1995final=df1995final%>%
  select(c(hhcode,province,age,retire,edu,eduyear,gender,hhsize,minzoku,marriage,totalwage,cons,networth,finasset,liquid,mortgage,house,ownership))%>%
  mutate(datayear=1995)

df1995final=na.omit(df1995final)
###################2002

df2002_4=read.dta13("2002_4.dta",nonint.factors=TRUE,generate.factors=T)
df2002_4=df2002_4%>%
  mutate(hhcode=PCODE)%>%
  mutate(cons=E1+F2+F3+F5+F6-F6324+F72+F73+F8)%>%
  mutate(rent=F711)%>%
  mutate(purchase=D952)%>%
  mutate(area=B23)%>%
  select(c(hhcode,cons,purchase,rent,area))

df2002_3=read.dta13("2002_3.dta",nonint.factors=TRUE,generate.factors=T)
df2002_3=df2002_3%>%
  mutate(hhcode=PCODE)%>%
  mutate(income=A15)%>%  #there is no NA value here. so I do not have to....
  mutate(wage=ifelse(is.na(A151),0,A151))%>%
  mutate(wagetransfer=A151+A154)%>%
  mutate(pension=ifelse(is.na(A181),0,A181))%>%
  mutate(hpfhead=ifelse(A182>0,1,0))

df2002_hhcode=df2002_3%>%
  group_by(hhcode)%>%
  summarize( hhsize = n(), totalincome = sum(income),hpf=sum(A182),totalwage=sum(wage),totalwt=sum(wagetransfer),pension=ifelse(sum(pension)==0,0,1))

df2002_1=read.dta13("2002_1.dta",nonint.factors=TRUE,generate.factors=T)
df2002_1=df2002_1%>%
  mutate(hhcode=PCODE)%>%
  mutate(hhhead=P103)%>%
  mutate(age=P106)%>%
  mutate(retire=ifelse(P107=='Retired'| P107=='Early retirement' | P107=='Internal retirement',1,0))%>%
  mutate(gender=ifelse(P105=="male",1,ifelse(P105=="female",0,NA)))%>%
  mutate(minzoku=ifelse(P108=="Minority",1,ifelse(P108=="Han",0,NA)))%>%
  mutate(marriage=ifelse(P109=="With spouse",1,ifelse(is.na(P109),NA,0)))%>%
  mutate(party=ifelse(P110=="The Communist Party",1,ifelse(is.na(P110),NA,0)))%>%
  mutate(zhiqing=ifelse(P127=="Yes",1,ifelse(P124=="No",0,NA)))%>%
  mutate(edu=P112)%>%
  mutate(eduyear=P113)%>%
  mutate(province=PROVINCE)%>%
  filter(hhhead=="Self")%>%
  select (c(hhcode,age,retire,gender,minzoku,marriage,party,zhiqing,edu,eduyear,province)) # I only want these columns


df2002_2=read.dta13("2002_2.dta",nonint.factors=TRUE,generate.factors=T)
df2002_2=df2002_2%>%
  mutate(hhcode=PCODE)%>%
  mutate(mortgage=H417)%>%
  mutate(finasset=H401)%>% #this computation considers the stock.while in your pension model, there is no stock.
  mutate(liquid=ifelse(!is.na(H402),H402,0)+ifelse(!is.na(H403),H403,0))%>%
  mutate(house=H414)%>%
  filter(!is.na(house))%>%
  mutate(networth=finasset-mortgage+house)%>%
  select (c(hhcode,finasset,liquid,mortgage,house,networth)) 

df2002final=inner_join(df2002_1,df2002_2,by="hhcode")
df2002final=inner_join(df2002final,df2002_4,by="hhcode")
# I combine two data set with key variable hhcode.
df2002final=inner_join(df2002final,df2002_hhcode,by="hhcode")
df2002final=df2002final%>%
  mutate(ownership=ifelse(house>0 ,1,0))


df2002final=df2002final%>%
  select(c(hhcode,province,age,retire,edu,eduyear,gender,hhsize,minzoku,marriage,totalwage,cons,networth,finasset,liquid,mortgage,house,ownership,pension))%>%
  mutate(datayear=2002)


#######################################

df2009_1=read.dta13("UHS_w2_ac.dta",nonint.factors=TRUE,generate.factors=T)
# this dataset has the education, age, and income data

df2009_1_new=df2009_1%>%
  #mutate(hhcode=code)%>% # i dont need this command. there is already hhcode in the orginal dataset
  mutate(wage=ifelse(is.na(c18),0,c18))%>%   # we treat the null value of income as zero.
  mutate(province=substr(code,1,2))%>% # the code variable already contains the information o thet
  mutate(wagetransfer=wage+c20)%>% # c20 is the other benefit
  mutate(pension=ifelse(c10=='None' | is.na(c10),0,1))%>%
  #mutate(hpfhead=ifelse(c12==1 |c12==2 |c12==3 ,1,0))%>%
  mutate(hhhead=person)%>%  # according to the dataset, person who 'person=1' is hhhead
  mutate(age=2009-floor(a06/10000))%>%
  mutate(gender=ifelse(a05=='Male',1,ifelse(a05=='Female',0,NA)))%>%
  mutate(retire=ifelse(a29=='Retired or resigned' ,1,0)) %>% # the forth choice is 'retire'
  mutate(minzoku=ifelse(a18=='Han',1,0))%>%  # if you are han, then 1
  mutate(marriage=ifelse(a16=='Married' | a16=='Single (Skip to A18)',1,0))%>%  # 1 is first marriege, 2 is second marriage.
  mutate(edu=a21)%>%
  mutate(eduyear=a22)  # here I mainly mutate some of the variables

df2009_hh=df2009_1_new%>%
  group_by(hhcode)%>%
  summarize( hhsize = n(), totalwage=sum(wage),pension=ifelse(sum(pension)==0,0,1))
   # I create a dataframe that sum the totalincome and count the population within each hh.

df2009_1_new=df2009_1_new%>%filter(hhhead==1)%>%
  select (c(hhcode,province,age,retire,gender,minzoku,marriage,edu,eduyear)) # I only want these columns


df2009_2=read.dta13("UHS_w2_jk.dta",nonint.factors=TRUE,generate.factors=T)
# this data set is actually a household level data.
# this dataset has the housing value data
df2009_2_new=df2009_2%>%
  # in 1999 data, a609 have value from 1 to 6. 5 is rent. 6 is others. 
  mutate(hhcode=code)%>%
  mutate(finasset=ifelse(is.na(k03),0,k03))%>% # we only consider the deposit
  #mutate(finasset_new=a401-a409-a411)%>%
  mutate(liquid=ifelse(is.na(k04),0,k04))%>%
  mutate(house=ifelse(is.na(k02),0,k02))%>%
  mutate(ownership=ifelse(house>0,1,0))%>%
  #filter(!is.na(ownership))%>%
  mutate(mortgage=ifelse(is.na(k13),0,k13))%>%
  mutate(networth=finasset+house-mortgage)%>%
  #mutate(area=a601)%>%
  #mutate(hpf=a409)%>%
  mutate(cons=j01)
  #mutate(purchase=a523)


df2009final=inner_join(df2009_hh,df2009_2_new,by="hhcode") 
df2009final=inner_join(df2009final,df2009_1_new,by="hhcode")
df2009final=df2009final%>%
  select(c(hhcode,province,age,retire,edu,eduyear,gender,hhsize,minzoku,marriage,totalwage,cons,networth,finasset,liquid,mortgage,house,ownership,pension))%>%
  mutate(datayear=2009)

dfreg=rbind(df2002final,df2009final)

dfreg=dfreg%>%
  mutate(provinceyear=paste(datayear,province))
pricedata=pricedata%>%
  mutate(provinceyear=paste(year,province))
dfregfinal=merge(dfreg,pricedata,by='provinceyear')

averageprice=mean(dfregfinal$price)
dfregfinal=dfregfinal%>%
  mutate(house_pure=house/price)%>%
  mutate(networth_pure=house_pure*averageprice+finasset-mortgage)

#-----------------------------------------------------
# we next calculate the house hsare and wealth share. every year.

dfregfinal=dfregfinal[order(dfregfinal$networth_pure),]

wsharebot50= vector(mode = "numeric",70)
wsharemid40= vector(mode = "numeric",70)
wsharetop20= vector(mode = "numeric",70)
wsharetop10= vector(mode = "numeric",70)
wsharetop1= vector(mode = "numeric",70)
for (j in 1:70)
{
  a=dfregfinal$networth_pure[dfregfinal$age==j+20 ]  # notice that a is sorted. 
  #if (j==7) {a=dfregfinal$networth_pure[ dfregfinal$age>=21+(j-1)*10 ]}
  na=sum(dfregfinal$age==j)
  #if (j==7) {na=sum(dfregfinal$age>=21+(j-1)*10)}
  wsharebot50[j]=sum(a[1:floor(na*0.5)])/sum(a)
  wsharemid40[j]=sum(a[floor(na*0.3):floor(na*0.7)])/sum(a)
  wsharetop20[j]=sum(a[floor(na*0.8):na])/sum(a)
  wsharetop10[j]=sum(a[floor(na*0.9):na])/sum(a)
  wsharetop1[j]=sum(a[floor(na*0.99):na])/sum(a)
}
plot(21:90, wsharetop10,type='l')






#------------------------------------
# we next calculate the house share and wealth share.  every 5 years.

dfregfinal=dfregfinal[order(dfregfinal$networth_pure),]

wsharebot50= vector(mode = "numeric",7)
wsharemid40= vector(mode = "numeric",7)
wsharetop20= vector(mode = "numeric",7)
wsharetop10= vector(mode = "numeric",7)
wsharetop1= vector(mode = "numeric",7)
for (j in 1:7)
{
a=dfregfinal$networth_pure[dfregfinal$age<20+j*10 & dfregfinal$age>=21+(j-1)*10 ]  # notice that a is sorted. 
#if (j==7) {a=dfregfinal$networth_pure[ dfregfinal$age>=21+(j-1)*10 ]}
na=sum(dfregfinal$age<20+j*10 & dfregfinal$age>=21+(j-1)*10)
#if (j==7) {na=sum(dfregfinal$age>=21+(j-1)*10)}
wsharebot50[j]=sum(a[1:floor(na*0.5)])/sum(a)
wsharemid40[j]=sum(a[floor(na*0.3):floor(na*0.7)])/sum(a)
wsharetop20[j]=sum(a[floor(na*0.8):na])/sum(a)
wsharetop10[j]=sum(a[floor(na*0.9):na])/sum(a)
wsharetop1[j]=sum(a[floor(na*0.99):na])/sum(a)
}
# this is for house. 
dfregfinal=dfregfinal[order(dfregfinal$house_pure),]
hsharebot20= vector(mode = "numeric",7)
hsharemid20= vector(mode = "numeric",7)
hsharetop20= vector(mode = "numeric",7)
for (j in 1:7)
{
  a=dfregfinal$house_pure[dfregfinal$age<20+j*10 & dfregfinal$age>=21+(j-1)*10 ]  # notice that a is sorted. 
  if (j==7) {a=dfregfinal$house_pure[ dfregfinal$age>=21+(j-1)*10 ]}
  na=sum(dfregfinal$age<20+j*10 & dfregfinal$age>=21+(j-1)*10)
  if (j==7) {na=sum(dfregfinal$age>=21+(j-1)*10)}
  hsharebot20[j]=sum(a[1:floor(na*0.2)])/sum(a)
  hsharemid20[j]=sum(a[floor(na*0.4):floor(na*0.6)])/sum(a)
  hsharetop20[j]=sum(a[floor(na*0.8):na])/sum(a)
}
plot(1:7, wsharetop10,type='l')

#----------------------------------------------------
#plot the house density for those who has positive house
dfregfinal_housepositive=dfregfinal%>%
  filter(house>0)

dfregfinal_housepositive=dfregfinal_housepositive%>%
  mutate(house_standard=house/10000)%>%
  filter(house_standard<200)

ggplot(dfregfinal_housepositive,aes(x=dfregfinal_housepositive$house_standard))+geom_density()

#----------------------------------------------------------



df1995final=df1995final%>%
  mutate(age2=age^2)%>%
  filter(totalwage>0)


reg_w=lm(log(totalwage)~ age+age2+eduyear, data = df1995final )
df1995final=df1995final%>%
  mutate(resid_w=residuals(reg_w))
a=df1995final$resid_w
a=unique(a) # now the array a contains all the values without any repetition.
n=sum(df1995final$age<22)
w5=as.numeric(quantile(a,0.8)) # find the 75 quantile in array a. corresponds to e5 in my simulation code
p5=sum(df1995final$resid_w>w5 & df1995final$age<22)/n
w4=as.numeric(quantile(a,0.6))# find the 50-75 quantile in array a. corresponds to e5 in my simulation code
p4=sum(df1995final$resid_w>w4 & df1995final$resid_w<w5 & df1995final$age<22)/n
w3=as.numeric(quantile(a,0.4))  # find the 75 quantile in array a. corresponds to e5 in my simulation code
p3=sum(df1995final$resid_w>w3 & df1995final$resid_w<w4 & df1995final$age<22)/n
w2=as.numeric(quantile(a,0.2)) # find the 75 quantile in array a. corresponds to e5 in my simulation code
p2=sum(df1995final$resid_w>w2 & df1995final$resid_w<w3 & df1995final$age<22)/n
p1=sum(df1995final$resid_w<w2 & df1995final$age<22)/n

