library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
# what you need to do? 
# first, plot the average house life cycle between init and base 
# See the difference. And decompose it into base_mortality, and base_growth
# discuss the difference, but do not touch the the distribution issue

# second, plot the average CHANGE of house for different income group under
# base, replace, tau, and gama. 
# discuss the potential difference in the housing asset change

theme_set(theme_classic(base_size=11, base_family =''))

gridh = read_excel('gridh.xlsx',col_names = 
                     c('h', 'h_value'))

s_list = c('init','base',
           'base_mortality','base_popgrowth',
           'replace','tau','gama')

get_life = function (name){
  filename = paste ('life_cycle',name,sep = '_')%>%
    paste (., 'xlsx',sep = '.')%>%
    toString(.)
  data = read_excel(filename, col_names = 
               c ('cons', 'asset','house','income','ownership')) %>%
  mutate (situation = name)%>%
  mutate (age = 20+ (row_number()-1)*5)
  return (data)
}

df_life = map (s_list, ~get_life(.))%>%
  reduce(rbind)

get_h_data = function(name){
filename = paste ('measure',name,sep = '_')%>%
  paste (., 'xlsx',sep = '.')%>%
  toString(.)
data = read_excel(filename,col_names = 
                    c('e','h', 'b', 'j', 'm'))%>%
  filter(m>0)%>%
  left_join(gridh, by = 'h')%>%
  group_by (e,j)%>%
  summarize(average_h = h_value%*% m /sum (m))%>%
  mutate (age = 20 +floor((j-21)/5)*5  ) %>%
  ungroup()%>%
  group_by (e,age) %>%
  summarize (average_h = mean(average_h))%>%
  mutate (situation = name)%>%
  ungroup()
  return (data)
}

df_house_split = map (s_list, ~get_h_data(.))%>%
  reduce(rbind)


#--------------Now we get a plot on start to plot
plot_single = function (eqm, e_input){
#temp = reduce(eqm, function (x,y) paste(x,y, sep = ' & '))
plot_title = c('Average House when e = ', e_input
               )%>%
  reduce(paste)
p = df_house_split%>%
  filter (e == e_input & situation %in% eqm) %>%
  ggplot(mapping = aes(x= age,
        y = average_h, color = situation)) + 
  geom_line(size = 1)+
  labs(y = "Average house",x='Age', title =plot_title)
  #filename = c('h',e_input, temp,'.png')%>%reduce(paste)
  #ggsave(file = filename, plot = p,
      # dpi = 'retina', width = 8.0, height = 4.8)
  return (p)
  
}

plot_aggregate = function (eqm){
  p_list = map (c (1,2,3,4,5), ~plot_single(eqm, .))
  he = ggarrange(plotlist = p_list,ncol = 2,nrow = 3)
  temp = reduce (eqm, function(x,y) paste(x,y, sep='&'))
  filename = paste(temp,'.png', sep = '')
  ggsave(file = filename, plot = he,
         dpi = 'retina', width = 8, height = 8)
  
}

plot_aggregate (c('init','base','base_popgrowth'))

