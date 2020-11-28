library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(tidytable)
library(scales)
theme_set(theme_classic(base_size=11, base_family =''))

#---------------------part 1 
# first, plot the aggregate the variable under different equilibria.
eqm_list = c('init','base',
           'base_mortality','base_popgrowth',
           'replace','tau','gama')

var_list = c('p','r','wage','ownership','what','price-income ratio')
get_a = function (eqm_name){
  filename = paste ('aggregate', eqm_name, sep = '_')%>%
    paste (., 'xlsx', sep = '.')
    data = read_excel(filename, col_names = var_list)
}
# the following code seems interesting.
data_aggregate = map (eqm_list, ~get_a(.))%>%
  reduce(rbind)%>%
  cbind(eqm_list)%>%rename(eqm = eqm_list)

# now start to plot the geom
get_plot_aggregate = function (var_name){
y.var <- rlang::sym(var_name)
plot = data_aggregate %>%
   ggplot (mapping = aes (x = reorder(eqm, !! y.var) ,
                          y =!! y.var, fill = eqm))+
   labs(y = var_name,x='Eqm', 
        title = paste (var_name, 'plot',sep = ' '))+
   geom_col()+coord_flip()+
   theme(plot.title = element_text(hjust = 0.5))
  filename = paste(var_name, 'png',sep ='.')
  ggsave (file = filename, plot = plot,
         dpi = 'retina', width = 6.4, height = 4.8)
  return (plot) }


plot_list = map (var_list, ~get_plot_aggregate(.) )


#---------------part 2: the life cycle 
# plot the average lice cycle profile for different equilibrium. 
get_life = function (eqm_name){
  filename = paste ('life_cycle',eqm_name,sep = '_')%>%
    paste (., 'xlsx',sep = '.')%>%
    toString(.)
  data = read_excel(filename, col_names = 
               c ('cons', 'asset','house','income','ownership')) %>%
  mutate (eqm = eqm_name)%>%
  mutate (age = 20+ (row_number()-1)*5)
  return (data)
}

plot_average = function (eqm_input, var_name){
y.var <- rlang::sym(var_name)
plot = map (eqm_input, ~get_life(.))%>%
  reduce(rbind)%>%
  ggplot(mapping = aes(x= age,
         y = !! y.var, color = eqm)) + 
  geom_line(size = 1)+
  labs(y = var_name,x='Age', title = paste ('life_cycle',var_name,sep = ' '))+
  theme(plot.title = element_text(hjust = 0.5))
  filename = str_c (var_name,reduce(eqm_input, function(a,b) paste(a,b, sep = '&')))%>%
    str_c(., '.png')
        ggsave (file = filename, plot = plot,
                dpi = 'retina', width = 8.0, height = 4.8)
        return (plot)
}

plot_average (c('init', 'base','base_mortality'), 'house')
plot_average (c('init', 'base','base_popgrowth'), 'house')
plot_average (c('init', 'base','replace'), 'house')

# ----------part 3---------
# plot the change of housing wealth for each age and each producitivty level. 

gridh = read_excel('gridh.xlsx',col_names = 
                     c('h', 'h_value'))

get_h_data = function(eqm_name){
filename = paste ('measure',eqm_name,sep = '_')%>%
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
  summarize (avg_h = mean(average_h))%>%
  ungroup()%>%
  rename_with(~paste(eqm_name, 'h',sep ='_' ), .cols = c(avg_h))
  return (data)
}

# this is really interesting...
df_house_split = map (eqm_list, ~get_h_data(.))%>%
  reduce(., function(a,b) inner_join(a,b,by = c('e','age')))

# each new equilibrium, we want to plot the the change of 
plot_h_change = function (eqm_name){
y.var <- rlang::sym(paste(eqm_name,'h',sep = '_'))
plot = df_house_split %>%
  filter(age>=25)%>%
  ggplot(mapping = aes (x = age, 
  y = (!! y.var - init_h)/init_h  , fill = age))+
  geom_col()+ facet_wrap(~ e ,scales='free_x') +
  labs(y = 'perchange change ',x='Age')+
  theme(axis.line=element_line())+
  theme(strip.background =element_rect(fill="springgreen3"))+
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5))
filename = paste ('hchange',eqm_name, sep = '_')%>%
  paste(., '.png', sep = '')
ggsave (file = filename, plot = plot,
        dpi = 'retina', width = 9.6, height = 4.8)
return (plot)
}

plot_list_3 = map (eqm_list, ~plot_h_change(.))

