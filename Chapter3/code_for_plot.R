library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(tidytable)
library(scales)
library(rlang)
theme_set(theme_classic(base_size=11, base_family =''))

#---------------------part 1 -----------
# first, plot the aggregate the variable under different equilibria.
scenario_name = c('mortality_only', 'popgrowth_only','mortality_popgrowth')
policy_name = c('adjust_replacement', 'adjust_tax','pension_reform')

 eqm_list = expand.grid (scenario_name, policy_name) %>%
   mutate(eqm_name = paste0(Var1,'_',Var2)) %>%
   select (eqm_name) %>%unlist(.)

a = get_a(eqm_list[1])
var_list = c('Housing Price','Interest Rate','Wage',
  'Ownership','House Stock','Construction','kl_ratio', 'Land Profit','Transfer')
get_a = function (eqm){
     filename = paste0('./',eqm, '_aggregate.txt')  
     read_table(filename, col_names = var_list) %>%
     mutate(eqm = eqm)
}
# the following code seems interesting.
data_aggregate = map (eqm_list, ~get_a(.))%>%
  reduce(rbind)%>%
  rbind( get_a('initial')) %>%
  mutate (across(where(is.numeric),   ~(.-nth(.,n()))/nth(.,n())  ) )%>%
  slice(1:n()-1)
  # This is very usefully. standardize each row by the value in the first row
  
# now start to plot the geom
get_plot_aggregate = function (var_name){

plot = data_aggregate %>%
   ggplot (mapping = aes (x = reorder(eqm, !! sym(var_name)) ,
                          y =!! sym(var_name), fill = eqm))+
   labs(y =str_c('Percent Change in ', var_name))+
   geom_col()+coord_flip()+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank())
  filename = paste(var_name, 'png',sep ='.')
  ggsave (file = filename, plot = plot,
         dpi = 'retina', width = 6.4, height = 4.8)
  return (plot) }

get_plot_aggregate('Housing Price')

plot_aggregate = ggarrange (plotlist = map (var_list ,
                            ~get_plot_aggregate(.) ) , ncol = 3, nrow = 3)
ggsave (file = 'aggregate.png', plot = plot_aggregate,
        dpi = 'retina', width = 10.0, height = 6.0)

#---------------part 2: the life cycle 
# plot the average lice cycle profile for different equilibrium. 

a = get_life(eqm_list[1])

get_life = function (eqm){

  age = map(seq(21, 90, by = 5), ~rep(.,5))%>%unlist(.)
  
  paste0 ('./',eqm,sep = '_life.txt')%>%
  read_table(., col_names = 
               c ('Cons', 'Asset','House','Ownership')) %>%
  mutate (eqm = eqm)%>%
  cbind ( age) %>%
  group_by (age)%>%
  summarize (across(-eqm, ~mean(.))) 
  
}

plot_average = function (eqm_list, var_name){

plot = map (eqm_list, ~get_life(.))%>%
  reduce(rbind)%>%
  ggplot(mapping = aes(x= age,
         y = !!sym(var_name), color = eqm)) + 
  geom_line(size = 1)+
  labs(y = var_name,x='Age')+
  theme(legend.position = 'bottom')
  filename = paste0(var_name,
                    reduce(eqm_list, function(a,b) paste(a,b, sep = '&')),
                    '.png')
  
        ggsave (file = filename, plot = plot,
                dpi = 'retina', width = 6.4, height = 4.8)
        return (plot)
}


plot_life_house = ggarrange(plotlist = map (setdiff(eqm_list, c('initial')),~plot_average(
  c('initial',.), 'Income'
) ), ncol = 2, nrow =3)

ggsave (file = 'life_Income.png', plot = plot_life_house,
        dpi = 'retina', width = 8.0, height = 6.0)


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


