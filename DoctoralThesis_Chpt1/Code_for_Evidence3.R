

# plot for the r&d and gdp evidence.(Evidence 3)
df_gdp = read.xlsx('./gdp.xlsx')%>%
  pivot_longer(-c('Country.Name','Country.Code' ),
               names_to = 'year', values_to = 'gdp')%>%
  select (-c('Country.Name'))

df_rnd = read.xlsx('./rdratio.xlsx')%>%
  pivot_longer(-c('Country.Name','Country.Code' ), 
               names_to = 'year', values_to = 'rnd')%>%
  select (-c('Country.Name'))

plot_res = inner_join (df_gdp , df_rnd, by = c('year', 'Country.Code'))%>%
  na.omit(.)%>%
  mutate (gdp = log(gdp), rnd = rnd/100) %>%
  ggplot(aes(x=gdp, y=rnd)) + 
  geom_point(color = 'grey')+
  #geom_smooth(method = lm)
  stat_smooth(method = "lm", formula = y ~ exp(x), size = 1) +
  labs (x = 'log(GDP) per Capita',
        y = 'R&D Expenditure-GDP Ratio') +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text = element_text (size = 20) ,
        axis.title = element_text (size = 30, hjust =0.5))

ggsave('rd_gdp.jpg', plot = plot_res, 
       height = 9, width = 16, dpi = 'retina')  

