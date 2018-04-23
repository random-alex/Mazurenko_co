
# prereq ------------------------------------------------------------------

dir <- 'data/j_1'


# read file ---------------------------------------------------------------

df <- tibble(dirs = list.files(dir,pattern = 'task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(data = map(dirs,my_read_xml_aveg,param = c('J','L','T','LATTICE'))) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate_at(vars(value,J,L,temp,error),funs(as.numeric))

df_corr <- tibble(dirs = list.files(dir,pattern = 'task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(data = map(dirs,my_read_xml_corr)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value))


# average param plot ------------------------------------------------------

df %>% 
  ggplot(aes(temp,value,col = as.factor(LATTICE))) +
  geom_line() +
  geom_pointrange(aes(ymin = value - error,ymax = value + error)) +
  facet_wrap(c('parameter'),scales = 'free') + 
  scale_color_discrete(name = 'Тип решетки', labels = c('Нестандартная','Стандартная')) +
  xlab('Температура, T/J') + 
  ylab('Значения измеряемого параметра') +
  theme_bw()
