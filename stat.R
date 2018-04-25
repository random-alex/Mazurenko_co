require(GA)

# prereq ------------------------------------------------------------------

dir <- 'data/j_1'


# read file ---------------------------------------------------------------

df <- tibble(dirs = list.files(dir,pattern = 'task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)[-c(12:16)]) %>% 
  mutate(data = map(dirs,my_read_xml_aveg,param = c('J','L','T','LATTICE'))) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate_at(vars(value,J,L,temp,error),funs(as.numeric))

df_corr <- tibble(dirs = list.files(dir,pattern = 'task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(data = map(dirs,my_read_xml_corr,param = c('J','L','T','LATTICE'))) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate_at(vars(value,J,L,temp,error),funs(as.numeric))


# average param plot ------------------------------------------------------
unique(df$parameter)

nes_par <- c('Staggered Magnetization Density^2')
nes_par <- c('Staggered Magnetization^2')
df %>% 
  filter(parameter %in% nes_par) %>%
  ggplot(aes(temp,value,col = as.factor(LATTICE))) +
  geom_line() +
  geom_pointrange(aes(ymin = value - error,ymax = value + error)) +
  facet_wrap(c('parameter'),scales = 'free',ncol = 3) + 
  scale_color_discrete(name = 'Тип решетки', labels = c('Нестандартная','Стандартная')) +
  xlab('Температура, T/J') + 
  ylab('Квадрат плотности намагниченности подрешетки') +
  theme_bw()  +
  theme(legend.position="bottom")  



# cor plot ----------------------------------------------------------------

df_corr  %>% 
  filter(coordinats == '0__1') %>% 
  mutate(value = -value) %>% 
  ggplot(aes(temp, value ,col = as.factor(LATTICE))) +
  geom_line() +
  geom_pointrange(aes(ymin = value - error,ymax = value + error)) +
  facet_wrap(c('parameter'),scales = 'free',ncol = 3) + 
  scale_color_discrete(name = 'Тип решетки', labels = c('Нестандартная','Стандартная')) +
  xlab('Температура, T/J') + 
  ylab('Значения измеряемого параметра') +
  theme_bw()  +
  theme(legend.position="bottom")  


nes_coor <- c(50:54)
x_coor <- 49
df_corr  %>% 
  # filter(temp == 0.4375) %>% 
  separate('coordinats',c('x','y'),sep = '__') %>% 
  mutate_at(vars(x,y),funs(as.numeric)) %>% 
  filter(x == x_coor & y %in% nes_coor) %>% 
  mutate(value = abs(value)) %>% 
  ggplot(aes(y, value ,col = as.factor(LATTICE))) +
  geom_line() +
  geom_pointrange(aes(ymin = value - error,ymax = value + error)) +
  facet_wrap(c('temp'),ncol = 3) + 
  # scale_color_discrete(name = 'Тип решетки', labels = c('Нестандартная','Стандартная')) +
  # xlab('Температура, T/J') +
  scale_y_log10() +
  ylab('Значения измеряемого параметра') +
  theme_bw()  +
  theme(legend.position="bottom")  
  

# calculating corr length -------------------------------------------------

my_corr_len_finder <- function(data,temp, x_coor_foo = x_coor ,nes_coor_foo = nes_coor) {
  df_param <- data %>% 
    separate('coordinats',c('x','y'),sep = '__') %>% 
    mutate_at(vars(x,y),funs(as.numeric)) %>% 
    filter(x == x_coor_foo & y %in% nes_coor_foo) %>% 
    mutate(value = log(abs(value)))
  mod <- lm(value ~ y,df_param )
 df_pred <- df_param %>% 
   modelr::add_predictions(mod) %>% 
   select(y,value,pred)
  res <- tibble(cor_length = - 1 /mod$coefficient[2], coef = mod$coefficients[2]) %>% cbind( df_pred)
  return(res)
}

df_corr_len <- df_corr %>% 
  nest(-temp) %>% 
  mutate(data_len = map2(data,temp,my_corr_len_finder)) %>% 
  unnest(data_len,.drop = T) 

df_corr_len %>% 
  gather(type,value,c(value,pred)) %>% 
  ggplot(aes(y,value,col = type)) +
  geom_point() +
  geom_line() + 
  scale_color_discrete(name = 'Тип значений', labels = c('Модельные', 'Исходные')) +
  facet_wrap(c('temp'),labeller = 'label_both',ncol = 2) +
  ylab('Значения') +
  xlab('Номер ячейки') +
  theme_bw() +
  theme(legend.position = 'bottom')

df_corr_len %>% 
  select(temp,cor_length,coef) %>% 
  .[!duplicated.array(.),] %>% 
  gather(parameter,value,-1) %>% 
  ggplot(aes(temp,value)) +
  geom_line() +
  geom_point() +
  facet_grid(parameter ~.,scales = 'free') +
  theme_bw()

df_corr_len %>% 
  select(temp,cor_length,coef) %>% 
  .[!duplicated.array(.),] %>% 
  ggplot(aes(temp,cor_length)) +
  geom_line() +
  geom_point() +
  ylab('Корреляционная длина') +
  xlab('Относительная температура, T/J') +
  theme_bw()






