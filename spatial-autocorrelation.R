source('header.R')
library(lctools)

set_sub('models')

mod <- load_object('modsimp.full')

set_sub('models')

dat <- load_data('datamod') 
xy <- data.frame(st_coordinates(dat)) %>% group_by(L1) %>%
  summarise(X = mean(X),
            Y= mean(Y)) 

dat %<>% as_tibble()
dat %<>% cbind(xy)

# get mean residuals
resid <- lapply(mod, residuals) %>% 
  ldply() %>% 
  t() %>% 
  as_tibble() %>%
  mutate(MeanResidual = rowMeans(.))

dat %<>% mutate(MeanResidual = resid$MeanResidual)

morans <- moransI(Coords = cbind(dat$X, dat$Y),
                  x = dat$MeanResidual,
                  Bandwidth = 6)

t(as.matrix(morans[2:7]))
