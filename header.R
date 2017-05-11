library(poiscran)
library(dismo)
library(Hmisc)
library(RColorBrewer)
library(haidawave)
library(gbm)
library(gridExtra)

rm(list = ls())
graphics.off()

source('functions.R')

bcalbers <- 3005
wgs84 <- 4326
canalb <- 102001
  
  if (getDoParWorkers() == 1) {
    message("registering 4 workers")
    registerDoParallel(4)
    options(mb.parallel = TRUE)
  }

options(mb.quick = FALSE)

subfoldr::reset_all()

options(subfoldr.ask = FALSE)

theme_set(theme_Poisson())