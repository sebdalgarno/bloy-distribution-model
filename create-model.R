source('header.R')

set_sub("tidy")

load_datas()

set_sub("models")

datamod <- filter(segs, !is.na(SurveyYear))
datamod %<>% select(-IT200, -IT500)

# downweight absence points (npresent/nabsent)
weight <- sum(datamod$Occur)/(nrow(datamod) - sum(datamod$Occur))
datamod %<>% mutate(Weight = ifelse(Occur == 0, weight, 1))

datatemp <- filter(datamod, SurveyYear == 2005)
dataspat <- filter(datamod, Zone %in% c(2, 3, 4))

#shznpreds <- c("IslandArea", "Mussel", "Fucus", "BioExp", "SegLength", "ShoreType")
fullpreds <- names(datamod)[c(3:14)]

# fit models - 10 iterations
fit_gbm <- function(data, iter = 10, gbm.x = fullpreds) {
  
  data %<>% as("Spatial")
  mod <- vector("list", iter)
  
  for (i in 1:iter) {
    mod[[i]] <- dismo::gbm.step(data = data@data, gbm.x = gbm.x, gbm.y = "Occur",
                                family = "bernoulli", tree.complexity = 3, learning.rate = 0.002,
                                bag.fraction = 0.5, site.weights = data$Weight, fold.vector = data$Fold)
  }
  mod
}

modfull <- fit_gbm(data = datamod, iter = 10)

# variable selection
simplify_gbm <- function(data, iter = 10, n.drops) {
  simp <- vector("list", iter)
  for (i in 1:iter) {
    simp[[i]] <- gbm.simplify(data[[i]])
  }
  simp
}

fullsimp <- simplify_gbm(modfull, iter = 10)

# build final models, including independent evaluation training sets
for(i in 1:10) {
  print(fullsimp[[i]]$final.drops)
}

namesimp.full <- fullsimp[[1]]$pred.list$preds.4

modsimp.full <- fit_gbm(data = datamod, gbm.x = namesimp.full)
modtemp.full <- fit_gbm(data = datatemp, gbm.x = namesimp.full)
modspat.full <- fit_gbm(data = dataspat, gbm.x = namesimp.full)

save_object(modfull)
save_object(modsimp.full)
save_object(modtemp.full)
save_object(modspat.full)

save_object(fullsimp)

save_data(datamod)

# generate predictions
set_sub("prediction")

predict_gbm <- function(model, data, iter = 10) {
  pred <- data.frame(matrix(nrow = nrow(data), ncol = iter))
  
  for (i in 1:iter) {
    ntrees <- model[[i]]$n.trees
    pred[,i] <- predict.gbm(model[[i]], data, n.trees = ntrees, type = "response") 
  }
  mean <- rowMeans(pred) %>% as.vector()
}

datpred <- mutate(segs, ProbOccur =  predict_gbm(modsimp.full, segs, iter = 1))

save_data(datpred)


