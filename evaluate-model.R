source('header.R')

set_sub("models")

load_datas()
load_objects()

set_sub("evaluation")

testtemp <- filter(datamod, SurveyYear == 2010)
testspat <- filter(datamod, Zone == 1)

### mean RI
get_ri <- function(data, model) {

  modsumm <- vector("list", 10)
  for (i in 1:10) {
    modsumm[[i]] <- summary(data[[i]], order = F)
  }
  
  names <- data[[1]]$gbm.call$predictor.names
  modri <- data.frame(matrix(nrow = 10, ncol = length(names)))
  
  for ( i in 1:10) {
    for(j in 1:12) {
      modri[i,j] <- round(modsumm[[i]]$rel.inf[j], 0)
    }
  }
  
  meanri <- as.data.frame(sapply(modri, mean))
  names(meanri) <- "RI"
  meanri %<>% mutate(Model = model) %>%
    filter(!is.na(RI)) %>%
    mutate(Predictor = names)
  
}

# tmp <- get_ri(data = modfull, model = "modfull")
fulldrop <- fullsimp[[2]]$final.drops %>% 
  filter(!is.na(order)) 
fulldrop <- as.character(fulldrop[1:nrow(fulldrop),1])

allri <- data.frame(get_ri(modsimp.full, model = "modsimp.full")) %>%
  spread(key = Model, value = RI) %>%
  arrange(-modsimp.full) %>%
  select(Predictor, modsimp.full)

level = arrange(allri, -modsimp.full)[1:nrow(allri), 1]
allri %<>% mutate(Predictor = ordered(Predictor, levels = level))

allri %<>% mutate(`RI (%)` = modsimp.full) %>%
  select(-modsimp.full) %>%
  arrange(Predictor)

save_table(allri)

### mean AUC
auc_ind <- function(model, test, iter = 10) {
  pred <- data.frame(matrix(nrow = nrow(test), ncol = iter))
  
  for (i in 1:iter) {
    ntrees <- model[[i]]$n.trees
    pred[,i] <- predict.gbm(model[[i]], test, n.trees = ntrees, type = "response") 
  }
  mean <- data.frame(Prob = rowMeans(pred), Occ = test$Occur) 
  
  pres <- mean[mean[,2] == 1, 1]
  abs <- mean[mean[,2] == 0, 1]
  eval <- round(evaluate(pres, abs)@auc, 3)
}

auc_train <- function(model, test = datamod, iter = 10) {
  auc <- data.frame(matrix(nrow = iter, ncol = 1))
  for (i in 1:iter) {
    auc[i,] <- model[[i]]$self.statistics$discrimination
  }
  fin <- round(mean(auc[,1]), 3)
}

auc_cv <- function(model, test = datamod, iter = 10) {
  auc <- data.frame(matrix(nrow = iter, ncol = 1))
  for (i in 1:iter) {
    auc[i,] <- model[[i]]$cv.statistics$discrimination.mean
  }
  fin <- round(mean(auc[,1]), 3)
}

aucsumm <- data.frame("Evaluation Method" = c("Training", "10-fold Cross-Validation", 
                                              "North/South Partition", "2005/2010 Partition"),
                      `AUC` = c(auc_train(modsimp.full), 
                                             auc_cv(modsimp.full), 
                                             auc_ind(model = modspat.full, test = testspat), 
                                             auc_ind(model = modtemp.full, test = testtemp))
                     )

save_table(aucsumm)
