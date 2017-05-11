source('header.R')

set_sub("models")

mod <- load_object("modsimp.full")
predictors <- mod[[1]]$gbm.call$predictor.names

set_sub("models")

datamod <- load_data("datamod")

set_sub("partdep")

get_partdep <- function(model = mod[[1]], pred = predictors) {
  part <- vector("list", length(pred))
  names(part) <- pred
  for (i in 1:length(predictors)){
    part[[i]] <- plot.gbm(model, i.var = pred[i], n.trees = model$n.trees, 
                     continous.resolution = 100, type = "response", return.grid = T) %>%
      mutate(yc = scale(y, scale = FALSE),
             ylog = 1/(1+exp(-y)))
  }
  part
}

partdep <- get_partdep(mod[[1]])

plot_cont <- function(data = partdep, pred = "SegLength", log = F) {
  
  part <- data[[pred]]
  
  dat <- datamod
  st_geometry(dat) <- NULL
  
  gp <- ggplot(part, aes_string(x = pred, y = "y")) + 
    geom_line() + theme_classic() +
    geom_rug(data = dat, aes(x = dat[, pred], y=0), cex = 0.07, sides = "t") +
    labs(x = pred, y = "") + ylim(0, 0.8) +
    # geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) + 
    theme(text = element_text(size = 16), axis.title.y = element_text(margin = margin(0,15,0,0)), 
          plot.margin = unit(c(0.2, 0.2, 0.7, 0.2), "cm"), axis.text = element_text(size = 8), 
          axis.title = element_text(size = 11, face = 'bold'), 
          axis.title.x = element_text(margin = margin(7, 0, 0, 0))) 
  
  if (log == T) {
    gp + annotation_logticks(sides="b", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm")) + 
      scale_x_continuous(labels = scales::math_format(10^.x))
  }
  
  else {
    gp
  }
}

part <- partdep[["ShoreType"]] %>% select_("ShoreType", "y")
level = arrange(part, -y)[1:nrow(part), 1]
part[,1] <- ordered(part[,1], levels = level)

plot_fact <- function(data = partdep, pred = "ShoreType") {
  part <- data[[pred]] %>% select_(pred, "y")

  level = arrange(part, y)[1:nrow(part), 1]
  part[,1] <- ordered(part[,1], levels = level)
  
  gp <- ggplot(data = part, aes_string(x = pred, y = "y")) +
    geom_bar(stat = "identity", width = 0.4, colour = "black") + 
    theme_classic()  + coord_flip() +
    # scale_fill_manual(values = c("light grey","white")) +
    labs(x = "", y = "") + 
    theme(axis.text.y = element_text(hjust = 1, vjust = 0.5),
          axis.text.x = element_blank(), legend.position = "none", 
          axis.text = element_text(size = 11, margin = margin(20, 0, 0, 0)),
          axis.title = element_text(size = 14, face = 'bold'),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0))) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.6)) 
  
  gp
}

seglength <- plot_cont(pred = "SegLength", log = F)
fetch <- plot_cont(pred = "Fetch",  log = T)
it50 <- plot_cont(pred = "ITArea50",  log = F)
it1000 <- plot_cont(pred = "ITArea1000", log = F)
treedist <- plot_cont(pred = "TreeDist", log = T)
islandarea <- plot_cont(pred = "IslandArea", log = T)
humandist <- plot_cont(pred = "HumanDist", log = F)

cont <- grid.arrange(treedist, islandarea, fetch, humandist, seglength, it50, it1000, ncol = 2)
print(cont)

save_plot(cont)

shoretype <- plot_fact(pred = "ShoreType")
ratstatus <- plot_fact(pred = "RatStatus")

fact <- grid.arrange(shoretype, ratstatus, ncol = 1)
fact
save_plot(fact)









