source('header.R')

palette = c("#253494", "#2c7fb8", "#41b6c4", "#7fcdbb", "#c7e9b4", "#ffffcc")
set_sub("prediction")

preds <- load_data("datpred")

set_sub("input")
hg <- load_data("hgpoly")

set_sub("tidy")
occ <- load_data("occ")
occ %<>% data.frame(st_coordinates(.))

set_sub("maps")

bboxm <- st_bbox(preds) 

# draw inset bounding boxes (this is how bboxes were determined,
# but this cannot be included in reproducible code)
# edit.bbox1 <- editMap(mapview(preds))
# bbox1 <- edit.bbox1$finished %>% 
#   st_transform(crs = 3005) %>% 
#   st_bbox() 

# edit.bbox2 <- editMap(mapview(preds))
# bbox2 <- edit.bbox2$finished %>% 
#   st_transform(crs = 3005) %>% 
#   st_bbox() 

# these values resulted from above mapedit process
bbox1 <- c(623115.7, 854581.1, 625224.0, 856503.9)
bbox2 <- c(627429.4, 854440.9, 634633.9, 860005.0)

save_object(bbox1)
save_object(bbox2)

plot_prediction <- function(data = preds, bbox, dist, lwdpred, 
                            padx1 = 0, padx2 = 0, pady1 = 0, pady2 = 0,
                            axes = T, scalesize = 3.5, ptsize) {
  
  bboxp <- c(bbox[1] + padx1, bbox[2] + pady1,
            bbox[3] - padx2, bbox[4] - pady2)

  gp <- ggplot(data) + 
    ggplot2::geom_sf(data = hg, fill = "black", color = "black", lwd = 0.01) +
    ggplot2::geom_sf(data = data, aes(color = ProbOccur), lwd = lwdpred) +
    ggplot2::geom_point(data = occ, aes(x = X, y = Y), fill = "white", color = "black", pch = 21, size = ptsize) +
    ggplot2::coord_sf(xlim = c(bboxp[1], bboxp[3]), ylim = c(bboxp[2], bboxp[4])) +
    scale_color_gradientn(colours = palette, guide = "colourbar") + 
    ggsn::scalebar(data = NULL, location = "bottomleft", dist = dist, 
                   height = 0.007, st.size = scalesize, st.dist = 0.015,
                   x.min = bboxp[[1]], x.max = bboxp[[3]], y.min = bboxp[[2]], y.max = bboxp[[4]]) +
    ggsn::north(data = NULL, location = "bottomleft", scale = 0.1, symbol = 7,
                x.min = bboxp[[1]], x.max = bboxp[[3]], y.min = bboxp[[2]], y.max = bboxp[[4]]) + 
    theme_dark()
  
  if(axes == T) {
    gp <- gp + labs(x = "Longitude", y = "Latitude", color = "Probability of\nOccurrence") +
      theme(axis.title = element_text( size = 13),
            legend.title = element_text(size = 13),
            axis.text = element_text(size = 9))
    gp
  } else {
    gp <- gp + theme(axis.ticks = element_blank(),
               legend.position = "none",
               axis.title = element_blank())
    gp
  }
}

psa <- plot_prediction(bbox = bboxm, dist = 10, lwdpred = 0.4, ptsize = 0) 

inset1 <- plot_prediction(dist = 0.25, bbox = bbox1, 
                          scalesize = 2.5, lwdpred = 0.7, 
                          padx1 = 200, pady2 = 200, 
                          padx2 = 100, pady1 = 100,
                          axes = F, ptsize = 2.5)


inset2 <- plot_prediction(bbox = bbox2, dist = 0.5, 
                          scalesize = 2.5, lwdpred = 0.8,
                          padx1 = 400, pady2 = 500, 
                          padx2 = 400, pady1 = 200,
                          axes = F, ptsize = 2) 

subfoldr::save_plot(plot = psa, x = "predict-studyarea", width = 7, height = 8, csv = F, report = F)
subfoldr::save_plot(plot = inset1, x = "predict-inset1", width = 5, height = 5, csv = F, report = F)
subfoldr::save_plot(plot = inset2, x = "predict-inset2", width = 6, height = 5, csv = F, report = F)



