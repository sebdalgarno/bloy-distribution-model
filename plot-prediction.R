source('header.R')

palette = c("#253494", "#2c7fb8", "#41b6c4", "#7fcdbb", "#c7e9b4", "#ffffcc")
set_sub("prediction")

preds <- load_data("datpred")

set_sub("input")

hg <- load_data("hgpoly")

set_sub("maps")

bboxm <- st_bbox(preds) %>% 
  ldply() %>% 
  pull(V1)

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

plot_prediction <- function(data = preds, lwd = 0.2, bbox = bboxm, dist, lwdpred, 
                            padx1 = 0, padx2 = 0, pady1 = 0, pady2 = 0,
                            axes = T, scalesize = 3.5) {
  bbox <- c(bbox[1] + padx1, bbox[2] + pady1,
            bbox[3] - padx2, bbox[4] - pady2)
  boxp <- st_polygon(list(rbind(c(bbox[1], bbox[2]), c(bbox[1], bbox[4]),
                             c(bbox[3], bbox[4]), c(bbox[3], bbox[2]),
                             c(bbox[1], bbox[2]))))
  boxpsf <- st_sf(geometry = st_sfc(boxp, crs = 3005))
  data %<>% st_intersection(boxpsf)
  hg %<>% st_intersection(boxpsf)
  
  gp <- ggplot(data) + 
    ggplot2::geom_sf(data = hg, fill = "black", color = "black", lwd = 0.01) +
    ggplot2::geom_sf(data = data, aes(color = ProbOccur), lwd = lwdpred) +
    ggplot2::coord_sf(xlim = c(bbox[[1]], bbox[[3]]), ylim = c(bbox[[2]], bbox[[4]])) +
    scale_color_gradientn(colours = palette, guide = "colourbar") + 
    ggsn::scalebar(data = NULL, location = "bottomleft", dist = dist, 
                   height = 0.007, st.size = scalesize, st.dist = 0.015,
                   x.min = bbox[[1]], x.max = bbox[[3]], y.min = bbox[[2]], y.max = bbox[[4]]) +
    ggsn::north(data = NULL, location = "topright", scale = 0.1, symbol = 7,
                x.min = bbox[[1]], x.max = bbox[[3]], y.min = bbox[[2]], y.max = bbox[[4]]) + 
    theme_dark()
  
  if(axes == T) {
    gp <- gp + labs(x = "Longitude", y = "Latitude", color = "Probability of\nOccurrence") +
      theme(axis.title = element_text(face = "bold", size = 15),
            legend.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 10))
    gp
  } else {
    gp <- gp + theme(axis.text = element_blank(),
               axis.ticks = element_blank(),
               legend.position = "none",
               axis.title = element_blank())
    gp
  }
}

gp <- plot_prediction(bbox = bboxm, dist = 10, lwdpred = 0.2,
                      pady2 = 5000, padx1 = 3000)

inset1 <- plot_prediction(bbox = bbox1, dist = 0.25, lwdpred = 1, 
                          padx1 = 200, pady2 = 200, 
                          padx2 = 100, pady1 = 100,
                          axes = T)

inset2 <- plot_prediction(bbox = bbox2, dist = 0.5, lwdpred = 1,
                          padx1 = 200, pady2 = 200, 
                          padx2 = 100, pady1 = 100,
                          axes = T)

save_plot()


