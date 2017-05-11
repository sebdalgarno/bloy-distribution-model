source('header.R')
library(ggsn)

palette = c("#253494", "#2c7fb8", "#41b6c4", "#7fcdbb", "#c7e9b4", "#ffffcc")
set_sub("prediction")

preds <- load_data("datpred")

set_sub("input")

hg <- load_data("hgpoly")

set_sub("maps")

preds2 <- preds
preds2 %<>% st_cast("POINT") 
preds2 %<>% data.frame(st_coordinates(.))

lims <- st_bbox(preds)

plot_prediction <- function(data = preds, lwd = 0.2, 
                            x.min = lims[1], x.max = lims[3], y.min = lims[2], y.max = lims[4], 
                            xlim = c(lims[1], lims[3]), ylim = c(lims[2], lims[4])) {
  gp <- ggplot(data) + 
    ggplot2::geom_sf(data = hg, fill = "black", color = "black", lwd = 0.01) +
    ggplot2::geom_sf(data = data, aes(color = ProbOccur), lwd = lwd) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    scale_color_gradientn(colours = palette, 
                          name = "Probability of\nOccurence", guide = "colourbar") + 
    labs(x = "Longitude", y = "Latitude") +
    guides(colour = guide_legend(override.aes = list(size = 1))) +
    ggsn::scalebar(data = NULL, location = "bottomleft", dist = 10, 
                   height = 0.007, st.size = 2.3, st.dist = 0.015,
                   x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max) +
    ggsn::north(data = NULL, location = "bottomleft", scale = 0.1, symbol = 7,
                x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max) + 
    theme_dark()
}

xlims1 <- c(623000, 625000)
ylims1 <- c(856500, 854500)

gp <- plot_prediction()
gp

inset1 <- plot_prediction(xlim = xlims1, ylim = ylims1, lwd = 1)
inset1

xlims2 <- c(627500, 634300)

inset1 <- gp + coord_sf(xlim = xlims1, ylim = ylims1)
inset1



gp
