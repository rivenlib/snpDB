LDheatmapLegend_geneHapR.add<-function (color, LDmeasure, vp) 
{
  ImageRect <- makeImageRect_geneHapR(2, length(color), cols = c(rep(NA, 
                                                            length(color)), color[length(color):1]), "colorKey")
  keyVP <- grid::viewport(x = 1.1, y = -0.1, height = 0.1, 
                          width = 0.5, just = c("right", "bottom"), name = "keyVP")
  if (LDmeasure == "r") {
    ttt <- expression(paste(R^2, " Color Key"))
  }
  else {
    ttt <- "D' Color Key"
  }
  title <- grid::textGrob(ttt, x = 0.5, y = 1.25, name = "title", 
                          gp = grid::gpar(cex = 0.7))
  labels <- grid::textGrob(paste(0.2 * 0:5), x = 0.2 * 0:5, 
                           y = 0.25, gp = grid::gpar(cex = 0.6), name = "labels")
  ticks <- grid::segmentsGrob(x0 = c(0:5) * 0.2, y0 = rep(0.4, 
                                                          6), x1 = c(0:5) * 0.2, y1 = rep(0.5, 6), name = "ticks")
  box <- grid::linesGrob(x = c(0, 0, 1, 1, 0), y = c(0.5, 
                                                     1, 1, 0.5, 0.5), name = "box")
  key <- grid::gTree(children = grid::gList(ImageRect, title, 
                                            labels, ticks, box), name = "Key", vp = keyVP)
  key
}
