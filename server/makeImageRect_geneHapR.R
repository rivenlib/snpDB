makeImageRect_geneHapR<-function (nrow, ncol, cols, name, byrow = TRUE) 
{
  xx <- (1:ncol)/ncol
  yy <- (1:nrow)/nrow
  if (byrow) {
    right <- rep(xx, nrow)
    top <- rep(yy, each = ncol)
  }
  else {
    right <- rep(xx, each = nrow)
    top <- rep(yy, ncol)
  }
  grid::rectGrob(x = right, y = top, width = 1/ncol, height = 1/nrow, 
                 just = c("right", "top"), gp = grid::gpar(col = NA, 
                                                           fill = cols), name = name)
}
