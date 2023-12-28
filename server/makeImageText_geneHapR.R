makeImageText_geneHapR<-function (nrow, ncol, cols, name, flip = FALSE) 
{
  cols <- as.character(cols)
  cols[is.na(cols)] <- ""
  cols <- paste(" ", cols)
  xx <- (1:ncol)/ncol
  yy <- (1:nrow)/nrow
  if (flip) {
    right <- rep(xx, each = nrow)
    top <- rep(yy, ncol)
  }
  else {
    right <- rep(xx, nrow)
    top <- rep(yy, each = ncol)
  }
  grid::textGrob(cols, x = right, y = top, gp = grid::gpar(cex = 0.3), 
                 just = c("right", "top"), name = name)
}
