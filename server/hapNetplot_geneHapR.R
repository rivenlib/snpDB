hapNetplot_geneHapR<-function (hapNet, size = "freq", scale = 1, cex = 0.8, cex.legend = 0.6, 
          col.link = 1, link.width = 1, show.mutation = 1, backGround = backGround, 
          hapGroup = hapGroup, legend = FALSE, show_size_legend = TRUE, 
          show_color_legend = TRUE, main = main, labels = TRUE, ...) 
{
  if (!(inherits(hapNet, "haploNet") | inherits(hapNet, "hapNet"))) 
    stop("'hapNet' must be of 'hapNet' class")
  if (missing(hapGroup)) 
    hapGroup <- attr(hapNet, "hapGroup")
  if (size == "freq") 
    size <- attr(hapNet, "freq")
  else if (!is.numeric(size)) 
    stop("'size' should be 'freq' or a numeric vector")
  if (is.numeric(scale)) {
    size.sc <- size/scale
  }
  else if (scale == "log10") {
    size.sc <- log10(size + 1)
  }
  else if (scale == "log2") {
    size.sc <- log2(size + 1)
  }
  else {
    warning("Scale should be one of 'log10' or 'log2' or a numeric, ", 
            "scale will reset as 1")
  }
  if (!is.null(hapGroup)) {
    if (missing(backGround)) 
      backGround <- rainbow
      plot(hapNet, col.link = col.link, size = size.sc, cex = cex, 
         show.mutation = show.mutation, lwd = link.width, 
         bg = backGround, pie = hapGroup, labels = labels, 
         ...)
  }
  else {
    if (missing(backGround)) 
      backGround <- "grey90"
       plot(hapNet, col.link = col.link, bg = backGround, size = size.sc, 
             cex = cex, show.mutation = show.mutation, lwd = link.width, 
             labels = labels, ...)
      
  }
  if (legend[1]) {
    if (is.logical(legend)) {
      cat("Click where you want to draw the legend.")
      xy <- unlist(locator(1))
      cat("\nThe coordinates x = ", xy[1], ", y = ", xy[2], 
          " are used\n", sep = "")
    }
    else {
      if (!is.numeric(legend) || length(legend) < 2) 
        stop("wrong coordinates of legend")
      xy <- legend
    }
    SZ <- unique(size)
    SZ.sc <- unique(size.sc)
    if (show_size_legend) 
      if (length(SZ) > 1) {
        SZ.sc.50 <- (min(SZ.sc) + max(SZ.sc)) * 0.5
        SZ.sc.25 <- (min(SZ.sc) + SZ.sc.50) * 0.5
        SZ.sc.75 <- (SZ.sc.50 + max(SZ.sc)) * 0.5
        SZ.sc <- unique(c(min(SZ.sc), SZ.sc.25, SZ.sc.50, 
                          SZ.sc.75, max(SZ.sc)))
        if (is.numeric(scale)) 
          SZ <- SZ.sc * scale
        else SZ <- switch(scale, log10 = 10^SZ.sc - 
                            1, log2 = 2^SZ.sc - 1)
        SZ <- ceiling(SZ)
        SHIFT <- max(SZ.sc)
        vspace <- strheight(" ", cex = cex.legend)
        hspace <- strwidth(" ", cex = cex.legend)
        for (sz.sc in SZ.sc) {
          seqx <- seq(-sz.sc/2, sz.sc/2, length.out = 150)
          seqy <- sqrt((sz.sc/2)^2 - seqx^2)
          seqx <- c(seqx, rev(seqx))
          seqy <- c(seqy, -seqy)
          xy[2] <- xy[2] - max(seqy)
          lines(seqx + SHIFT/2 + xy[1], xy[2] + seqy)
          text(xy[1] + SHIFT + hspace * 2, xy[2] + seqy[150], 
               SZ[match(sz.sc, SZ.sc)], adj = c(0, 0.5), 
               cex = cex.legend)
          xy[2] <- xy[2] - max(seqy) - vspace
        }
        xy[2] <- xy[2] - 0.5 * vspace
      }
    if (show_color_legend) 
      if (!is.null(hapGroup)) {
        nc <- ncol(hapGroup)
        co <- if (is.function(backGround)) 
          backGround(nc)
        else rep(backGround, length.out = nc)
        legend(x = xy[1], y = xy[2], legend = colnames(hapGroup), 
               fill = co, cex = cex.legend)
      }
  }
  if (!missing(main)) 
    title(main = main)
}
