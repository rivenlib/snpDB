LDheatmap_geneHapR<-function (input,gdat, gff, Chr, start, end, geneID, genetic.distances = NULL, 
          distances = "physical", LDmeasure = LDmeasure, title = "Pairwise LD", 
          add.map = TRUE, map.height = 0.02, colorLegend = TRUE, geneMapLocation = 0.15, 
          geneMapLabelX = NULL, geneMapLabelY = NULL, SNP.name = NULL, 
          color = NULL, color_gmodel = color_gmodel, color_snp = color_snp, 
          #start_color=start_color,mid_color=mid_color,end_color=end_color,
          color_snpname = color_snpname, cex_snpname = cex_snpname, 
          snpmarks_height = snpmarks_height, newpage = TRUE, name = "ldheatmap", 
          vp.name = NULL, pop = FALSE, flip = TRUE, text = FALSE) 
{
  if (is.null(color)) {
    # color <- (grDevices::colorRampPalette(c("red", "orange",
    #                                              "yellow")))(30)
 
    color <- (grDevices::colorRampPalette(c(input$start_color, input$mid_color,
                                            input$end_color)))(30)

  }
  else if (length(color) < 10) {
    color <- (grDevices::colorRampPalette(c(color)))(30)
  }
  if (is.null(flip)) {
    flip <- FALSE
  }
  if (inherits(gdat, "data.frame")) {
    for (i in 1:ncol(gdat)) {
      if (!genetics::is.genotype(gdat[, i])) 
        stop("column ", i, " is not a genotype object\n")
    }
    gvars <- unlist(sapply(gdat, function(x) genetics::nallele(x) == 
                             2))
    if (any(!gvars)) 
      warning("Only bi-alleles supported,", "Variables with less or more than 2 allels will be omitted.")
    if (sum(gvars) < 2) 
      stop("Variants number is less than two after removed non-bialleles")
    genetic.distances <- genetic.distances[gvars]
    gdat <- gdat[gvars]
    if (!is.vector(genetic.distances)) {
      stop("Distance should be in the form of a vector")
    }
    o <- order(genetic.distances)
    genetic.distances <- genetic.distances[o]
    gdat <- gdat[, o]
    myLD <- genetics::LD(gdat)
    if (LDmeasure == "r") 
      LDmatrix <- myLD[[LDmeasure]]^2
    else if (LDmeasure == "D'") 
      LDmatrix <- abs(myLD[[LDmeasure]])
    else stop("Invalid LD measurement, choose r or D'.")
  }
  heatmapVP <- grid::viewport(width = unit(0.8, "snpc"), height = unit(0.8, 
                                                                       "snpc"), name = vp.name)
  flipVP <- grid::viewport(width = unit(0.8, "snpc"), height = unit(0.8, 
                                                                    "snpc"), y = 0.6, angle = -45, name = "flipVP")
  if (color[1] == "blueToRed")
    color = rainbow(20, start = 4/6, end = 0, s = 0.7)[20:1]
  if (newpage) 
    grid::grid.newpage()
  mybreak <- 0:length(color)/length(color)
  imgLDmatrix <- LDmatrix
  byrow <- ifelse(flip, FALSE, TRUE)
  colcut <- as.character(cut(1 - imgLDmatrix, mybreak, labels = as.character(color), 
                             include.lowest = TRUE))
  if (is.numeric(color)) 
    colcut <- as.integer(colcut)
  ImageRect <- makeImageRect_geneHapR(dim(LDmatrix)[1], dim(LDmatrix)[2], 
                             colcut, name = "heatmap", byrow)
  ImageText <- NULL
  if (text) 
    ImageText <- makeImageText_geneHapR(dim(LDmatrix)[1], dim(LDmatrix)[2], 
                               round(imgLDmatrix, digits = 2), name = "heatmaptext")
  title <- grid::textGrob(title, 0.5, 1.05, gp = grid::gpar(cex = 1), 
                          name = "title")
  if (flip) {
    ImageRect <- grid::editGrob(ImageRect, vp = flipVP)
    if (text) {
      ImageText <- makeImageText_geneHapR(dim(LDmatrix)[1], dim(LDmatrix)[2], 
                                 round(imgLDmatrix, digits = 2), name = "heatmaptext", 
                                 flip = TRUE)
      textVal <- ImageText
      ImageText <- grid::editGrob(ImageText, vp = flipVP, 
                                  rot = 0, just = c("right", "top"))
    }
  }
  heatMap <- grid::gTree(children = grid::gList(ImageRect, 
                                                ImageText, title), name = "heatMap")
  nsnps <- ncol(LDmatrix)
  step <- 1/(nsnps - 1)
  SNP.name <- names(SNP.name)
  p <- paste0("X", SNP.name)
  ind <- match(p, row.names(LDmatrix), nomatch = 0)
  geneMapVP <- NULL
  if (flip) 
    geneMapVP <- flipVP
  geneMap <- LDheatmapMapNew_geneHapR.add(nsnps, genetic.distances = genetic.distances, 
                                 geneMapLocation = geneMapLocation, add.map = add.map, 
                                 geneMapLabelX = geneMapLabelX, geneMapLabelY = geneMapLabelY, 
                                 distances = distances, vp = geneMapVP, SNP.name = SNP.name, 
                                 ind = ind, color_gmodel = color_gmodel, color_snp = color_snp, 
                                 color_snpname = color_snpname, cex_snpname = cex_snpname, 
                                 snpmarks_height = snpmarks_height, flip = flip, gff = gff, 
                                 Chr = Chr, start = start, end = end, geneID = geneID, 
                                 map.height = map.height)
  if (colorLegend) 
    Key <- LDheatmapLegend_geneHapR.add(color, LDmeasure, heatmapVP)
  else Key <- NULL
  LDheatmapGrob <- grid::gTree(children = grid::gList(heatMap, 
                                                      geneMap, Key), vp = heatmapVP, name = name, cl = "ldheatmap")
  grid::grid.draw(LDheatmapGrob)
  if (pop) {
    grid::downViewport(heatmapVP$name)
    grid::popViewport()
  }
  ldheatmap <- list(LDmatrix = LDmatrix, LDheatmapGrob = LDheatmapGrob, 
                    heatmapVP = heatmapVP, flipVP = geneMapVP, genetic.distances = genetic.distances, 
                    distances = distances, color = color)
  class(ldheatmap) <- "LDheatmap"
  invisible(ldheatmap)
}
