
LDheatmapMapNew_geneHapR.add<-function (nsnps, add.map, map.height = 0.02, genetic.distances, 
          geneMapLocation = 0.15, geneMapLabelX = NULL, geneMapLabelY = NULL, 
          distances = "physical", vp = NULL, SNP.name = NULL, ind = 0, 
          color_gmodel = color_gmodel, color_snp = color_snp, color_snpname = color_snpname, 
          cex_snpname = cex_snpname, snpmarks_height = snpmarks_height, 
          flip = FALSE, gff, Chr, start, end, geneID) 
{
  snp <- ((1:nsnps - 1) + 0.5)/nsnps
  if (add.map) {
    if (missing(start) | missing(end)) {
      min.dist <- min(genetic.distances)
      max.dist <- max(genetic.distances)
    }
    else {
      min.dist <- min(genetic.distances, start, end)
      max.dist <- max(genetic.distances, start, end)
    }
    total.dist <- max.dist - min.dist
    if (flip) 
      geneMapLocation <- (-geneMapLocation)
    i <- 0.01
    seq.x <- c(0.5 * geneMapLocation + 1/(nsnps * 2), 1 + 
                 0.5 * geneMapLocation + 1/(nsnps * 2))
    seq.y <- c(-0.5 * geneMapLocation - 1/(nsnps * 2), 1 - 
                 0.5 * geneMapLocation - 1/(nsnps * 2))
    diagonal <- grid::linesGrob(seq.x, seq.y, gp = grid::gpar(lty = 1, 
                                                              col = color_snp), name = "diagonal", vp = vp)
    xs <- ys <- c()
    w <- map.height
    if (!missing(Chr) & !missing(gff) & add.map) {
      gene <- GenomicRanges::GRanges(Chr, IRanges::IRanges(start = min.dist, 
                                                           end = max.dist))
      gff <- gff[gff %over% gene]
      type <- tolower(gff$type)
      probe1 <- stringr::str_detect(type, "cds") | stringr::str_detect(type, 
                                                                       "utr")
      gff <- gff[probe1]
      if (!is.null(geneID)) {
        ids <- tolower(gff$ID)
        nms <- tolower(gff$Name)
        geneID <- tolower(geneID)
        probe2 <- stringr::str_detect(nms, geneID) | 
          stringr::str_detect(ids, geneID)
        gff <- gff[probe2]
      }
      gff$Parent <- unlist(gff$Parent)
      strand <- unique(gff@strand)[1]
      ps <- unique(gff$Parent)
      for (p in seq_len(length(ps))) {
        gffp <- gff[gff$Parent == ps[p]]
        for (i in seq_len(length(gffp@ranges))) {
          start <- gffp@ranges[i]@start
          wid <- gffp@ranges[i]@width
          end <- start + wid
          r1 <- (start - min.dist)/total.dist
          r2 <- (end - min.dist)/total.dist
          x1 <- seq.x[1] + (seq.x[2] - seq.x[1]) * r1
          x2 <- seq.x[1] + (seq.x[2] - seq.x[1]) * r2
          y1 <- seq.y[1] + (seq.y[2] - seq.y[1]) * r1
          y2 <- seq.y[1] + (seq.y[2] - seq.y[1]) * r2
          if (stringr::str_detect(tolower(gff$type[i]), 
                                  "cds")) 
            v <- 1
          else v <- 1/2
          xsp <- c(x1 - w * v, x1 + w * v, x2 + w * 
                     v, x2 - w * v) - w * (p * 3 - 1.5)
          ysp <- c(y1 + w * v, y1 - w * v, y2 - w * 
                     v, y2 + w * v) + w * (p * 3 - 1.5)
          xs <- c(xs, xsp, NA)
          ys <- c(ys, ysp, NA)
        }
        xsp <- c(seq.x[1] - 0.002, seq.x[1] + 0.002, 
                 seq.x[2] + 0.002, seq.x[2] - 0.002)
        ysp <- c(seq.y[1] + 0.002, seq.y[1] - 0.002, 
                 seq.y[2] - 0.002, seq.y[2] + 0.002)
        xs <- c(xs, NA, xsp - w * (p * 3 - 1.5), NA)
        ys <- c(ys, NA, ysp + w * (p * 3 - 1.5), NA)
      }
      xs <- xs + 0.2 * geneMapLocation + 1/(nsnps * 2)
      ys <- ys - 0.2 * geneMapLocation - 1/(nsnps * 2)
    }
    else gmodel <- NULL
    regionx <- seq.x[1] + ((genetic.distances - min.dist)/total.dist) * 
      (seq.x[2] - seq.x[1])
    regiony <- seq.y[1] + ((genetic.distances - min.dist)/total.dist) * 
      (seq.y[2] - seq.y[1])
    if (distances == "physical") 
      mapLabel <- paste("Physical Length:", round((total.dist/1000), 
                                                  1), "kb", sep = "")
    else mapLabel <- paste("Genetic Map Length:", round(total.dist, 
                                                        1), "cM", sep = "")
    if (strand == "+") 
      mapLabel <- paste0(mapLabel, "\n5' -> 3'")
    else mapLabel <- paste0(mapLabel, "\n3' <- 5'")
    if (!flip) {
      if (is.null(geneMapLabelY)) 
        geneMapLabelY <- 0.3
      if (is.null(geneMapLabelX)) 
        geneMapLabelX <- 0.5
    }
    else {
      if (is.null(geneMapLabelY)) 
        geneMapLabelY <- 0.8
      if (is.null(geneMapLabelX)) 
        geneMapLabelX <- 0.4
    }
    title <- grid::textGrob(mapLabel, geneMapLabelX - w * 
                              length(ps) * 4, geneMapLabelY + w * length(ps) * 
                              4, gp = grid::gpar(cex = 0.9), just = "left", name = "title")
    if (!is.null(SNP.name) && (any(ind != 0))) {
      if (flip) {
        length_SNP_name <- max(nchar(SNP.name))
        long_SNP_name <- paste(rep(8, length_SNP_name), 
                               collapse = "")
        name_gap <- grid::convertWidth(grid::grobWidth(grid::textGrob(long_SNP_name)), 
                                       "npc", valueOnly = TRUE)
        diagonal <- grid::linesGrob(seq.x - name_gap, 
                                    seq.y + name_gap, gp = grid::gpar(lty = 1), 
                                    name = "diagonal", vp = vp)
        segments <- segmentsGrob(snp - name_gap, snp + 
                                   name_gap, regionx - name_gap, regiony + name_gap, 
                                 name = "segments", vp = vp)
        if (is.null(snpmarks_height)) 
          snpmarks <- NULL
        else snpmarks <- grid::segmentsGrob(x0 = regionx - 
                                              name_gap, y0 = regiony + name_gap, x1 = regionx - 
                                              w * (p * 3 + 2) - snpmarks_height, y1 = regiony + 
                                              w * (p * 3 + 2) + snpmarks_height, name = "snpmarks", 
                                            vp = vp, gp = gpar(col = color_snp))
        symbols <- grid::pointsGrob(snp[ind] - name_gap, 
                                    snp[ind] + name_gap, pch = "", gp = grid::gpar(cex = 0.25, 
                                                                                   bg = "blue", col = "blue"), name = "symbols", 
                                    vp = vp)
        SNPnames <- grid::textGrob(SNP.name, just = -0.1, 
                                   rot = -45, snp[ind] - name_gap, snp[ind] + 
                                     name_gap, gp = grid::gpar(cex = cex_snpname, 
                                                               col = color_snpname), name = "SNPnames", 
                                   vp = vp)
        title <- grid::editGrob(title, y = unit(geneMapLabelY + 
                                                  name_gap + 0.05, "npc"))
        if ((!is.null(xs)) & (!is.null(ys))) 
          gmodel <- grid::polygonGrob(x = xs - name_gap, 
                                      y = ys + name_gap, gp = gpar(fill = color_gmodel, 
                                                                   lty = 0), vp = vp)
      }
      geneMap <- grid::gTree(children = grid::gList(diagonal, 
                                                    segments, title, gmodel, snpmarks, symbols, 
                                                    SNPnames), name = "geneMap")
    }
  }
  else if (!add.map && !is.null(SNP.name) && (any(ind != 0))) {
    geneMap <- grid::textGrob(paste(" ", SNP.name), just = "left", 
                              rot = -45, snp[ind] - name_gap, snp[ind] + name_gap, 
                              gp = grid::gpar(cex = cex_snpname, col = color_snpname), 
                              name = "SNPnames")
    if (flip) 
      geneMap <- grid::editGrob(geneMap, vp = vp)
  }
  else geneMap <- NULL
  geneMap
}
