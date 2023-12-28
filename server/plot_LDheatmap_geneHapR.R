plot_LDheatmap_geneHapR<-function (input,hap, gff, Chr, start, end, geneID = NULL, distances = "physical", 
                                   LDmeasure = "r", title = "Pairwise LD", add.map = TRUE, 
                                   map.height = 1, colorLegend = TRUE, geneMapLocation = 0.15, 
                                   geneMapLabelX = NULL, geneMapLabelY = NULL, SNP.name = TRUE, 
                                   color = NULL, color_gmodel = "grey", color_snp = "grey", 
                                   #start_color=start_color,mid_color=mid_color,end_color=end_color,
                                   color_snpname = "grey40", cex_snpname = 0.8, snpmarks_height = NULL, 
                                   newpage = TRUE, name = "ldheatmap", vp.name = NULL, pop = FALSE, 
                                   text = FALSE) 
{
  flip = TRUE
  if (inherits(hap, "hapResult")) {
    gdat <- data.frame(hap)
    if (isFALSE(SNP.name)) 
      SNP.name <- NULL
    else if (isTRUE(SNP.name)) 
      SNP.name <- t(hap[4, -c(1, ncol(hap))])[, 1]
    genetic.distances <- suppressWarnings(as.numeric(gdat[2, 
    ])) %>% na.omit()
    gdat <- gdat[-c(1:4), -c(1, ncol(gdat))]
    for (i in seq_len(ncol(gdat))) {
      gdat[, i] <- sapply(gdat[, i], function(x) {
        if (stringr::str_detect(x, "[|]")) {
          stringr::str_replace(x, "[|]", "/")
        }
        else {
          paste0(x, "/", x)
        }
      }) %>% unlist()
      gdat[, i] <- genetics::as.genotype(gdat[, i])
    }
  }
  else return("Use hapResult instead of hapSummary")
  LDheatmap_geneHapR(input,gdat, gff = gff, Chr = Chr, start = start, end = end, 
            geneID = geneID, genetic.distances = genetic.distances, 
            distances = distances, LDmeasure = LDmeasure, title = title, 
            add.map = add.map, map.height = map.height, colorLegend = colorLegend, 
            geneMapLocation = geneMapLocation, geneMapLabelX = geneMapLabelX, 
            geneMapLabelY = geneMapLabelY, SNP.name = SNP.name, 
            color = color, color_gmodel = color_gmodel, color_snp = color_snp,
            #start_color=start_color,mid_color=mid_color,end_color=end_color,
            color_snpname = color_snpname, cex_snpname = cex_snpname, 
            snpmarks_height = snpmarks_height, newpage = newpage, 
            name = "ldheatmap", vp.name = vp.name, pop = pop, flip = flip, 
            text = text)
}
