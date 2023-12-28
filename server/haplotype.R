
haplotype_server<-function(input,output,session){
 
       	# Update the choices of the selectInput
   colnames <- c("Red", "Blue", "Green", "Purple", "Orange", "Yellow", "Brown", "Pink")
   updateSelectInput(session, "start_color", choices = colnames,selected = "Yellow")
   updateSelectInput(session, "mid_color", choices = colnames,selected = "Orange")
   updateSelectInput(session, "end_color", choices = colnames,selected = "Red")
  
    # ==================import vcf=========================#

    vcf <- eventReactive(input$vcf_im, {
        vcf <- if(is.surfix(input$vcf_file$datapath, c("vcf","vcf.gz")))
            geneHapR::import_vcf(input$vcf_file$datapath) else
                NULL
        if(!is.null(vcf)){
            chrs <- as.list(unique(vcf@fix[,1]))
            names(chrs) <- as.vector(chrs)
            updateSelectInput(session, "filter_chr",
                              choices = chrs,
                              selected = chrs[[1]])
            start <- min(as.numeric(vcf@fix[,2]))
            end <- max(as.numeric(vcf@fix[,2]))
            updateNumericInput(session,"filter_start",
                               value = start,
                               min = start,
                               max = end)
            updateNumericInput(session,"filter_end",
                               value = end,
                               min = start,
                               max = end)
        }
        vcf
    }
     )


    observeEvent(input$vcf_im,{
        #info <- vcf()
        output$vcf_info <- renderPrint({vcf()})
    })

    # ==================import annotation files=========================#

    gff <- eventReactive(input$gff_im,{
        gff <- if(is.surfix(input$gff_in$datapath, c("gff","gff3","bed","bed6","bed4")))
            switch(input$gff_format,
                   "GFF" = geneHapR::import_gff(input$gff_in$datapath),
                   "BED" = geneHapR::import_bed(input$gff_in$datapath)) else NULL
        if(!is.null(gff)){
            choices <- as.vector(unique(gff$type))
            updateCheckboxGroupInput(session,
                                     "filter_type",
                                     choiceNames = choices,
                                     choiceValues = choices)
            updateCheckboxGroupInput(session,
                                     "geneElement", inline = TRUE,
                                     choiceNames = choices,
                                     choiceValues = choices)

        }
        gff
    })

    # import annotation file
    observeEvent(input$gff_im,{
        output$annotation_data <- renderPrint(gff())
    })

    # ==================import pheno files=========================#

    pheno <- eventReactive(input$pheno_im,{
        if(any(is.surfix(input$pheno_in$datapath, c("txt","csv")))){
            switch(input$pheno_format,
                   "txt" = geneHapR::import_AccINFO(input$pheno_in$datapath),
                   "csv" = utils::read.csv(input$pheno_in$datapath, header = TRUE, row.names = 1))
        }
    })

    observeEvent(input$pheno_im,{
        pheno_object <- pheno()
        output$pheno_info <- renderUI({
          if(is.null(pheno())){
            
            text<-NULL
          } else {

            text <- paste(h4("Imported pheno names:"), br(),
                          paste(names(pheno_object), collapse = "; "))
          }
            
            HTML(text)})
        if(input$pheno_showdata)
        {
          output$pheno_data <- renderTable(pheno_object) 
        }
        else
        {
          output$pheno_data <- renderTable(NULL)
        }
                
        choices <- as.list(names(pheno_object))
        names(choices) <- names(pheno_object)
        updateSelectInput(session, "hapvspheno_phenoname", choices = choices)
    })



    # =======================import accinfo file==============#
    accinfo <- eventReactive(input$accinfo_im,{
        if(nchar(input$accinfo_in) > 0 & any(is.surfix(input$accinfo_in, c("txt","csv")))){
            switch(input$accinfo_format,
                   "txt" = geneHapR::import_AccINFO(input$accinfo_in),
                   "csv" = utils::read.csv(input$accinfo_in, header = TRUE, row.names = 1))
        } else NULL
    })

    
    
    # ==================================haplotyping===================================#
    fvcf <- eventReactive(input$hapresult_bt,{
        vcf <- vcf()
        message("orignal variants: ", nrow(vcf@fix))
        message("filter mode: ", input$filter_mode)
        if(input$filter_mode == "none") vcf <- vcf() else {
            if(input$filter_mode %in% c("POS","both")) {
                message("filter position: ", input$filter_chr,": ",
                        input$filter_start,"-",
                        input$filter_end)
            }
            if(input$filter_mode %in% c("type","both")) {
                message("filter type: ", input$filter_type)
                if(is.null(gff()) | is.null(input$filter_type))
                    output$hapresult <- renderPrint("Please input annotation or cancel filter by type")
            }
            vcf <- geneHapR::filter_vcf(vcf(), gff = gff(),
                                        mode = input$filter_mode,
                                        Chr = input$filter_chr,
                                        start = input$filter_start,
                                        end = input$filter_end,
                                        type = input$filter_type)
            message("variants after filter: ", nrow(vcf@fix))
        }
        vcf
    })
    
    
    
    geno_fmt <- reactive("vcf"
      #{input$hapresult_source}
      )
    hapre <- eventReactive(input$hapresult_bt,{
        hapre <- try(switch(geno_fmt(),
                            "vcf" = geneHapR::vcf2hap(fvcf(),
                                                      hapPrefix = input$happrefix,
                                                      hyb_remove = input$hyb_remove,
                                                      na.drop = input$na_drop)
                            )
                     )
        hapre
    })
    hapsum <- eventReactive(input$hapsummary_bt,{
        geneHapR::hap_summary(hapre())
    })
       observeEvent(input$filter_gentype_info,{
      vcf <- vcf()
      message("orignal variants: ", nrow(vcf@fix))
      message("filter mode: ", input$filter_mode)
      if(input$filter_mode == "none") vcf <- vcf() else {
        if(input$filter_mode %in% c("POS","both")) {
          message("filter position: ", input$filter_chr,": ",
                  input$filter_start,"-",
                  input$filter_end)
        }
        if(input$filter_mode %in% c("type","both")) {
          message("filter type: ", input$filter_type)
          if(is.null(gff()) | is.null(input$filter_type))
            output$filter_info <- renderPrint("Please input annotation or cancel filter by type")
        }
        vcf <- geneHapR::filter_vcf(vcf(), gff = gff(),
                                    mode = input$filter_mode,
                                    Chr = input$filter_chr,
                                    start = input$filter_start,
                                    end = input$filter_end,
                                    type = input$filter_type)
        message("variants after filter: ", nrow(vcf@fix))
      }
      vcf
      output$filter_info <- renderPrint(vcf)
    })

    output$hapsummary_DL_bt<- downloadHandler(
    filename = function() {
     "hapResult.csv"
    },
    content = function(file) {

      write.csv(hapsum() , file)
    }
  )

    
    
 
    observeEvent(input$hapresult_bt,{
        hapresult_object <- hapre()
        output$hapresult <- renderPrint(hapresult_object)
        if(inherits(hapresult_object, "hapResult")){
            haps <- names(attr(hapresult_object, "freq"))
            updateRadioButtons(session, inputId = "hapvspheno_hap1",
                               choiceNames = haps,choiceValues = haps)
            updateRadioButtons(session, inputId = "hapvspheno_hap2",
                               choiceNames = haps,choiceValues = haps)
            haps <- haps[attr(hapresult_object, "freq") >= 2]
            # updateCheckboxGroupInput(session, inputId = "hapdistribution_hapnames",
            #                          choiceNames = haps,choiceValues = haps, inline = TRUE)
           
            
            info <- hapresult_object[3, -c(1, ncol(hapresult_object))] %>%
                data.frame() %>% t()
            info <- info[,1]
            info <- sapply(info, function(x) strsplit(x, "=")[[1]][1]) %>%
                unlist() %>% unique()
            
            if(length(info) >= 1){
                infos <- as.list(c(info,"none"))
                names(infos) <- c(info, "none")
                updateSelectInput(session, "plothaptable_infotag",
                                  choices = infos,
                                  selected = "none")
              
            }
        }
    })

    observeEvent(input$hapsummary_bt,{
        hapsummary_object <- hapsum()
        output$hapsummary <- renderPrint(hapsummary_object)
        
        chr <- unique(hapsummary_object[1, c(2:(ncol(hapsummary_object) - 2))])

        chr_list <- as.list(chr)

        names(chr_list) <- chr

        updateSelectInput(session, "displayVarOnGeneModel_chromosome",
                          choices = chr_list)
        updateSelectInput(session, "LD_heatmap_Chr",
                          choices = chr_list)
       
        pos <- as.numeric(hapsummary_object[2,c(2:(ncol(hapsummary_object) - 2))])
        updateNumericInput(session, "displayVarOnGeneModel_start", value = min(pos))
        updateNumericInput(session, "displayVarOnGeneModel_end", value = max(pos))
        updateNumericInput(session, "LD_heatmap_start", value = min(pos))
        updateNumericInput(session, "LD_heatmap_end", value = max(pos))
       
    })
#==========================================Visulization===============================================#

    haptable_plot <- reactiveValues()
    # plotHapTable
    observeEvent(input$plothaptable,{
        genename <- if(input$plothaptable_genename == "") FALSE else
            input$plothaptable_genename
        title <- input$plothaptable_title
        if(nchar(title) == 0) title <- ""
        if(input$plothaptable_infotag == "none") {
            infotag <- tagsplit <- tagfield <- tagname <- NULL
        } else {
            infotag <- input$plothaptable_infotag
            tagsplit <- input$plothaptable_tagsplit
            tagfield <- input$plothaptable_tagfield
            tagname <- input$plothaptable_tagname
        }
        hapSummary <- hapsum()
        output$plothaptable <- renderPlot(
            {
              haptable_plot$plot<-geneHapR::plotHapTable(
                    hapSummary = hapSummary,
                    title = title,
                    hapPrefix = input$plothaptable_prefix,
                    geneName = genename,
                    INFO_tag = infotag,
                    tag_split = tagsplit,
                    tag_field = tagfield,
                    tag_name = tagname,
                    angle = input$plothaptable_angle)
              haptable_plot$plot
            },
           # width = input$img_width,
           # height = input$img_height,
           # res = input$img_res
	    )})
    
    output$download_haptable <- downloadHandler(
      filename = function() {
        "haptable.pdf"
      },
      content = function(file) {
   
        ggsave(file, haptable_plot$plot, width = input$down_width, height = input$down_height, units = "in")
        
      }
    )
    

    # plothapnet
    legend_hapnet <- reactive(
        {if(any(input$plothapnet_legend_size,
                input$plothapnet_legend_color))
            c(input$plothapnet_x, input$plothapnet_y) else
                FALSE
        })

    observeEvent(input$plothaplonet_bt,{
        message("start")
        # message("processing")
        if(input$plothapnet_group != "none"){
            accinfo <- accinfo()
            message("accinfo")
            hapNet <- geneHapR::get_hapNet(hapsum(), AccINFO = accinfo,
                                           groupName = input$plothapnet_group)
        } else {
            hapNet <- geneHapR::get_hapNet(hapsum())
        }
        message("ploting")

      
        xlim <- input$plothapnet_xlim
        ylim <- input$plothapnet_ylim
        f <- tempfile()
        png(f)
        geneHapR::plotHapNet(hapNet)
        m <- round(par("usr"))
        dev.off()
        unlink(f)
        #updateSliderInput(session, "plothapnet_xlim",
        #                  min = m[1]*3,
        #                  max = m[2]*3,
        #                  value = c(m[1],m[2]))
        #updateSliderInput(session, "plothapnet_ylim",
        #                  min = m[3]*3,
        #                  max = m[4]*3,
        #                  value = c(m[3],m[4]))
        #updateSliderInput(session, "plothapnet_x",
        #                  min = -100,
        #                  max = 100,
        #                  value = 0)
        #updateSliderInput(session, "plothapnet_y",
        #                  min = -100,
        #                  max = 100,
        #                  value = 0)
        output$download_Network <- downloadHandler(
          filename = function() {
            "Network.pdf"
          },
          content = function(file) {
            pdf(file, width = input$down_width, height = input$down_height)
            hapNetplot_geneHapR(hapNet, scale = input$plothapnet_scale,
                      show.mutation = input$plothapnet_showmutation,
                      cex = input$plothapnet_cex,
                      label = input$plothapnet_labels,
                      cex.legend = input$plothapnet_cexlegend,
                      col.link = input$plothapnet_collink,
                      link.width = input$plothapnat_linkwidth,
                      legend = legend_hapnet(),
                      #show_color_legend = input$plothapnet_legend_color,
                      show_size_legend = input$plothapnet_legend_size,
                      #xlim = input$plothapnet_xlim,
                      #ylim = input$plothapnet_ylim
            )
            dev.off()        
          }
        )
        

        output$plothaplonet <- renderPlot({
          # geneHapR::plotHapNet(hapNet, scale = "log2",
          #            show.mutation = 2, # mutants type, one of 0,1,2,3
          #            col.link = 2, link.width = 2, # links colours and widths
          #            #main = geneID, # main title
          #            label = input$plothapnet_labels, # plot labels or not
          #            # legend = F) # do not add legend
          #            #legend = c(12,0),
          #            cex.legend = 0.6) # legend posizion and legend size   geneHapR::plotHapNet
          
          hapNetplot_geneHapR(hapNet, scale = input$plothapnet_scale,
                                 show.mutation = input$plothapnet_showmutation,
                                 cex = input$plothapnet_cex,
                                 label = input$plothapnet_labels,
                                 cex.legend = input$plothapnet_cexlegend,
                                 col.link = input$plothapnet_collink,
                                 link.width = input$plothapnat_linkwidth,
                                 legend = legend_hapnet(),
                                 #show_color_legend = input$plothapnet_legend_color,
                                 show_size_legend = input$plothapnet_legend_size,
                                 #xlim = input$plothapnet_xlim,
                                 #ylim = input$plothapnet_ylim
                                 )
        
        }, #width = input$img_width, height = input$img_height, res = input$img_res
        )


        message("end")

    })

    # display vars on model

    observeEvent(input$displayvar, {
        message("start ploting vars")
        Chr <- input$displayVarOnGeneModel_chromosome
        startPOS <- input$displayVarOnGeneModel_start
        endPOS <- input$displayVarOnGeneModel_end
        type <- input$displayVarOnGeneModel_type
        cex <- input$displayVarOnGeneModel_cex
        geneElement <- input$geneElement
        gff <- gff()
        hapSummary <-  hapsum()
        
        output$download_var <- downloadHandler(
          filename = function() {
            "Variants.pdf"
          },
          content = function(file) {
            pdf(file, width = input$down_width, height = input$down_height)
            geneHapR::displayVarOnGeneModel(gff = gff,hapSummary =  hapSummary,
                                            Chr = Chr,
                                            startPOS = startPOS,
                                            endPOS = endPOS,
                                            type = type,
                                            cex = cex,
                                            geneElement = geneElement)
            dev.off()        
          }
        )
        
      
      
        output$displayvarongenemodel <- renderPlot({
          geneHapR::displayVarOnGeneModel(gff = gff,hapSummary =  hapSummary,
                                          Chr = Chr,
                                          startPOS = startPOS,
                                          endPOS = endPOS,
                                          type = type,
                                          cex = cex,
                                          geneElement = geneElement)
        
        })#, width = input$img_width, height = input$img_height, res = input$img_res)
        message("ending plot vars")
    
 
    })

    # 
    # ========================================hapdistribution===========================#
    observeEvent(input$hapdistribution_legend,{
        if(input$hapdistribution_legend == "other"){
            show("hapdistribution_x")
            show("hapdistribution_y")
        } else {
            hide("hapdistribution_x")
            hide("hapdistribution_y")
        }
    })
    observeEvent(input$hapdistribution_region,{
        if(input$hapdistribution_region == "other"){
            show("hapdistribution_region_other")
        } else {
            hide("hapdistribution_region_other")
        }
    })
    observeEvent(input$hapdistribution,{
        message("begin")
        # library(mapdata)
        # library(maptools)
        # accinfo1 <- accinfo1()
        # if(input$hapdistribution_loncol %in% names(accinfo1))
        #   accinfo <- accinfo1 else
        accinfo <- accinfo()
        if(input$hapdistribution_legend == "other")
            legend <- c(input$hapdistribution_x, input$hapdistribution_y) else
                legend <- input$hapdistribution_legend
        if(input$hapdistribution_region == "other")
            database <- input$hapdistribution_region_other else
                database <- input$hapdistribution_region
        message("ploting")
        LON.col = input$hapdistribution_loncol
        LAT.col = input$hapdistribution_latcol

        hap <- hapre()
        output$hapdistribution <- renderPlot({
            if(input$hapdistribution_latcol == "none" ||
               input$hapdistribution_loncol == "none") {
                plot.new()
                title("Please choose latitude and longitude")
            } else
                geneHapR::hapDistribution(hap = hap, AccINFO = accinfo,
                                          LON.col = LON.col,
                                          LAT.col = LAT.col,
                                          hapNames = input$hapdistribution_hapnames,
                                          database = database,
                                          regions = ".",
                                          legend = legend,
                                          lwd.pie = input$hapdistribution_lwdpie,
                                          lwd = input$hapdistribution_lwd,
                                          cex.legend = input$hapdistribution_cexlegend,
                                          symbolSize = input$hapdistribution_symbolsize,
                                          xlim = input$hapdistribution_xlim,
                                          ylim = input$hapdistribution_ylim)
        }, width = input$img_width, height = input$img_height, res = input$img_res)
        message("end")

    })

    # hapVsPhenos
    hapvaspheno_plot <- reactiveValues()
    observeEvent(input$hapvaspheno,{
        result <- try(geneHapR::hapVsPheno(hap = hapre(), pheno = pheno(),
                                           mergeFigs = FALSE,
                                           minAcc = input$hapvspheno_minacc,
                                           phenoName = input$hapvspheno_phenoname,
                                           title = input$hapvspheno_title,
                                           hapPrefix = input$hapvspheno_prefix,
                                           angle = input$hapvspheno_angle,
                                           outlier.rm = input$hapvspheno_outlierm))
        if(inherits(result, "try-error")){
            output$hapvaspheno <- renderPlot({
                plot.new()
                title(as.character(result))
            })
        } else
            output$hapvaspheno <- renderPlot({
              hapvaspheno_plot$plot<-result$fig_Violin
              hapvaspheno_plot$plot
              })

    })
    output$download_hapvaspheno <- downloadHandler(
      filename = function() {
        "hapvaspheno.pdf"
      },
      content = function(file) {
        
        ggsave(file, hapvaspheno_plot$plot, width = input$down_width, height = input$down_height, units = "in")
        
      }
    )
    
    
    # LD_heatmap
    #LD_plot <- reactiveValues()
    observeEvent(input$plotLD_bt,{

           output$ui_LD <- renderUI({
        withSpinner(  plotOutput("plotLD", height = 1000)
                      )
      
      })


      title <- if(nchar(input$LD_heatmap_title) == 0) " " else
        input$LD_heatmap_title
      LDmeasure <- input$LD_heatmap_LDmeasure
      distances <- input$LD_heatmap_distance
      colorLegend <- input$LD_heatmap_colorLegend
        #addmap <- F#input$LD_heatmap_addmap
      text <- input$LD_heatmap_text
      
      # 
      # 
      Chr <- input$LD_heatmap_Chr
      start <- input$LD_heatmap_start
      end <- input$LD_heatmap_end
      #color <- input$LD_heatmap_clolor
      #if(color == "grey") color <- grDevices::grey.colors(40) else
      color <- NULL
      # start_color=input$st_color
      # mid_color=input$mid_color
      # end_color=input$end_color
      geneMapLocation <- input$LD_heatmap_maplocation
      map.height <- input$LD_heatmap_mapheight
      hapre <- hapre()
      gff <- gff()
      
      output$download_LD <- downloadHandler(
        filename = function() {
          "LD-heat.pdf"
        },
        content = function(file) {
          pdf(file, width = input$down_width, height = input$down_height)
              plot_LDheatmap_geneHapR(input,hapre, gff = gff, Chr = Chr,
                                  start = start, end = end,
                                  color = color,
                                  distances = distances,
                                  LDmeasure = LDmeasure,
                                  title = title,
                                  #add.map = addmap,
                                  map.height = map.height,
                                  geneMapLocation = geneMapLocation,
                                  colorLegend = colorLegend,
                                  text = text
          )
          dev.off()        
        }
      )
      #color = color,
      # start_color=start_color,
      # mid_color=mid_color,
      # end_color=end_color,
      output$plotLD <- renderPlot({
        
        plot_LDheatmap_geneHapR(input,hapre, gff = gff, Chr = Chr,
                       start = start, end = end,
                        color = color,
                        distances = distances,
                        LDmeasure = LDmeasure,
                       title = title,
                       #add.map = addmap,
                       map.height = map.height,
                        geneMapLocation = geneMapLocation,
                        colorLegend = colorLegend,
                        text = text
                       )
      })


      # if(addmap){
      #   print("true")
      #   Chr <- input$LD_heatmap_Chr
      #   start <- input$LD_heatmap_start
      #   end <- input$LD_heatmap_end
      #   color <- input$LD_heatmap_clolor
      #   if(color == "grey") color <- grDevices::grey.colors(40) else color <- NULL
      #   geneMapLocation <- input$LD_heatmap_maplocation
      #   map.height <- input$LD_heatmap_mapheight
      #   hapre <- hapre()
      #   gff <- gff()
      #   output$plotLD <- renderPlot({
      #     plot_LDheatmap(hapre, gff = gff, Chr = Chr,
      #                    start = start, end = end,
      #                    color = color,
      #                    distances = distances,
      #                    LDmeasure = LDmeasure,
      #                    title = title,
      #                    add.map = addmap,
      #                    map.height = map.height,
      #                    geneMapLocation = geneMapLocation,
      #                    colorLegend = colorLegend,
      #                    text = text)
      #   })
      # } else {
      # 
      #   color <- input$LD_heatmap_clolor
      #   if(color == "grey") color <- grDevices::grey.colors(40) else color <- NULL
      #   hapre <- hapre()
      #   output$plotLD <- renderPlot({
      #     print("false")
      #     plot_LDheatmap(hapre
      #                    # color = color,
      #                    # distances = distances,
      #                    # LDmeasure = LDmeasure,
      #                    # title = title,
      #                    # add.map = addmap,
      #                    # colorLegend = colorLegend,
      #                    # text = text
      #                    )
      #   })
      # }
    })
    
#=========================================tabPanel_preprocess============================#
    # Large_Vcf <- reactiveVal()
    # observeEvent(eventExpr = input$srcFile_im, {
    #   Large_Vcf<- import_vcf(input$lvcf_filter_vcfin$datapath)
    #   chrs <- as.list(unique(Large_Vcf@fix[,1]))
    #   names(chrs) <- as.vector(chrs)
    #   updateSelectInput(session = getDefaultReactiveDomain(),inputId="lvcf_filter_chr",
    #                     choices = chrs,
    #                     selected = chrs[[1]])
    #   
    #       start <- min(as.numeric(Large_Vcf@fix[,2]))
    #       end <- max(as.numeric(Large_Vcf@fix[,2]))
    #       updateNumericInput(session,"lvcf_filter_start",
    #                          value = start,
    #                          min = start,
    #                          max = end)
    #       updateNumericInput(session,"lvcf_filter_end",
    #                          value = end,
    #                          min = start,
    #                          max = end)
    # })
    # observeEvent(eventExpr = input$lvcf_filter_bt, {
    #   
    #     # a) Extract a single gene/range from a large vcf
    #     # New VCF file will be saved to disk
    #     setwd("E:/RWork/snpDB_v3/extdata/")
    #     #updateNumericInput("lvcf_filter_start",value = )
    #     filterLargeVCF(VCFin = input$lvcf_filter_vcfin$datapath,
    #                   
    #                    VCFout = input$lvcf_filter_vcfout,
    #                    Chr = input$lvcf_filter_chr,
    #                    POS = c(input$lvcf_filter_start,input$lvcf_filter_end),
    #                    override = TRUE)
    # })
















    is.surfix <- function(x, surfix) {
        tolower(x)
        return(any(endsWith(x, surfix)))

    }
    ffilter <- matrix(ncol = 2, byrow = TRUE,
                      c("fasta \u6587\u4EF6 (*.fa, *.fasta)","*.fa;*.fatsa",
                        "vcf \u6587\u4EF6 (*.vcf, *.vcf.gz)", "*.vcf;*.vcf.gz",
                        "p.link \u6587\u4EF6 (ped & map)","*.map;*.ped",
                        "hapmap \u6587\u4EF6 (*.hmp)"," *.hmp",
                        "CSV\u9017\u53F7\u5206\u9694\u7684\u6587\u672C\u6587\u4EF6 (*.csv)", "*.csv",
                        "\u5236\u8868\u7B26\u5206\u9694\u7684\u6587\u672C\u6587\u4EF6 (*.txt)","*.txt",
                        "hapResult ( *.hapResult, *.txt)", "*.hapResult;*.txt",
                        "hapSummary ( *.hapSummary, *.txt)", "*.hapSummary;*.txt",
                        "GFF \u6587\u4EF6 (*.gff, *.gff3)", "*.gff;*.gff3",
                        "BED \u6587\u4EF6 (*.bed, *.bed4, *.bed6)","*.bed;*.bed4;*.bed6",
                        # 所有文件
                        "\u6240\u6709\u6587\u4EF6 (*.*)","*.*"))
    row.names(ffilter) <- c("fa", "vcf", "plink","hmp",
                            "csv","txt","hapResult","hapSummary",
                            "gff","bed","all")


}
