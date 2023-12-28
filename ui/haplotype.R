import_vcf <- tabPanel(
    "VCF file",
    tabPanelBody(
        "IMPORT_VCF",
        br(),
        fluidRow(column(width = 6,
                        fileInput("vcf_file", label = NULL)
                        ),
                 column(width=2,
                        actionButton("vcf_im",
                                     #width = "95%",
                                     label = "Import"))
        ),
        fluidRow(column(width = 12,
                        h4("import result"),
                        verbatimTextOutput("vcf_info")))))

import_genomic <- tabPanel(
    "Genotypic Data",
        import_vcf
)


import_annotation <- tabPanel(
  "Annotation",
  tabPanelBody(
    "import annotations",
    br(),
    column(width = 12,
           fluidRow(
             column(width = 2,
                    
                    radioButtons("gff_format", label = "Format",
                                 choiceNames = c("GFF","BED"),
                                 choiceValues = c("GFF","BED"))
                    
             ),
             column(width = 4,
                    fileInput("gff_in", label = NULL)
             ),
             column(width = 2,
                    actionButton("gff_im",
                                 #width = "100%",
                                 label = "Import")
             )
           )),
    
    fluidRow(column(width = 12,
                    h4("import result"),
                    verbatimTextOutput("annotation_data"))))
)
import_pheno <- tabPanel(
  "Phenotypic Data",
  tabPanelBody(
    "Import Phenotypic Data",
    br(),
    column(width = 12,
           fluidRow(
             column(width = 2,
                    radioButtons("pheno_format",label = "format",
                                 choiceNames = c("txt","csv"),
                                 choiceValues = c("txt","csv")),
             ),
             column(width = 4,
                    fileInput("pheno_in", label = NULL),
             ),
             column(width = 2,
                    actionButton("pheno_im",
                                 #width = "40%",
                                 label = "Import")
             )
             
           ),
           fluidRow(
             checkboxInput("pheno_showdata",
                           label = "Show pheno data after import",
                           value = FALSE)
             
           )
    ),
    
    
    fluidRow(column(width = 12,
                    htmlOutput("pheno_info"))),
    fluidRow(column(width = 12,
                    tableOutput("pheno_data"))))
)



import_infos <- tabPanel(
    "Accession Information Data",
    tabPanelBody(
        "import accession information data",
        br(),
        column(width = 12,
               fluidRow(
                 column(width =2,
                      
                        radioButtons("accinfo_format",label = "Format",
                                     choiceNames = c("txt","csv"),
                                     choiceValues = c("txt","csv")),
                        checkboxInput("accinfo_showdata",
                                      label = "Show accession information data after import",
                                      value = FALSE)
                 ),
                   column(width = 4,
                          fileInput("accinfo_in", label = NULL)
                          ),
                 column(width =2,
                        actionButton("accinfo_im",
                                     #width = "100%",
                                     label = "Import"))),
               fluidRow(
                 htmlOutput("accinfo_info")
               ),
               tableOutput("accinfo_data")
               ),
    )
)



tabPanel_import <- tabPanel(
    "Import data",
    navlistPanel(widths = c(4, 8),

      import_genomic,
      import_annotation,
      import_pheno,
      import_infos
    )


)
tabPanel_filter <-  tabPanel(
    "Filter Genotypic Data",
    br(),
    tabPanelBody(
        "filter_vcf",
        fluidRow(column(width = 3,
                        radioButtons("filter_mode", label = "Filter variants by",
                                     selected = "POS",
                                     choiceNames = c("Position", "Type",
                                                     "Both of above","Do not filter"),
                                     choiceValues = c("POS","type","both","none")),
                        checkboxGroupInput("filter_type",
                                           label = "Type",
                                           choiceNames = c("CDS", "UTR"),
                                           choiceValues = c("CDS", "UTR"),selected = "CDS")),
                 column(width = 3,
                        selectInput("filter_chr", label = "Chromosome",
                                    choices = list()),
                        numericInput("filter_start", label = "Start", value = 0),
                        numericInput("filter_end", label = "End",value = 0))),
        fluidRow(column(width = 6,
                        actionButton("filter_gentype_info", "Filter")
                        )),
        br(),
        fluidRow(column(width = 12,
                        h4("filter result"),
                        verbatimTextOutput("filter_info")))
        ))

# #_________________________________________________

tabPanel_Haplotyping <- tabPanel( # Do haplotyping Start
    "Haplotype",
    fluidPage(
        fluidRow(
            # column(width = 2,
            #        br(),
            #        radioButtons("hapresult_source",
            #                     label = "Genotypic Data Format",
            #                     choiceNames = c("vcf", "p.link", "Fasta", "Table", "Hapmap"),
            #                     choiceValues = c("vcf", "p.link", "Fasta", "Table", "Hapmap"),
            #                     selected = "vcf")),
            column(width = 6,
                   h3("Haplotype Identification"),
                   textInput("happrefix", "Prefix of haplotype names", value = "H"),
                   checkboxInput("hyb_remove", "Remove heterozygote individuals", value = TRUE),
                   checkboxInput("na_drop", "Remove genotype unknown individuals", value = TRUE),
                   fluidRow(column(width = 2,
                                   actionButton("hapresult_bt", "1. Identify Haplotype")),
                            column(width = 2,
                                   actionButton("hapsummary_bt", "2. Summary")),
			    column(width = 2,
                                   downloadButton("hapsummary_DL_bt", "DownLoad")),
			    )),
        ),
            br(),
            fluidRow(column(width = 6,
                            h4("Haplotype result"),
                            verbatimTextOutput("hapresult")),
                     column(width = 6,
				    h4("Summary of haplotype result"),
                            verbatimTextOutput("hapsummary")
		     )

                           # h4("Summary of haplotype result"),
                           # verbatimTextOutput("hapsummary"))
	    ),
        
    )
)
#================================tabPanel_Visualization==================================

Hapotype_Table <- tabPanel(
    "Hapotype Table",
    br(),
    fluidRow(
        column(width = 6,
               textInput("plothaptable_prefix","Prefix of Haplotype Names",value = "H"),
               textInput("plothaptable_title", "Title", value = " "),
               textInput("plothaptable_genename", "Gene name", value = ""),
               sliderInput("plothaptable_angle", label = "Angle of x-axis label",
                           min = 0, max = 90, step = 45, value = 0)),


        column(width = 6,
               selectInput("plothaptable_infotag", "Tag name in INFO", choices = list()),
               textInput("plothaptable_tagname", "Tag name in IMG", value = ""),
               textInput("plothaptable_tagsplit", "Tag split", value = "|"),
               numericInput("plothaptable_tagfield", "Tag field", value = "1"))),
    fluidRow(
      column(width = 6,
             actionButton("plothaptable", "Plot IMG")
             ),
      column(width = 6,
             downloadButton("download_haptable", "DOWNLOAD")
             )
   
    ),
    br(),
     plotOutput("plothaptable")#, height = 750))
    )


Dispaly_Variants_on_Gene_Model <- tabPanel(
    "Dispaly Variants on Gene Model",
    br(),
    fluidRow(
        column(width = 6,
               selectInput("displayVarOnGeneModel_chromosome","Chromosome",choices = list()),
               numericInput("displayVarOnGeneModel_start", "Start", value = 0),
               numericInput("displayVarOnGeneModel_end", "End", value = 0)),
        column(width = 6,
               selectInput("displayVarOnGeneModel_type", label = "Variants type",
                           choices = list("circle" = "circle", "pie" = "pie", "pin" = "pin", "flag" = "flag"),
                           selected = "pin"),
               sliderInput("displayVarOnGeneModel_cex",
                           label = "Size of label", min = 0, max = 3, value = 0.8, step = 0.05),
               checkboxGroupInput("geneElement",label = "Display elements", inline = TRUE,
                                  choiceNames = c("CDS","UTR"), choiceValues = c("CDS","UTR")))),
    br(),
    fluidRow(
      column(width = 6,
             actionButton("displayvar", "Plot IMG")
      ),
      column(width = 6,
             downloadButton("download_var", "DOWNLOAD")
      )
      
    ),
    br(),
    plotOutput("displayvarongenemodel")#, height = "800px"))

)
haploNet <- tabPanel(
    "Haplotype Network",
    br(),
    fluidRow(
        column(width = 4,
               selectInput("plothapnet_scale", label = "Scale",selected = "log2",
                           choices = list("none" = 1, "log10" = "log10", "log2" = "log2"))),
        column(width = 4,
               selectInput("plothapnet_showmutation", label = "Mutation symbol",selected = "line",
                           choices = list("none" = 0, "line" = "1",
                                          "dot" = "2", "number of mutantions" = 3))),
        column(width = 4,
               selectInput("plothapnet_group", "Group by", choices = list("none"="none")))),
    fluidRow(column(width = 6,
                    numericInput("plothapnet_collink", "Link color", value = 1)),
             column(width = 6,
                    numericInput("plothapnat_linkwidth", "Line width", value = 1))),

    fluidRow(
        column(width = 6,
               sliderInput("plothapnet_cex",
                           label = "Size of label", min = 0, max = 3, value = 0.8, step = 0.05)),
        column(width = 6,
               sliderInput("plothapnet_cexlegend",
                           label = "Size of legend", min = 0, max = 3, value = 0.8, step = 0.05))),
    #fluidRow(
    #    column(width = 6,
    #           sliderInput("plothapnet_xlim", "X limit range",
    #                       min = -200, max = 200, value =c(-1,1))
    #    ),
    #    column(width = 6,
    #           sliderInput("plothapnet_ylim", "Y limit range",
    #                       min = -200, max = 200, value =c(-1,1))
    #    )),
    h4("Legend options"),
    fluidRow(
        column(width = 6,
               checkboxInput("plothapnet_legend_size", "Show size legend", value = TRUE)),
        column(width = 6,
               checkboxInput("plothapnet_legend_color", "Show color legend", value = TRUE))),
    fluidRow(column(width = 6,
                    sliderInput("plothapnet_x", "x", min = -200, max = 200, value = 70)),
             column(width = 6,
                    sliderInput("plothapnet_y", "y", min = -200, max = 200, value = 25))),
    br(),

    fluidRow(
      column(width = 6,
             actionButton("plothaplonet_bt", "Plot IMG")
      ),
      column(width = 6,
             downloadButton("download_Network", "DOWNLOAD")
      )
      
    ),
    
    checkboxInput("plothapnet_labels", "Display haplotype names", value = TRUE),

    br(),
    ( plotOutput("plothaplonet"))
   ,

)

#===============================Geography distribution of main haplotypes===============#
Geo_distribution <- tabPanel(
    "Geo-distribution",
    br(),
    fluidRow(
        column(width = 6,
               selectInput("hapdistribution_loncol","Column name of longitude",
                           choices = list("none" = "none")),
               numericInput("hapdistribution_symbolsize", "Synbol size",
                            step = 0.1, value = 1, min = 0, max = 10),
               numericInput("hapdistribution_lwd", "Line width of map", value = 1),
               selectInput("hapdistribution_region", "Region", selected = "china",
                           choices = list("china" = "china",
                                          "world" = "world",
                                          "other" = "other")),
               hidden(
                   textInput("hapdistribution_region_other", label = NULL)),
        ),
        column(width = 6,
               selectInput("hapdistribution_latcol","Column name of latitude",
                           choices = list("none" = "none")),
               numericInput("hapdistribution_lwdpie", "Line width of pie", value = 0.5),
               numericInput("hapdistribution_cexlegend", "Size of legend", value = 0.8),
               selectInput("hapdistribution_legend", "Legend position",
                           choices = list("none" = FALSE, 'left' = 'left',
                                          'right' = 'right', 'top' = 'top', 'bottom' ='bottom',
                                          'topleft' = 'topleft', 'topright' = 'topright',
                                          'bottomright' = 'bottmoright', 'bottomleft' = 'bottomleft',
                                          "other" = "other")),
               hidden(
                   numericInput("hapdistribution_x", "x", step = 1, width = 300, value = -170),
                   numericInput("hapdistribution_y", "y", step = 1, width = 200, value = -60)))),
    fluidRow(
        column(width = 6,
               sliderInput("hapdistribution_xlim", "Longitude range",
                           min = -180, max = 180, value =c(-180,180)),

        ),
        column(width = 6,
               sliderInput("hapdistribution_ylim", "Latitude range",
                           min = -90, max = 90, value =c(-90,90)))),
    checkboxGroupInput("hapdistribution_hapnames",
                       "Display haplotypes",inline = TRUE,
                       choiceNames = c("H001", "H002", "H003"),
                       choiceValues = c("H001", "H002", "H003"),
                       selected = c("H001", "H002", "H003")),
    actionButton("hapdistribution", "Plot IMG", width = "85%"),
    plotOutput("hapdistribution")
)
Phenotype_Comparison <- tabPanel(
    "Phenotype Comparison",
    br(),
    fluidRow(
        column(width = 6,
               selectInput("hapvspheno_phenoname", "Pheno name",
                           choices = list("first pheno" = "1", "second pheno" = 2),
                           selected = "2"),
               textInput("hapvspheno_prefix", "Prefix of haplotype names", value = "H"),
               textInput("hapvspheno_title", "Title", value = "")),
        column(width = 6,
               numericInput("hapvspheno_minacc", "Minimum number of accession", value = 5),
               numericInput("hapvspheno_angle", "Angle of x labels", value = 0))),
    fluidRow(
      column(width = 6,
             actionButton("hapvaspheno", "Plot IMG")
      ),
      column(width = 6,
             downloadButton("download_hapvaspheno", "DOWNLOAD")
      )
      
    ),

    checkboxInput("hapvspheno_outlierm", "Remove outlier", value = TRUE),
    #
    # h4("Custom comparisons"),
    # fluidRow(
    #   column(width = 6,
    #          actionButton("hapvspheno_addcompare", "Add"),
    #          radioButtons("hapvspheno_hap1", "",
    #                       choiceNames = c("H001","H002"),
    #                       choiceValues = c("H001","H002"))),
    #   column(width = 6,
    #          actionButton("hapvspheno_addcompare", "Clear"),
    #          radioButtons("hapvspheno_hap2", "",
    #                       choiceNames = c("H001","H002"),
    #                       choiceValues = c("H001","H002")))),
    br(),
    (plotOutput("hapvaspheno"))
    ,
)
plot_LDheatmap <- tabPanel(
    "LD Heatmap",
    br(),
    fluidRow(
        column(width = 6,
               textInput("LD_heatmap_geneID", "Gene ID", " "),
               selectInput("LD_heatmap_distance","Distance type",
                           choices = list("physical" = "physical",
                                          "genetic" = "genetic"),
                           selected = "physical"),
               textInput("LD_heatmap_title", "Title", value = " "),
               sliderInput("LD_heatmap_mapheight", "Map height",
                           min = 0.0025, max = 0.1,
                           step = 0.0025,
                           value = 0.02)),
        column(width = 6,
               fluidRow(
                   column(
                       width = 4,
                       selectInput("LD_heatmap_Chr","Chromosome",
                                   choices = list("none" = "none"))
                   ),
                   column(
                       width = 4,
                       numericInput("LD_heatmap_start",
                                    "Start", step = 500,
                                    value = 1)),
                   column(
                       width = 4,
                       numericInput("LD_heatmap_end",
                                    "End", step = 500,
                                    value = 1)
                   )),
               fluidRow(
                        column(width=4,
                               selectInput("start_color", "start color:", choices ="" )
                               ),
                        column(width=4,
                               selectInput("mid_color", "mid color:", choices = "")),
                        column(width=4,
                               selectInput("end_color", "end color:", choices = ""))

               ),
               # selectInput("LD_heatmap_clolor", "Color",
               #             choices = list("whiteyellowred"="whiteyellowred",
               #                            "grey" = "grey")),
                                          
              selectInput("LD_heatmap_LDmeasure","LDmeasure",
                          choices = list("allelic correlation r^2" = "r",
                                         "Lewontin's |D'|" = "D'"),
                          selected = "physical"),
               
               sliderInput("LD_heatmap_maplocation", "Map location",
                           min = 0, max = 1, value = 0.15, step = 0.025))),

    fluidRow(
      column(width = 6,
             actionButton("plotLD_bt", "Plot IMG")
      ),
      column(width = 6,
             downloadButton("download_LD", "DOWNLOAD")
      )
      
    ),
    checkboxInput("LD_heatmap_colorLegend", "Show color legend", TRUE),
    #checkboxInput("LD_heatmap_addmap", "Show gene map", FALSE),
    checkboxInput("LD_heatmap_text", "Add text to cell", FALSE),

    br(),
     uiOutput("ui_LD")
                    
    )

tabPanel_Visualization <- tabPanel( # visualization start
    "Visualization",
    # siderbar UI start
    #sidebarPanel(
    #    width = 3,
    #    h3("Image  Options"),
    #    # textInput("img_name", "File name")
    #    numericInput("img_width", "width", value = 960,min = 1,step = 1),
    #    numericInput("img_height", "height", value = 720,min = 1,step = 1),
    #    numericInput("img_res", "resolution ", value = 72,min = 1,step = 1)),
    # siderbar UI End
    # siderbar UI start
    sidebarPanel(
      #theme = shinytheme("darkly"),
       width = 2,
       h4("download  Options"),
       # textInput("img_name", "File name")
       numericInput("down_width", "download width", value = 12,min = 1,step = 1),
       numericInput("down_height", "download height", value = 9,min = 1,step = 1),
      ),
    # siderbar UI End

    # plotHap table UI start
    mainPanel(tabsetPanel(
        Hapotype_Table,
        Phenotype_Comparison,
        haploNet,
        
        Dispaly_Variants_on_Gene_Model,
        Geo_distribution,
        plot_LDheatmap
    ))
) # visualization End
#===========================tabPanel_preprocess===============================#
tabPanel_preprocess<-tabPanel("Extract Large VCF",
                                   tabPanelBody(
                                       "filter_large_vcf",
                                       br(),
                                       fluidRow(
                                           column(width = 6,
                                                  fileInput("lvcf_filter_vcfin", label = "Input vcf file")#,value = "E:/RWork/snpDB_v3/extdata/var.vcf")
                                                  ),
                                           column(width = 2,
                                                  actionButton("srcFile_im", "import", align = "center"))
                                           ),
                                       fluidRow(
                                           column(width = 6,

                                                  textInput("lvcf_filter_vcfout", label = "Output vcf file",value  = "E:/RWork/snpDB_v3/extdata/newvar.vcf")
                                           )
                                       ),

                                       br(),
                                       fluidRow(column(width = 2,
                                                       selectInput("lvcf_filter_chr", label = "Chromosome",choices = list())),
                                                column(width = 4,
                                                       
                                                       
                                                       numericInput("lvcf_filter_start", label = "Start", value = 0),
                                                       numericInput("lvcf_filter_end", label = "End", value = 0))),
                                       actionButton("lvcf_filter_bt", "Extract From Large VCF", align = "center")))




ui <- tabItem(
    tabName = "tab2_4",
    navbarPage(
        title = " ",
        # preprocess UI
        #tabPanel_preprocess,
        # Import data UI
        tabPanel_import,
        # filter ui
        tabPanel_filter,
        # Haplotype Identification
        tabPanel_Haplotyping,

        # Visualization
        tabPanel_Visualization
    )
)
