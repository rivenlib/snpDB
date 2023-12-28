tabPanel_import4 <- tabPanel(
  "data import",
  fluidPage(
    fluidRow(
      br(),
            column(width = 4,
                   selectizeInput(
                     "species",
                     NULL,
                     choices = c("rice","wheat"),
                     selected = "wheat"
                   )),
      ),
    fluidRow(
      column(width = 6,
             h4("phenotype file:"),
             fileInput("mvp_phe_file", label = NULL)
      ),
      column(width = 6,
             h4("import result"),
             uiOutput("phe_ui", height="500px"))
      ),
    br(),
    fluidRow(
      column(width=4,
             actionButton("start_GWAS_im",
                          #width = "95%",
                          label = "Start GWAS")),
      column(width = 6,
             textOutput("outpath_text"))
    )
  )
  
  
)

tabPanel_Visualization4<-tabPanel(
  "Visualization",
  fluidRow(
    column(width = 6,
           selectizeInput(
             "folder_select",
             "Select Task ID:",
             choices = NULL,
             options = list(
               search = TRUE
             )
             #selected = NULL
           )
           
           #uiOutput("folder_select_ui")
           ),
    column(width = 6,
           br(),
           downloadButton("downloadData5", "Download Data"))
  ),
  
  uiOutput("visual_ui")  
  
)

ui <- tabItem(
  tabName = "tab4",
  fluidPage(
    mainPanel(tabsetPanel(
     
      # Import data UI
      tabPanel_import4,
      # Visualization
      tabPanel_Visualization4
    ))
    
  )
)

