
tabPanel_import <- tabPanel(
  "Import data",
  fluidPage(
    br(),
    fluidRow(column(width = 12,
                    fileInput("gwas_file", label = NULL)
    ),
    #column(width=2,
    #       actionButton("gwas_im",
    #                    #width = "95%",
    #                    label = "Import"))
    ),
    fluidRow(column(width = 12,
                    h4("import result"),
                    br(),
                    tableOutput("default2_5"),
                    ))
  )
  

)

tabPanel_Visualization<-tabPanel(
  "Visualization",
  # sidebarPanel(
  #   #theme = shinytheme("darkly"),
  #   width = 2,
  #   h4("plot  Options"),
  #   # textInput("img_name", "File name")
  #   numericInput("plot_width", "download width", value = 12,min = 1,step = 1),
  #   numericInput("plot_height", "download height", value = 9,min = 1,step = 1),
  # ),

  
  fluidPage(
    br(),
    fluidRow(
      column(width = 4,
             selectInput("CMplot_color", label = "fill colr",
                         choices = c("skyblue","red", "violet","blue","darkgreen", "green", "purple", "orangered","orange","yellow",
                                     "brown", "pink", "black", "white", "gray","cyan"),
                         selected = NULL,#c("#3E0A52", "#423D77","#3F678B","#468C8D", "#5FB47F", "#9FD55C","#F9E956"),
                         multiple = TRUE)),
      
      column(width = 4,
             textInput("plot_name", label = "Title",value = "illumilla_60K")),
    ),
    fluidRow(
      column(width = 4,
             selectInput("qq_color", label = "QQ colr",
                         choices = c("skyblue","red",  "violet","blue", "darkgreen", "green", "purple", "orangered","orange","yellow",
                                              "brown", "pink", "black", "white", "gray","cyan"),
                         multiple = F)),
      
      column(width = 4,
             selectInput("density_color", label = "density colr",
                         choices = c("skyblue","red", "grey", "violet","blue","darkgreen",  "green", "purple", "orangered","orange","yellow",
                                              "brown", "pink", "black", "white", "cyan"),
                                              selected = c("skyblue", "grey", "red"),
                         multiple = TRUE)),
    ),
    
    fluidRow(
      column(width = 4,
             numericInput("plot_width", "download width", value = 12,min = 1,step = 1)
             ),
      column(width = 4,
             numericInput("plot_height", "download height", value = 9,min = 1,step = 1)
      )
    ),
    fluidRow(
      column(width = 4,
             checkboxInput("manhattan_plot_threshold","show threshold",value = TRUE)
             )
    ),
    fluidRow(
      column(width=2,
             actionButton("CMplot_bt",
                          #width = "95%",
                          label = "PLOT IMG")),
      column(width=2,
             downloadButton("download_CMplot",
                          #width = "95%",
                          "DOWNLOAD"))
      
      
    ),
    br(),
    uiOutput("plot_ui")
  )
 

)

ui <- tabItem(
  tabName = "tab2_5",
  fluidPage(
    mainPanel(tabsetPanel(
      # Import data UI
      tabPanel_import,
      # Visualization
      tabPanel_Visualization
    ))
    
  )
)
  
