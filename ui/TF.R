library(shinydashboard)
#Transcription Factors
ui<-tabItem(
  tabName = "tab3",
  fluidPage(
    titlePanel("Search for Regulation information by TFs:"),
    br(),
    fluidRow(
      column(
        
        width = 2,
        textInput(
          "tFs",
          "TF_ID :",
          #value = "AT1G"
        )
      ),
      column(
        width = 2,
        textInput(
          "tFname",
          "col_amp:",
          #value = "SMB1"
        )
      ),
      column(
        
        width = 2,
        textInput
        (
          "tfSymbol1",
          "TF_Symbol:",
          #value = "Promoter"
        )

      ),
      column(        
        width = 2,
        textInput
        (
          "annotation",
          "Effect:",
          #value = "Promoter"
        ))
      ),
    fluidRow(
      column(
        width = 2,
        textInput("targetId", "TargetID:")#,value = "AT3G"),
      ),
      
      column(
        width = 2,
        textInput("symbol", "TargetSymbol:"),
        
        
      ),
      column(
        width = 2,
        textInput("key", "Key:"),
        
      ),
      column(
        width = 2,
        
         div(actionButton(
           "submit_btn3",
           "Submit",
           class = "btn-primary"
         ),class="my-column")
    )
    
      
      
      
    ),

    br(),
    
    tableOutput("default3"),
    uiOutput("ui3"),
    uiOutput("plot_ui3")
    
  )
)
