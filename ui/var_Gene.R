
ui<-tabItem(
  source("./ui/submit.R")$value,
  
  tabName = "tab2_2",
  fluidPage(
    titlePanel("Search for Variations in Gene"),
    #h5("             (You Must Input GenId,Upstream,Downstream)"),
    #br(),
    fluidRow(
      column(
        width = 2,
        textInput(
          "GSN",
          "Gene ID/Symbol/Name",
          value = ""   #"LOC_Os01g01100"
        )
      ),
      column(
        
        width = 2,
        selectizeInput(
          "EffectMSU",
          "EffectMSU",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
          
        )
      ),
      column(
        width = 2,
        selectizeInput(
          "EffectRAP",
          "EffectRAP",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
          
        )
      ),
      column(
        width = 2,
        textInput("Upstream", "Upstream (kb,  must <= 10 kb)"),
      ),
      column(
        width = 2,
        textInput("Downstream", "Downstream (kb,  must <= 10 kb)"),
        
      ),
      column(
        width = 2,
        div(actionButton(
          "submit_btn2_2",
          "Submit",
          class = "btn-primary"
        ),class="my-column")
      )
    ),
    br(),
    dataTableOutput("results2_2"),
    tableOutput("default2_2"),
    uiOutput("ui2_2"),
  )
)