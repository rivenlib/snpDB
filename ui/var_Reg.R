ui<-tabItem(
  tabName = "tab2_1",
  fluidPage(
    titlePanel("Search for Variations by Region"),
    br(),
    fluidRow(
      column(
        width = 3,
        selectizeInput(
          "Chromsome",
          "Chromsome",
          choices = NULL,
          selected = NULL
        )
      ),
      column(
        width = 3,
        textInput("SP","Start position:"),
        
      ),
      column(
        width = 3,
        textInput("EP","End position:"),
        
      ),
      column(
        width = 3,
        div(actionButton(
          "submit_btn2_1",
          "Submit",
          class = "btn-primary"
        ),class="my-column")
      )
    ),
    br(),
    
    tableOutput("default2_1"),
    dataTableOutput("results2_1"),
    uiOutput("ui2_1")
    
  )
)