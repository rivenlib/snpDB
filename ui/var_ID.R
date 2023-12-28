ui<-tabItem(
  
  tabName = "tab2_3",
  fluidPage(
    
    titlePanel("Search for Variation information by Variation ID:"),
    br(),
    fluidRow(
      column(
        width = 4,
        selectizeInput(
          "filter_col", 
          "Select columns to filter",
          choices = NULL
        ),
      ),
      column(
        width = 4,
        textInput("filter_val", "Enter the value to filter"),
        
      ),
      
      column(
        width = 4,
        div(actionButton(
          "submit_btn2_3",
          "Submit",
          class = "btn-primary"
        ),class="my-column")
      )
    ),
    br(),
    dataTableOutput("results2_3"),
    tableOutput("default2_3"),
    uiOutput("ui2_3")
    
  )
  
)