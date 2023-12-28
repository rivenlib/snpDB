ui<-tabItem(
	tabName="tab5",
	fluidPage(
	
    fluidRow(
      br(),
            column(width = 4,
                   selectizeInput(
                     "SltDate",
                     "Select Dataset",
                     choices = c("rice","wheat"),
                     selected = "wheat"
                   )),
	     column(
        	width = 3,
        	selectizeInput(
        	  "SG",
        	  "Search Gene",
        	  choices = NULL,
        	  selected = NULL,
        	  multiple = TRUE
        	  
        	)
        
      		),
      ),
    br(),
  fluidRow(
  	column(width=3,
		plotOutput("heatmapPlot"),
	),
	column(width=9,
		plotOutput("earthmap")
	)
  )
	)
)
