

options(shiny.sanitize.errors = FALSE)
header <- dashboardHeader(title = "snp Dashboard")

sidebar<-dashboardSidebar(
  sidebarMenu(
    id = "sideBar",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem(
      "Genomic Variation", tabName = "tab2",
      menuItem("Variations by Region", tabName = "tab2_1", icon = icon("calendar-plus")),
      menuItem(
        "Variations in Gene", tabName = "tab2_2", icon = icon("keyboard")
      ),
      menuItem(
        "Variation by varID",
        tabName = "tab2_3",
        icon = icon("search")
      ),
      menuItem(
          "Haplotype",
          tabName = "tab2_4",
  	  icon=icon("palette")
      )
      #maenuItem(
      #  "gwas",
      #  tabName = "tab2_5",
      #  icon=icon("brush")
      #)

    ),
    menuItem(
      "Transcription factor",
      tabName = "tab3"
    ),
    menuItem(
      "GWAS",
      tabName = "tab4"
    ),
    menuItem(
      "plantmap",
      tabName = "tab5"
    )
  )
)

body<-dashboardBody(

  tabItems(
    source("./ui/home.R")$value,
    source("./ui/var_Reg.R")$value,
    source("./ui/var_Gene.R")$value,
    source("./ui/var_ID.R")$value,
    source("./ui/haplotype.R")$value,
    source("./ui/gwas.R")$value,
    source("./ui/TF.R")$value,
    source("./ui/mvp.R")$value,
    source("./ui/plantmap.R")$value
  )
)

fluidPage(theme = shinytheme("lumen"),
  dashboardPage(
    header,
    sidebar,
    body

  )
)
