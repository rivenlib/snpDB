plantmap_server <- function(input, output, session) {
  data <- read.table("./extdata/roi.txt", head = TRUE)
  first_row <- data[1, ]
  observe({
    updateSelectizeInput(
      inputId = "SG",
      choices = unique(names(data)[-1]),
      server = TRUE
    )
  })
  
  
  output$heatmapPlot <- renderPlot({
    new.ggPlantmap <- XML.to.ggPlantmap("./extdata/roi.xml")
    new.express<-read.table("./extdata/roi.txt",head=T)
    ggPlantmap.merge(new.ggPlantmap,new.express,"ROI.name")
    quant.data=ggPlantmap.merge(new.ggPlantmap,new.express,"ROI.name")
    if(is.null(input$SG)||input$SG=="")
    {
      
    }
    else
    {
      gen_name<-input$SG
      print(gen_name)
      id=colnames(quant.data)[which(names(quant.data) == input$SG)]
      
      ggPlantmap.heatmap(quant.data,get(id))
    }
    
    
    
    
    # 加载数据
    #plot_list<-list()
    #for(i in 1:5)
    #{ 
    
    
    # 创建 ggplot2 绘图
    #plot_list[[i]]<-ggPlantmap.heatmap(quant.data, expression)
    #}
    #combined_plot <- patchwork::wrap_plots(plot_list,rcol=1,heights=rep(1,5))  # 或 cowplot::plot_grid(plotlist = plot_list)
    #combined_plot 
    
    
  })
}
