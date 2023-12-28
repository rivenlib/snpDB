var_Reg_server<-function(input,output)
{
  
  #更新selectInput
  observe({
    updateSelectizeInput(
      inputId = "Chromsome",
      choices = unique(df2$CHROM),
      server = TRUE
    )
  })
  
  
  # 存储筛选结果
  filtered_data2_1 <- reactiveVal()
  
  # 当按下“提交”按钮时，筛选表格并更新结果
  observeEvent(eventExpr = input$submit_btn2_1, {
    
    output$ui2_1<-renderUI({
      removeUI(selector = "#default2_1")
      downloadButton("download2_1", "Download")
    })
    
    #filter筛选结果出错需检查updateSelectizeInput函数是否设置了selected选项
    if((is.null(input$SP) || input$SP == "")&&
       (is.null(input$EP) || input$EP == ""))
    {
      
      filtered_data2_1(df2%>%
                         filter(.data[['CHROM']]==input$Chromsome))
    }
    else if((!is.null(input$SP) || input$SP != "")&&
            (is.null(input$EP) || input$EP == ""))
    {
      
      filtered_data2_1(df2 %>%
                         filter(.data[['CHROM']]==input$Chromsome) %>%
                         filter(.data[['POS']] >= as.numeric(input$SP))) 
    }
    else if ((is.null(input$SP) || input$SP == "")&&
             (!is.null(input$EP) || input$EP != ""))
    {
      filtered_data2_1(df2 %>%
                         filter(.data[['CHROM']]==input$Chromsome)%>%
                         filter(.data[['POS']] <= as.numeric(input$EP)))
    }
    else{
      filtered_data2_1(df2 %>%
                         filter(.data[['CHROM']]==input$Chromsome) %>%
                         
                         filter(.data[['POS']] >= as.numeric(input$SP)) %>%
                         filter(.data[['POS']] <= as.numeric(input$EP)))
    }
  })
  
  # 显示结果
  output$results2_1 <- renderDataTable({
    filtered_data2_1()
  },options = list(scrollX = TRUE, scrollY = "500px"))
  output$download2_1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data2_1(), file)
    }
  )
  
  
}