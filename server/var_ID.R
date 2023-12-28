var_ID_server<-function(input,output)
{
  
  
  observe({
    updateSelectizeInput(
      inputId = "filter_col",
      choices = names(df2),
      selected = names(df2)[1],
      server = TRUE
    )
  })
  
  # 存储筛选结果
  filtered_data2_3 <- reactiveVal()
  
  # 当按下“提交”按钮时，筛选表格并更新结果
  observeEvent(eventExpr = input$submit_btn2_3, {
    output$ui2_3<-renderUI({
      removeUI(selector = "#default2_3")
      downloadButton("download2_3", "Download")
    })
    if(is.null(input$filter_val)||input$filter_val=="")
    {
      filtered_data2_3(df2)
    }
    else{
      filtered_data2_3(df2 %>% filter(.data[[input$filter_col]] == input$filter_val))
    }
  })
  
  
  # 显示结果
  output$results2_3 <- renderDataTable({
    filtered_data2_3()
  },options = list(scrollX = TRUE, scrollY = "500px"))
  
  output$download2_3 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data2_3(), file)
    }
  )
  
  
}