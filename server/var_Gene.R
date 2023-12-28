var_Gene_server<-function(input,output){
  
  
  observe({
    updateSelectizeInput(
      inputId = "EffectMSU",
      choices = unique(df2$EffectMSU),
      server = TRUE
    )
  })
  
  observe({
    updateSelectizeInput(
      inputId = "EffectRAP",
      choices = unique(df2$EffectRAP),
      server = TRUE
    )
  })
  
  # 存储筛选结果
  filtered_data2_2 <- reactiveVal()
  
  
  observeEvent(eventExpr = input$submit_btn2_2, {
    
    output$ui2_2 <- renderUI({
      removeUI(selector = "#default2_2")
      downloadButton("download2_2", "Download")
    })
    if(is.null(input$GSN)||input$GSN=="")
    {
      filtered_data2_2(df2)
    }
    else
    {
    row_num <- which(df2_2$GeneID == input$GSN)
    #df2<-df2[grep(input$GSN, df2[['TranscriptMSU']], ignore.case = TRUE),]
    
    if(df2_2[row_num,"Strand"]==1)
    {
      left<-df2_2[row_num,"Start"] - as.numeric(input$Upstream)*1000
      right<-df2_2[row_num,"End"] + as.numeric(input$Downstream)*1000
      left<-unlist(left)
      left<-as.numeric(left)
      right<-unlist(right)
      right<-as.numeric(right)

      if ((is.null(input$EffectMSU) || input$EffectMSU[1] == "")
          &&(is.null(input$EffectRAP) || input$EffectRAP[1] == ""))
      {
        
        filtered_data2_2(df2 %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))
      }

      else if (is.null(input$EffectMSU) || input$EffectMSU[1] == "")
      {
        filtered_data2_2(df2 %>%
                           filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))
      }
      else if (is.null(input$EffectRAP) || input$EffectRAP[1] == "")
      {
        filtered_data2_2(df2 %>%
                           filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))

      }
      else {
        filtered_data2_2(df2 %>%
                           filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                           filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))
      }


    } else if(df2_2[row_num,"Strand"]==0) {
      left<-df2_2[row_num,"Start"] - as.numeric(input$Downstream)*1000
      right<-df2_2[row_num,"End"] + as.numeric(input$Upstream)*1000

      left<-unlist(left)
      left<-as.numeric(left)
      right<-unlist(right)
      right<-as.numeric(right)
      if ((is.null(input$EffectMSU) || input$EffectMSU[1] == "")
          &&(is.null(input$EffectRAP) || input$EffectRAP[1] == "")
      )
      {

        filtered_data2_2(df2 %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))
      }
      else if (is.null(input$EffectMSU) || input$EffectMSU[1] == "")
      {
        filtered_data2_2(df2 %>%
                           filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))
      }
      else if (is.null(input$EffectRAP) || input$EffectRAP[1] == "")
      {
        filtered_data2_2(df2 %>%
                           filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))

      }else
      {
        filtered_data2_2(df2 %>%
                           filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                           filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                           filter(.data[['POS']] >= left) %>%
                           filter(.data[['POS']] <= right))
      }

    }
    }
    
  })
  
  # 显示结果
  output$results2_2 <- renderDataTable({
    filtered_data2_2()
  },options = list(scrollX = TRUE, scrollY = "500px"))
  
  output$download2_2 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data2_2(), file)
    }
  )
  
  
  
}