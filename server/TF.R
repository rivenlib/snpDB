TF_server<-function(input,output,session){


  # 存储筛选结果
  filtered_data3 <- reactiveVal()
  nodes<- reactiveVal()
  edgs<- reactiveVal()
  dgree<-data.frame()
  down<-reactiveVal()
  # 当按下“提交”按钮时，筛选表格并更新结果
  observeEvent(eventExpr = input$submit_btn3, {

    output$ui3 <- renderUI({
      removeUI(selector = "#default3")
      fluidRow(
        dataTableOutput("results3"),
        fluidRow(
          column(width = 2,
                 downloadButton("download3", "Download"),
          ),
          column(width = 2,
                 downloadButton("edgsdown", "edgs Download"),
          ),
          column(width = 2,
                 downloadButton("nodesdown", "nodes Download"),
          ),

          column(width = 2,offset = 2,
                 sliderInput(inputId = "threshold",
                             label = "Threshold:",
                             min = 1,
                             max = 50,
                             value = 1)
                 ),

          column(width = 2,

                 div(actionButton(
                   "plot_btn3",
                   "Plot",
                   class = "btn-primary"
                 ),class="my-column")

          )

        ),
        br(),
        br(),


      )
    })
    data<-df3[grep(input$tFs, df3[['TF_ID']], ignore.case = TRUE),]
    data<-data[grep(paste0(input$tFname, "$"), data[['col_amp']], ignore.case = TRUE),]
    data<-data[grep(input$annotation, data[['Effect']], ignore.case = TRUE),]
    data<-data[grep(input$tfSymbol1, data[['TF_Symbol1']], ignore.case = TRUE),]
    data<-data[grep(input$targetId, data[['targetID']], ignore.case = TRUE),]
    data<-data[grep(input$symbol, data[['targetSymbol']], ignore.case = TRUE),]
   
    if(is.null(input$key)||input$key=="")
    {
     
      filtered_data3(data)
    }else
    {
      
      filtered_data3(data %>% filter_all(any_vars(grepl(input$key, ., ignore.case = TRUE))))
    }

    # 提取边,
    edgs(
      data.frame(
        select(filtered_data3(), 'TF_ID', 'targetID')
        )
      )
    #write.table(edgs(),  sep = "\t", quote = F,row.names = FALSE)
    #结点
    nodes(
      data.frame(col1 = c(edgs()$TF_ID, edgs()$targetID), col2 = c(filtered_data3()$TF_Symbol1, filtered_data3()$targetSymbol))%>%
        distinct()
      #{rename(., name = col1, group = col2)}
    )
    #nodes(rename(nodes(), name = col1, group = col2))
    # write.table(nodes(),  sep = "\t", quote = F,row.names = FALSE)
    # 统计每个结点的出度
    dgree <- edgs() %>%
      count(targetID, name = "weight")


    edgs <- edgs() %>%
      mutate(weight = ifelse(targetID %in% dgree$targetID, dgree[match(targetID, dgree$targetID), "weight"], 0))
    down(edgs)
    # 更新滑动条的最大值和最小值
   # observe(
      updateSliderInput(session, "threshold",
                        min = min(dgree$weight),
                        max = max(dgree$weight),
                        value =50
     # )
    )


  })


  # 当按下“plot”按钮时，筛选表格并更新结果
  observeEvent(eventExpr = input$plot_btn3, {


    output$plot_ui3<-renderUI({
      fluidRow(
        column(
          width = 6,
          withSpinner(
            plotOutput("plot3_2",height="500px"),
          )

        ),
        column(
          width = 6,
          withSpinner(
            forceNetworkOutput("network",height="500px"),
          )
        )
      )
    })

    output$network <- renderForceNetwork({
      network_graph(input, down())
    })

    output$plot3_2 <- renderPlot({
      bar_chart(filtered_data3(), edgs())
    })


  })

  # 显示结果
  output$results3 <- renderDataTable({
    filtered_data3()
  },options = list(scrollX = TRUE, scrollY = "500px"),#filter = list(input$search)
  )


  output$download3 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(filtered_data3(), file, sep = "\t", quote = F,row.names = FALSE)
    }
  )


  output$edgsdown <- downloadHandler(
    filename = function() {
      paste("edgs-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(down(), file, sep = "\t",quote = F, row.names = FALSE)
    }
  )

  output$nodesdown <- downloadHandler(
    filename = function() {
      paste("nodes-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      write.table(nodes, file, sep = "\t",quote = F, row.names = FALSE)
    }
  )



}
