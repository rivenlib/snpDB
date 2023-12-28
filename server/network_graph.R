network_graph<-function(input,edgs){
  
  if(nrow(edgs) <1){
    showModal(modalDialog(
      title = "警告",
      HTML("过滤后没有边可显示！"),
      easyClose = TRUE,
      footer = NULL
    ))
  }
  else{
    
    edgs <- subset(edgs, weight >= input$threshold)
    #write.table(edgs, sep = "\t", row.names = T)
    
    #结点
    nodes<-data.frame(col1 = c(edgs$TF_ID, edgs$targetID))%>%#, col2 = c(data$TFname, data$Symbol))%>%
        distinct()
    # %>%
    #     rename(name=col1)#,group=col2)
   #write.table(nodes, sep = "\t", row.names = T)
    
    

    # nodes_frame<-as.data.frame(nodes)
    # rownames(nodes_frame) <- seq(0, nrow(nodes_frame) - 1)
    # edgs_frame<-as.data.frame(edgs)
    # rownames(edgs_frame) <- seq(0, nrow(edgs_frame) - 1)
    # write.table( net_d3$node, sep = "\t", row.names = T)
    # write.table( edgs, sep = "\t", row.names = T)
    
  
 

  
  # Generate graph from data frame
  net_pc <- graph_from_data_frame(
    d=edgs,vertices=nodes,
    directed=FALSE
  )
  
 
  # Calculate edge weights based on some criterion (e.g., degree)
  E(net_pc)$weight<-edgs$weight
  wc <- cluster_walktrap(net_pc)
  members <- membership(wc)
  net_d3  <- igraph_to_networkD3(net_pc,group = members)
  

  #number of 0 begin
  # edgs_frame<-as.data.frame(net_d3$links)
  # rownames(edgs_frame) <- seq(0, nrow(edgs_frame) - 1)
  # nodes_frame<-as.data.frame(net_d3$nodes)
  # rownames(nodes_frame) <- seq(0, nrow(nodes_frame) - 1)
  
  

  #net_d3$links <- subset(net_d3$links, value >= input$threshold)
  write.table( net_d3$node, sep = "\t", row.names = T)
      net_out <- forceNetwork(Links = net_d3$links, Nodes = net_d3$nodes,
                              Source = 'source', Target = 'target',
                              NodeID = 'name', Group = 'group',
                              zoom = T, bounded = F,
                              opacity = 5, fontSize = 32,
                              charge = -5
                              )
      #output$results3_2 <- renderDataTable({net_d3$links})

      #return(net_out)
    #}
  }
}
