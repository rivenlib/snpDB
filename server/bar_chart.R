bar_chart<-function(data,edgs)
{
  # edgs <- reactive({
  #   select(data, 'TFname', 'TargetId')
  # })
  # 
  
  # 统计每个结点的出度
  targetID <- edgs %>%
    count(TF_ID, name = "targetID") %>%
    arrange(desc(targetID))
  # 绘制柱状图

  
    bar<-ggplot(targetID, aes(x = TF_ID, y = targetID, fill = TF_ID)) +
      geom_bar(stat = "identity") +
      theme(
        #panel.background = element_rect(fill = "grey"),
        #plot.background = element_rect(fill = "grey"),
            axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
 

  return(bar)
}
