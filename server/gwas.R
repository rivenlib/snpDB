gwas_server<-function(input,output)
{

    gwas <- eventReactive(input$gwas_im, {
   gwas <- read.table(input$gwas_file$datapath,,header=T,nrows =10,sep = "\t",na.strings = c("NA"))
  gwas
})

observeEvent(input$gwas_im,{
  output$default2_5<-renderTable({
    gwas()
  })
})

gwas2 <- eventReactive(input$gwas_im, {

  gwas2 <- read.table(input$gwas_file$datapath,,header=T,sep = "\t",na.strings = c("NA"))
  gwas2
})


  observeEvent(input$CMplot_bt,{

    output$plot_ui<-renderUI({
      fluidRow(
        fluidRow(
          column(6, withSpinner(plotOutput("density_plot", height="500px"))),
          column(6, withSpinner(plotOutput("qq_plot", height="500px"))),
        ),
        br(),
        fluidRow(
          column(12, withSpinner(plotOutput("manhattan_plot", height="500px")))
        )
      )
    })
   
    output$density_plot<-renderPlot({
    
      #isolate({})
      
      
      CMplot(gwas2(),
             
             plot.type="d",#d——绘制SNP密度图，d——SNP密度  c——环状（circle） m——曼哈顿图 q——QQ图 b——以上全部类型（both）
             
             bin.size=1e6,#进行绘图的SNP总数
             
             chr.den.col=input$density_color,
             
             
             file=NULL,# file设置输出图片的格式，可以设定为"jpg", "pdf", "tiff"
             
             
             dpi=300, #dpi设置输出图片的分辨度
             
             file.output=F, #file.output设置是否输出图片
             
             main=input$plot_name,#备注图注标题
             
             verbose=T,#verbose是否打印日志信息
             
             #width = input$plot_width,height = input$plot_height
      )
      
      
      
    })
    
    
    output$qq_plot<-renderPlot({
      #isolate({})
      
      
      CMplot(gwas2(),
             
             plot.type="q",
             
             conf.int.col=NULL,#绘制QQ图中置信区间的颜色，可以用字符或向量
             
             box=TRUE,#是否添加边框
             col=input$qq_color,
             
             main=input$plot_name,#备注图注标题
             ylab.pos=2,
             dpi=300,
             
             file.output=F,
             
             #width = input$plot_width,height = input$plot_height,
             
             verbose=TRUE)
      
      
    })
    
    SNPs <- gwas2()[gwas2()[,4] < (0.05 / nrow(gwas2())), 1]
    genes <- paste("GENE", 1:length(SNPs), sep="_")
    if(input$manhattan_plot_threshold==TRUE)
    {
     if(is.null(input$CMplot_color))
     {
       
       output$manhattan_plot<-renderPlot({
         #isolate({})
         
         CMplot(gwas2(),
                col=c("orangered", "#EC407A", "#F1C40F", "#9B59B6", "#3498DB", "#1ABC9C", "#27AE60", 
                                 "#E67E22", "#E74C3C", "#FFC300", "#8E44AD", "#2C3E50"),
                highlight = SNPs,
                highlight.col = "orange",
                highlight.cex = 1,
                highlight.pch = c(15:17), 
                highlight.text = genes,      
                highlight.text.col = "black",
                ylab.pos=2,
                main=input$plot_name,#备注图注标题
                
                plot.type = "m",
                
                file.output=F,
                
                threshold =c(0.01,0.05)/nrow(gwas2()),#阈值设定，Bonfferoni法校正=0.01/nrow(Pmap)，通常以1/SNP总数为阈值。
                
                threshold.col=c('grey','black'),#控制阈值线的颜色，注意要对应
                
                threshold.lty = c(1,2),#阈值线的类型（7为实线，5为虚线）
                
                threshold.lwd = c(1,1),#阈值线的宽度，也可以调整对角线的宽度，2最合适）
                
                amplify = T,#放大显示~显著性的SNP点
                
                signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
                
                signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
                
                signal.col =c("red","orange"),
                
                width = 9,height = 6
         )#有效SNP位点的颜色
         
         
         
       }) 
       
     }
      else
      {
        
        output$manhattan_plot<-renderPlot({
          #isolate({})
          
          CMplot(gwas2(),
                 col =  input$CMplot_color,#c("#3E0A52", "#423D77","#3F678B","#468C8D", "#5FB47F", "#9FD55C","#F9E956"),
                 highlight = SNPs,
                 highlight.col = "orange",
                 highlight.cex = 1,
                 highlight.pch = c(15:17), 
                 highlight.text = genes,      
                 highlight.text.col = "black",
                 ylab.pos=2,
                 main=input$plot_name,#备注图注标题
                 
                 plot.type = "m",
                 
                 file.output=F,
                 
                 threshold =c(0.01,0.05)/nrow(gwas2()),#阈值设定，Bonfferoni法校正=0.01/nrow(Pmap)，通常以1/SNP总数为阈值。
                 
                 threshold.col=c('grey','black'),#控制阈值线的颜色，注意要对应
                 
                 threshold.lty = c(1,2),#阈值线的类型（7为实线，5为虚线）
                 
                 threshold.lwd = c(1,1),#阈值线的宽度，也可以调整对角线的宽度，2最合适）
                 
                 amplify = T,#放大显示~显著性的SNP点
                 
                 signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
                 
                 signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
                 
                 signal.col =c("red","orange"),
                 
                 width = 9,height = 6
          )#有效SNP位点的颜色
          
          
          
        }) 
        
      }
      
    }else
    {

      if(is.null(input$CMplot_color))
      {
        output$manhattan_plot<-renderPlot({
          CMplot(
            gwas2(),
            ylab.pos=2,
            col =  c("orangered", "#FFC300", "#F1C40F", "#9B59B6", "#3498DB", "#1ABC9C", "#27AE60", 
                                "#E67E22", "#E74C3C", "#EC407A", "#8E44AD", "#2C3E50"),
                                highlight = SNPs,
            highlight.col = "orange",
            highlight.cex = 1,
            highlight.pch = c(15:17), 
            highlight.text = genes,      
            highlight.text.col = "black",
            
            plot.type = "m",
            
            file.output=F,
            
            main=input$plot_name,#备注图注标题
            
            amplify = T,#放大显示~显著性的SNP点
            
            signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
            
            signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
            
            signal.col =c("red","orange"),
            dpi=300,
            verbose=TRUE,
            width = 9,height = 6
            
          )
          
        })
        
      }
      else
      {
        output$manhattan_plot<-renderPlot({
          CMplot(
            gwas2(),
            ylab.pos=2,
            col =  input$CMplot_color,
            highlight = SNPs,
            highlight.col = "orange",
            highlight.cex = 1,
            highlight.pch = c(15:17), 
            highlight.text = genes,      
            highlight.text.col = "black",
            
            plot.type = "m",
            
            file.output=F,
            
            main=input$plot_name,#备注图注标题
            
            amplify = T,#放大显示~显著性的SNP点
            
            signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
            
            signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
            
            signal.col =c("red","orange"),
            dpi=300,
            verbose=TRUE,
            width = 9,height = 6
            
          )
          
        })
        
      }
      
      
    }
    
    
    
    
  
  })
  
  
  output$download_CMplot <- downloadHandler(
    filename = function() {
      "file.pdf"
    },
    content = function(file) {
      pdf(file,  width = input$plot_width,height = input$plot_height, family = "Helvetica")
      #denesity_plot
      isolate({
        
        CMplot(gwas2(),
               
               plot.type="d",#d——绘制SNP密度图，d——SNP密度  c——环状（circle） m——曼哈顿图 q——QQ图 b——以上全部类型（both）
               
               bin.size=1e6,#进行绘图的SNP总数
               
               chr.den.col=input$density_color,
               
               
               file=NULL,# file设置输出图片的格式，可以设定为"jpg", "pdf", "tiff"
               
               
               dpi=300, #dpi设置输出图片的分辨度
               
               file.output=F, #file.output设置是否输出图片
               
               main=input$plot_name,#备注图注标题
               
               verbose=T,#verbose是否打印日志信息
               
               #width = input$plot_width,height = input$plot_height
        )
        
      })
      
      
      
      
      #qq_plot
      isolate({
        CMplot(gwas2(),
               
               plot.type="q",
               
               conf.int.col=NULL,#绘制QQ图中置信区间的颜色，可以用字符或向量
               
               box=TRUE,#是否添加边框
               col=input$qq_color,
               
               main=input$plot_name,#备注图注标题
               ylab.pos=2,
               dpi=300,
               
               file.output=F,
               #width = input$plot_width,height = input$plot_height,
               verbose=TRUE)
        
      })
      
      
      #manhattan_plot
      SNPs <- gwas2()[gwas2()[,4] < (0.05 / nrow(gwas2())), 1]
      genes <- paste("GENE", 1:length(SNPs), sep="_")
      if(input$manhattan_plot_threshold==TRUE)
      {
        if(is.null(input$CMplot_color))
        {
          isolate({
            CMplot(gwas2(),
                   col=c("orangered", "#FFC300", "#F1C40F", "#9B59B6", "#3498DB", "#1ABC9C", "#27AE60", 
                                    "#E67E22", "#E74C3C", "#EC407A", "#8E44AD", "#2C3E50"),
                   highlight = SNPs,
                   highlight.col = "orange",
                   highlight.cex = 1,
                   highlight.pch = c(15:17), 
                   highlight.text = genes,      
                   highlight.text.col = "black",
                   ylab.pos=2,
                   main=input$plot_name,#备注图注标题
                   
                   plot.type = "m",
                   
                   file.output=F,
                   
                   threshold =c(0.01,0.05)/nrow(gwas2()),#阈值设定，Bonfferoni法校正=0.01/nrow(Pmap)，通常以1/SNP总数为阈值。
                   
                   threshold.col=c('grey','black'),#控制阈值线的颜色，注意要对应
                   
                   threshold.lty = c(1,2),#阈值线的类型（7为实线，5为虚线）
                   
                   threshold.lwd = c(1,1),#阈值线的宽度，也可以调整对角线的宽度，2最合适）
                   
                   amplify = T,#放大显示~显著性的SNP点
                   
                   signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
                   
                   signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
                   
                   signal.col =c("red","orange"),
                   
                   width = 9,height = 6
            )#有效SNP位点的颜色
            
          })
          
        }
        else
        {
          isolate({
            CMplot(gwas2(),
                   col=input$CMplot_color,
                   highlight = SNPs,
                   highlight.col = "orange",
                   highlight.cex = 1,
                   highlight.pch = c(15:17), 
                   highlight.text = genes,      
                   highlight.text.col = "black",
                   ylab.pos=2,
                   main=input$plot_name,#备注图注标题
                   
                   plot.type = "m",
                   
                   file.output=F,
                   
                   threshold =c(0.01,0.05)/nrow(gwas2()),#阈值设定，Bonfferoni法校正=0.01/nrow(Pmap)，通常以1/SNP总数为阈值。
                   
                   threshold.col=c('grey','black'),#控制阈值线的颜色，注意要对应
                   
                   threshold.lty = c(1,2),#阈值线的类型（7为实线，5为虚线）
                   
                   threshold.lwd = c(1,1),#阈值线的宽度，也可以调整对角线的宽度，2最合适）
                   
                   amplify = T,#放大显示~显著性的SNP点
                   
                   signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
                   
                   signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
                   
                   signal.col =c("red","orange"),
                   
                   width = 9,height = 6
            )#有效SNP位点的颜色
            
          })
          
        }
        
      }else
      {
        if(is.null(input$CMplot_color))
        {
          isolate({
            CMplot(
              gwas2(),
              ylab.pos=2,
              col =  c("orangered", "#FFC300", "#F1C40F", "#9B59B6", "#3498DB", "#1ABC9C", "#27AE60", 
                                      "#E67E22", "#E74C3C", "#EC407A", "#8E44AD", "#2C3E50"),
              highlight = SNPs,
              highlight.col = "orange",
              highlight.cex = 1,
              highlight.pch = c(15:17), 
              highlight.text = genes,      
              highlight.text.col = "black",
              
              plot.type = "m",
              
              file.output=F,
              
              main=input$plot_name,#备注图注标题
              
              amplify = T,#放大显示~显著性的SNP点
              
              signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
              
              signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
              
              signal.col =c("red","orange"),
              dpi=300,
              verbose=TRUE,
              width = 9,height = 6
              
            )
            
          })
          
        }
        else
        {
          isolate({
            CMplot(
              gwas2(),
              ylab.pos=2,
              col = input$CMplot_color,
              highlight = SNPs,
              highlight.col = "orange",
              highlight.cex = 1,
              highlight.pch = c(15:17), 
              highlight.text = genes,      
              highlight.text.col = "black",
              
              plot.type = "m",
              
              file.output=F,
              
              main=input$plot_name,#备注图注标题
              
              amplify = T,#放大显示~显著性的SNP点
              
              signal.cex = c(1,1),#设置有效SNP位点的大小,建议选用2/3号为宜
              
              signal.pch = c(20,20),#设置有效SNP位点的形状，16和19为圆形。
              
              signal.col =c("red","orange"),
              dpi=300,
              verbose=TRUE,
              width = 9,height = 6
              
            )
            
          })
          
        }
        
        
      }
      
     # p <- CMplot(gwas2(), plot.type = input$plot_type, main = input$plot_name, file.output = F)
      dev.off()
      file.rename(from = file, to = paste0(input$plot_name, ".pdf"))
      file.copy(from = paste0(input$plot_name, ".pdf"), to = file)
    }
  )

}
