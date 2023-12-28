mvp_server<-function(input,output)
{
  # Define reactive expression to read the file
  mvp_phe <- reactive({
    if (is.null(input$mvp_phe_file))
      return(NULL)
    
    read.table(input$mvp_phe_file$datapath, header = TRUE)
  })
  
  mvp_phe_data <- mvp_phe()  # 调用 mvp_phe() 函数获取数据
  file_prefix <- mvp_phe_data[1, 2]  # 获取第二列第一行的内容
  
  output$phe_ui <- renderUI({
    req(input$mvp_phe_file)
    
    
    # 构建新的文件路径
    file_path <- paste0("/srv/shiny-server/snpDB_v3/", file_prefix, ".Phe_Dist.jpg")
    
    # 检查文件是否存在并删除
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
    
    # Read the selected file
    #phe <- read.table(input$mvp_phe_file$datapath, header = TRUE)
    # if (file.exists("/srv/shiny-server/snpDB_v3/V6.Phe_Dist.jpg")) {
    #   file.remove("/srv/shiny-server/snpDB_v3/V6.Phe_Dist.jpg")
    # }
    # Generate the plot
    MVP.Hist(phe = mvp_phe(), file.type = "jpg", breakNum = 18, dpi = 300)
    
    # Display the plot
    #path<-paste0("./"global$folder_name,"/V6.Phe_Dist.jpg")
    #if (file.exists("/srv/shiny-server/snpDB_v3/PR_SD.Phe_Dist.jpg")) {
    if (file.exists(file_path)) { 
     imageOutput("phe_ui_image", height = "500px")
    } else {
      p("Image not found")
    }
  })
  output$phe_ui_image<-renderImage({
    
    # img_path <- "/srv/shiny-server/snpDB_v3/PR_SD.Phe_Dist.jpg"  # 服务器上图片的路径
    img_path <- file_path  # 服务器上图片的路径
    
    # 读取图片
    image <- image_read(img_path)
    
    # 缩小图片
    scaled_image <- image_scale(image, "500x")  # 调整为宽度为800像素，高度按比例自动调整
    
    # 保存缩小后的图片到临时文件
    temp_file <- tempfile(fileext = ".jpg")
    image_write(scaled_image, path = temp_file)
    
    # 返回临时文件路径
    list(src = temp_file,
         contentType = "image/jpeg",
         alt = "Image not found")
    
  },deleteFile = TRUE)
  
  
  
  #geno data
  global <- reactiveValues(genotype = NULL, map = NULL,Task_Id = NULL,folder_name=NULL)
  
  observeEvent(input$species, {
    if (input$species == "rice") {
      global$genotype <- rMVP::attach.big.matrix("mvp.vcf.geno.desc")
      matrix <- as.matrix(global$genotype)
      data_frame_geno <- as.data.frame(matrix)
      data_frame_geno[is.na(data_frame_geno)] <- 0
      global$genotype<-rMVP::as.big.matrix(as.matrix(data_frame_geno))
      
      global$map <- read.table("mvp.vcf.geno.map", head = TRUE)
      
    } else if (input$species == "wheat") {
      global$genotype <- rMVP::attach.big.matrix("mvp.vcf.geno.desc")
      matrix <- as.matrix(global$genotype)
      data_frame_geno <- as.data.frame(matrix)
      data_frame_geno[is.na(data_frame_geno)] <- 0
      global$genotype<-rMVP::as.big.matrix(as.matrix(data_frame_geno))
      
      global$map <- read.table("mvp.vcf.geno.map", head = TRUE)
    }
  })
 
  
  current_path <- "./Output"
  folders <- list.dirs(current_path, full.names = FALSE)
  observe({
    
    updateSelectizeInput(
      inputId = "folder_select",
      choices = folders,
      server = TRUE
    )
    
    
  })
  observeEvent(input$start_GWAS_im, {
    
    if (!is.null(global$genotype) && !is.null(global$map)) {
      # 生成唯一的文件夹名称
      global$Task_Id <- format(Sys.time(), "%Y%m%d%H%M%S")
      #global$Task_Id <- timestamp
      
      # 创建文件夹
      dir_path <- file.path("./Output", global$Task_Id)
      dir.create(dir_path)
      
      # 设置输出路径为新创建的文件夹路径
      outpath <- dir_path
      output$outpath_text <- renderText({
        paste("Task ID:", global$Task_Id)
      })
      
      # Update selectize input choices
      folders <<- list.dirs(current_path, full.names = FALSE) 
      updateSelectizeInput(
        inputId = "folder_select",
        choices = folders,
        server = TRUE
      )
      
      
    MVP(
      phe = mvp_phe(),
      geno = global$genotype,
      map = global$map,
      nPC.GLM = 5,
      nPC.MLM = 3,
      nPC.FarmCPU = 3,
      priority = "speed",
      vc.method = "BRENT",
      maxLoop = 10,
      method.bin = "static",
      threshold = 0.05,
      method = c("GLM", "MLM", "FarmCPU"),
      outpath = outpath,
      file.output = TRUE
    )
    
  }
    })

  
 

  observe(  
   
    if(input$folder_select!="")
    {
      global$folder_name<-input$folder_select
      output$visual_ui<-renderUI({
        
        fluidPage(
          
          
          fluidRow(
            
            br(),
            fluidRow(
              column(6, imageOutput("phe_dist5", height = "500px")),
              column(6, imageOutput("qq_plot_5", height = "500px")),
              
            ),
            br(),
            fluidRow( column(12, imageOutput("density_plot_5"))),
            fluidRow( column(12, imageOutput("manhattan_plot_5")))
            
            
          )
        )
        
      })
    }
    else{
      return (NULL)
    }
    )
  #ui
 
 
  output$phe_dist5<-renderImage({
    
    img_path <- paste0("./Output/",global$folder_name,"/V6.Phe_Dist.jpg")  # 服务器上图片的路径
    
    # 读取图片
    image <- image_read(img_path)
    
    # 缩小图片
    scaled_image <- image_scale(image, "500x")  # 调整为宽度为800像素，高度按比例自动调整
    
    # 保存缩小后的图片到临时文件
    temp_file <- tempfile(fileext = ".jpg")
    image_write(scaled_image, path = temp_file)
    
    # 返回临时文件路径
    list(src = temp_file,
         contentType = "image/jpeg",
         alt = "Image not found")
    
  },deleteFile = TRUE)
  
  output$qq_plot_5<-renderImage({
    
    img_path <-paste0("./Output/",global$folder_name,"/V6.GLM.QQplot.jpg")  # 服务器上图片的路径
    
    # 读取图片
    image <- image_read(img_path)
    
    # 缩小图片
    scaled_image <- image_scale(image, "500x")  # 调整为宽度为800像素，高度按比例自动调整
    
    # 保存缩小后的图片到临时文件
    temp_file <- tempfile(fileext = ".jpg")
    image_write(scaled_image, path = temp_file)
    
    # 返回临时文件路径
    list(src = temp_file,
         contentType = "image/jpeg",
         alt = "Image not found")
    
  },deleteFile = TRUE)
 
  output$density_plot_5 <- renderImage({
   
    img_path <- paste0("./Output/",global$folder_name,"/V6.GLM.V6.MLM.V6.FarmCPU.SNP-Density.jpg")  # 服务器上图片的路径
    
    # 读取图片
    image <- image_read(img_path)
    
    # 缩小图片
    scaled_image <- image_scale(image, "900x")  # 调整为宽度为800像素，高度按比例自动调整
    
    # 保存缩小后的图片到临时文件
    temp_file <- tempfile(fileext = ".jpg")
    image_write(scaled_image, path = temp_file)
    
    # 返回临时文件路径
    list(src = temp_file,
         contentType = "image/jpeg",
         alt = "Image not found")
    
  },deleteFile = TRUE)
  
  output$manhattan_plot_5<-renderImage({
    path<-paste0("./Output/",global$folder_name,"/V6.MLM.Rectangular-Manhattan.jpg")
    img_path <- path  # 服务器上图片的路径
    
    # 读取图片
    image <- image_read(img_path)
    
    # 缩小图片
    scaled_image <- image_scale(image, "900x")  # 调整为宽度为800像素，高度按比例自动调整
    
    # 保存缩小后的图片到临时文件
    temp_file <- tempfile(fileext = ".jpg")
    image_write(scaled_image, path = temp_file)
    
    # 返回临时文件路径
    list(src = temp_file,
         contentType = "image/jpeg",
         alt = "Image not found")
  },deleteFile = TRUE)

 

  
  # 下载处理函数
  output$downloadData5 <- downloadHandler(
    filename = "data.zip", # 下载的ZIP文件名
    content = function(file) {
      # 根据选择的文件夹设置当前路径
      folder_name <- input$folder_select
      current_path <- file.path("./Output", folder_name)
      
      # 获取当前路径下的所有JPG文件和CSV文件
      files_to_zip <- list.files(current_path, pattern = "\\.(jpg|csv)$", full.names = TRUE)
      
      # 将文件打包成ZIP文件
      zip_file <- file.path(current_path, "data.zip")
      zip(zip_file, files = files_to_zip)
      
      # 将ZIP文件复制到下载目录
      file.copy(zip_file, file)
      
      # 删除临时文件
      file.remove(zip_file)
    }
  )

}
