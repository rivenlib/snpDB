mvp_server<-function(input,output,session)
{
  
  observe({
    current_path <<- file.path(getwd(), "Output")  # 替换为你要获取文件列表的文件夹路径
    folders <<- list.files(path = current_path, full.names = FALSE)
    updateSelectizeInput(inputId = "folder_select", choices = folders,selected = NULL)
  })
  
  # Define reactive expression to read the file
  mvp_phe <- reactive({
    # req(input$mvp_phe_file)
    # validate(need(!is.null(input$mvp_phe_file$datapath), "文件上传错误"))
    data <-  read.table(input$mvp_phe_file$datapath, header = TRUE)
     
  })
  

  
  output$phe_ui <- renderUI({
    req(input$mvp_phe_file)
    #mvp_phe_data <- mvp_phe()  # 调用 mvp_phe() 函数获取数据
    file_prefix <- colnames(mvp_phe())[2]  # 获取第二列第一行的内容
    # 构建新的文件路径
    file_path <- paste0(getwd(), "/result/",file_prefix, ".Phe_Dist.jpg")
    
    # Display the plot
    if (file.exists(file_path)) { 
      imageOutput("phe_ui_image", height = "500px")
    } else {
      print(file_path)
      # Generate the plot
      MVP.Hist(phe = mvp_phe(),file.type = "jpg", breakNum = 18,dpi = 300,outpath = "/srv/shiny-server/snpDB_v3/result" )
      imageOutput("phe_ui_image", height = "500px")
    }
    
    
    

    #print(file_path)
    
    
  })
  output$phe_ui_image<-renderImage({

    
    file_prefix <- colnames(mvp_phe())[2]  # 获取第二列第一行的内容
    # 构建新的文件路径
    file_path <- paste0(getwd(), "/result/",file_prefix, ".Phe_Dist.jpg")
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
  
  #observeEvent(input$species, {
   # if (input$species == "rice") {
   #   global$genotype <- rMVP::attach.big.matrix("mvp.plink.geno.desc")
   #   matrix <- as.matrix(global$genotype)
   #   data_frame_geno <- as.data.frame(matrix)
   #   data_frame_geno[is.na(data_frame_geno)] <- 0
   #   global$genotype<-rMVP::as.big.matrix(as.matrix(data_frame_geno))
   #   
   #   global$map <- read.table("mvp.plink.geno.map", head = TRUE)
   #   
   # } else if (input$species == "wheat") {
   #   global$genotype <- rMVP::attach.big.matrix("mvp.plink.geno.desc")
   #   matrix <- as.matrix(global$genotype)
   #   data_frame_geno <- as.data.frame(matrix)
   #   data_frame_geno[is.na(data_frame_geno)] <- 0
   #   global$genotype<-rMVP::as.big.matrix(as.matrix(data_frame_geno))
   #   
   #   global$map <- read.table("mvp.plink.geno.map", head = TRUE)
   # }
 # })
 
  



  
 

  observeEvent (input$folder_select, {
    
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
    
  })
  
  task_id <- reactiveVal()
  outpath <- reactiveVal()
  
  observeEvent(input$start_GWAS_im, {

    # 生成唯一的文件夹名称
    global$Task_Id <- format(Sys.time(), "%Y%m%d%H%M%S")
    task_id(format(Sys.time(), "%Y%m%d%H%M%S"))
    
    # 创建文件夹
    dir_path <- file.path("./Output", task_id())
    dir.create(dir_path)
    # 设置输出路径为新创建的文件夹路径
    outpath(dir_path)
    
    
    
    # 渲染输出路径
    output$outpath_text <- renderText({
      paste("Task ID:", task_id())
    })
    
    # 获取输入参数
    phe_path <- input$mvp_phe_file$datapath
    output_folder <- outpath()
    
    # 设置GWAS.R文件的路径
    gwas_script_path <- "GWAS.R"
    
    # 构造R命令行参数
    r_args <- paste0(" ",phe_path, " ", output_folder," ")
    
    # 构造完整的R脚本命令
    r_command <- paste("nohup Rscript", gwas_script_path, r_args,"&")
    
    # 执行GWAS.R文件
    system(r_command)

    
  })
  
  
  

  
  
  #ui
 
  output$phe_dist5<-renderImage({
    req(global$folder_name)

    folder_path <- paste0("./Output/", global$folder_name, "/")
    file_pattern <- "*.Phe_Dist.jpg$"
    img_path <- list.files(path = folder_path, pattern = file_pattern, full.names = TRUE)  # 服务器上图片的路径



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
    
    folder_path <- paste0("./Output/", global$folder_name, "/")
    file_pattern <- "*.GLM.QQplot.jpg"
    img_path <- list.files(path = folder_path, pattern = file_pattern, full.names = TRUE)
    

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
   
    folder_path <- paste0("./Output/", global$folder_name, "/")
    file_pattern <- "*.SNP-Density.jpg"
    img_path <- list.files(path = folder_path, pattern = file_pattern, full.names = TRUE)  # 服务器上图片的路径
     
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
    
    
    
    folder_path <- paste0("./Output/", global$folder_name, "/")
    file_pattern <- "*.Rectangular-Manhattan.jpg"
    path <- list.files(path = folder_path, pattern = file_pattern, full.names = TRUE)
    
    
    
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
