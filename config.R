phenotype <- read.table("mvp.phe",head=TRUE)
map <- read.table("mvp.geno.map" , head = TRUE)
genotype <- attach.big.matrix("mvp.geno.desc")
matrix <- as.matrix(genotype)
data_frame <- as.data.frame(matrix)
data_frame[is.na(data_frame)] <- 0
big_matrix <- as.big.matrix(as.matrix(data_frame))



genotype <- attach.big.matrix("mvp.vcf.geno.desc")
phenotype <- read.table("./extdata/phenotype.txt",head=TRUE)
map <- read.table("mvp.vcf.geno.map" , head = TRUE)


# 设置日志记录函数
logging.initialize <- function() {}

mvp_server<-function(input,output)
{
  #data processing
  # 创建一个字符向量来存储日志消息
  logMessages <- character()
  
  # 定义一个自定义的print函数，用于记录日志消息
  log <- function(message) {
    logMessages <- c(logMessages, message)
    print(message)
  }
  
  
  logging.log <- log
  observeEvent(input$rice_im,
               {
                 output$logOutput <- renderPrint({
                   # 执行MVP.Data函数时将日志输出到log函数
                   MVP.Data(fileVCF = "./extdata/test.vcf",
                            #filePhe = "Phenotype.txt",
                            fileKin = FALSE,
                            filePC = FALSE,
                            out = "mvp.vcf",
                            verbose = TRUE,
                            ncpus = 2
                   )
                   
                   # 返回日志消息
                   logMessages
                 })
               })
  observeEvent(input$wheat_im,
               {
                 
                 output$logOutput <- renderPrint({
                   
                   
                   # 执行MVP.Data函数时将日志输出到log函数
                   MVP.Data(fileVCF = "./extdata/test.vcf",
                            #filePhe = "Phenotype.txt",
                            fileKin = FALSE,
                            filePC = FALSE,
                            out = "mvp.vcf",
                            verbose = TRUE,
                            ncpus = 2
                   )
                   
                   # 返回日志消息
                   logMessages
                 })
               })
  
  #phenotype data
  mvp_phe <- eventReactive(input$mvp_phenotype_im, {
    #mvp_phe <- read.table(input$mvp_phe_file$datapath,,header=T,nrows =10,sep = "\t",na.strings = c("NA"))
    mvp_phe <- read.table(input$mvp_phe_file$datapath,header=T)
    mvp_phe
  })
  observeEvent(input$mvp_phenotype_im,{
    output$phe_result<-renderTable({
      
      head(mvp_phe(), n = 10)
    })
  })
  
  #geno data
  genotype  <- eventReactive(input$wheat_geno, {
    
    genotype <- rMVP::attach.big.matrix("mvp.geno.desc")
    matrix <- as.matrix(genotype)
    data_frame_geno <- as.data.frame(matrix)
    data_frame_geno[is.na(data_frame_geno)] <- 0
    genotype<-rMVP::as.big.matrix(as.matrix(data_frame_geno))
    return(genotype)
    
  })
  
  observeEvent(input$wheat_geno, {
    output$gen_result <- renderTable({
      selected_rows <- head(genotype(), n = 10)
      selected_columns <- 1:4
      selected_data <- selected_rows[, selected_columns]
      return(selected_data)
    })
  })
  
  
  genotype2 <- eventReactive(input$rice_geno, {
    
    genotype2 <- rMVP::attach.big.matrix("mvp.vcf.geno.desc")
    matrix <- as.matrix(genotype2)
    data_frame_geno <- as.data.frame(matrix)
    data_frame_geno[is.na(data_frame_geno)] <- 0
    genotype2<-rMVP::as.big.matrix(as.matrix(data_frame_geno))
    return(genotype2)
  })
  observeEvent(input$rice_geno,{
    output$gen_result<-renderTable({
      selected_rows <- head(genotype2(), n = 10)
      selected_columns <- 1:4
      selected_data <- selected_rows[, selected_columns]
      return(selected_data)
      
    })
  })
  
  #map data
  map <- eventReactive(input$wheat_map, {
    
    map <- read.table("mvp.geno.map", head = TRUE)
    return(map)
  })
  
  observeEvent(input$wheat_map, {
    output$map_result <- renderTable({
      selected_rows <- head(map(), n = 10)
      selected_columns <- 1:4
      selected_data <- selected_rows[, selected_columns]
      return(selected_data)
    })
  })
  
  
  
  map2 <- eventReactive(input$rice_map, {
    
    map2 <- read.table("mvp.vcf.geno.map", head = TRUE)
    return(map2)
  })
  observeEvent(input$rice_map, {
    output$map_result <- renderTable({
      selected_rows <- head(map2(), n = 10)
      selected_columns <- 1:4
      selected_data <- selected_rows[, selected_columns]
      return(selected_data)
    })
  })
  
  
  observeEvent(input$start_GWAS_im, {
    mvp_phe1 <- NULL
    genotype1 <- NULL
    map1 <- NULL
    
    if(!is.null(input$wheat_geno) && !is.null(input$wheat_map)) {
      print("gen")
      mvp_phe1 <- mvp_phe()
      genotype1 <- genotype()
      map1 <- map()
    } else if (!is.null(input$rice_geno) && !is.null(input$rice_map)) {
      print("gen2")
      mvp_phe1 <- mvp_phe()
      genotype1 <- genotype2()
      map1 <- map2()
    } else {
      print("Error: Missing data for GWAS")
      return()
    }
    
    print("GWAS")
    MVP(
      phe = mvp_phe1,
      geno = genotype1,
      map = map1,
      nPC.GLM = 5,
      nPC.MLM = 3,
      nPC.FarmCPU = 3,
      priority = "speed",
      vc.method = "BRENT",
      maxLoop = 10,
      method.bin = "static",
      threshold = 0.05,
      method = c("GLM", "MLM", "FarmCPU"),
      file.output = TRUE
    )
  })
  

}