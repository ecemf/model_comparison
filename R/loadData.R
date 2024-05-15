
loadData <- function(scenList = list("nzero" = "WP1 NetZero"), type = "latest", source = c("orig","extData")){
  
  out <- list()
  
  if(type == "latest"){
    
    # data folder
    dataFolder <- sort(list.dirs(path = "./data", full.names = TRUE, recursive = FALSE), decreasing = T)[1]
    time <- basename(dataFolder)
    
    if("orig" %in% source){
      out$origData <- list()
      for(group in names(scenList)){
        out$origData[[group]] <- readRDS(paste0(dataFolder, "/processed/", group,"_" ,time ,".rds"))
      }
    }
    
    if("extData" %in% source){
      out$extData <- list()
      for(group in names(scenList)){
        out$extData[[group]] <- readRDS(paste0(dataFolder, "/processed/", group,"_" ,time ,"_extended.rds"))
      }
    }
    
  } else if(type == "all") {
    
    if("orig" %in% source){
      out$origData <- list()
      for(group in names(scenList)){
        out$origData[[group]] <- NULL
        for (dir in list.dirs(path = "./data", full.names = TRUE, recursive = FALSE)){
          time <- basename(dir)
          out$origData[[group]] <- rbind(
            out$origData[[group]],
            readRDS(paste0(dir, "/processed/", group, "_", time, ".rds")) %>%
              mutate(version = time)
            )
        }
      }
    }
        
    if("extData" %in% source){
      out$extData <- list()
      for(group in names(scenList)){
        out$extData[[group]] <- NULL
        for (dir in list.dirs(path = "./data", full.names = TRUE, recursive = FALSE)){
          time <- basename(dir)
          out$extData[[group]] <- rbind(
            out$extData[[group]],
            readRDS(paste0(dir, "/processed/", group, "_", time, "_extended.rds")) %>%
              mutate(version = time)
          )
        }
      }
    }
  }
  
  return(out)
}


