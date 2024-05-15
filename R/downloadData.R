

downloadMetaData <- function(outputPath){
  
  dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
  filenameSuffix <- basename(outputPath)
  
  # download helper functions
  source_python("./python/download_meta.py")
  
  # download metadata
  print(paste0("Downloading metadata."))
  if(!file.exists(paste0(outputPath, "/metadata_" ,filenameSuffix ,".xlsx"))){
    download_iiasa_meta_py(fileName = paste0(outputPath, "/metadata_",filenameSuffix),
                           db="ecemf_internal",
                           default_only = TRUE)
  }
  
}

downloadData <- function(database, outputPath, scenList = list("data" = "*"), regions="*", models = "*", downloadPerScenario = FALSE){
  
  dir.create(paste0(outputPath, "/downloaded"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(outputPath,"/processed"), recursive = TRUE, showWarnings = FALSE)
  filenameSuffix <- basename(outputPath)
  
  # download helper functions
  source_python("./python/download_meta.py")
  source_python("./python/download_iiasa_db.py")
  
  # table to filter most up to date results  
  metaFilter <- read.xlsx(paste0(outputPath, "/metadata_",filenameSuffix,".xlsx")) %>%
    fill(model, .direction = "down") %>% # fill NAs with above row values
    arrange(desc(create_date)) %>%
    mutate(cleanModelName = simplifyModelName(model)) %>%
    group_by(cleanModelName,scenario) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  # download a single file with all scenarios for each group
  if(! downloadPerScenario){
    for(group in names(scenList)){
      if(!file.exists(paste0(outputPath, "/downloaded/", group,"_" ,filenameSuffix ,".xlsx"))){
        print(paste0("Downloading ", group, " results from iiasa."))
        download_iiasa_db_py(fileName = paste0(outputPath, "/downloaded/", group,"_" ,filenameSuffix),
                             db="ecemf_internal",
                             model=models,
                             scen=scenList[[group]],
                             region=regions)
      }
    }
    
    #filter only latest results  
    for(group in names(scenList)){
      if(!file.exists(paste0(outputPath, "/", group,"_" ,filenameSuffix ,".xlsx"))){
        print(paste0("Filtering downloaded data for group: ", group, "."))
        # cleaning data from NAs, formatting issues, filtering only more recent data, and simplifying model names
        data <- read.xlsx(paste0(outputPath, "/downloaded/", group,"_" ,filenameSuffix ,".xlsx")) %>% 
          as.quitte() %>% 
          filter(!is.na(value)) %>%  
          mutate(region = gsub("&amp;", "&", region)) %>% 
          right_join(metaFilter %>% filter(scenario %in% scenList[[group]]) %>% select(model,scenario), by = join_by(model, scenario)) %>%
          mutate(model = simplifyModelName(model)) %>%
          na.omit()
        saveRDS(data,paste0(outputPath, "/processed/", group,"_" ,filenameSuffix ,".rds"))
        write.xlsx(data %>% pivot_wider(names_from = period, values_from = value) %>% select(where(~!all(is.na(.x)))), file = paste0(outputPath, "/", group,"_" ,filenameSuffix ,".xlsx"))
      }
    }
    return()
  }
  
  #alternative way to download individual files per model and scenario
  if(downloadPerScenario){
    #download data and merge it together 
    for(group in names(scenList)){
      if(!file.exists(paste0(outputPath, "/", group,"_" ,filenameSuffix ,".xlsx"))){
        if(models == "*")
          modelsToDownload <- unique(metaFilter$model)
        else
          modelsToDownload <- unique(metaFilter$model) %>% filter(model %in% models)
        files <- list()
        l <- lapply(modelsToDownload, function(m){
          scenariosToDownload <- metaFilter %>% filter(model == m, scenario %in% scenList[[group]]) %>% pull(scenario)
          lapply(scenariosToDownload, function(s){
            if(!file.exists(paste0(outputPath, "/downloaded/", m, "_", s, "_",filenameSuffix ,".xlsx"))){
              print(paste0("Downloading:", outputPath, "/downloaded/", m, "_", s, "_",filenameSuffix ,".xlsx"))
              download_iiasa_db_py(fileName = paste0(outputPath, "/downloaded/", m, "_", s, "_",filenameSuffix),
                                  db=database,
                                  model=m,
                                  scen=s,
                                  region=regions)
            }
            files <- append(files,paste0(outputPath, "/downloaded/", m, "_", s, "_",filenameSuffix))
          })    
        })
        #merge all files into one
        mergedData <- setNames(lapply(files, read.xlsx), files)
        write.xlsx(mergedData, file = paste0(outputPath, "/downloaded/", group,"_" ,filenameSuffix ,".xlsx"))
        data <- read.xlsx(paste0(outputPath, "/downloaded/", group,"_" ,filenameSuffix ,".xlsx")) %>% 
          as.quitte() %>% 
          filter(!is.na(value)) %>%  
          mutate(region = gsub("&amp;", "&", region)) %>% 
          right_join(metaFilter %>% filter(scenario %in% scenList[[group]]) %>% select(model,scenario), by = join_by(model, scenario)) %>%
          mutate(model = simplifyModelName(model)) %>%
          na.omit()
        saveRDS(data,paste0(outputPath, "/processed/", group,"_" ,filenameSuffix ,".rds"))
        write.xlsx(data %>% pivot_wider(names_from = period, values_from = value) %>% select(where(~!all(is.na(.x)))), file = paste0(outputPath, "/", group,"_" ,filenameSuffix ,".xlsx"))
      }
    }
    return()
  }
  
}



