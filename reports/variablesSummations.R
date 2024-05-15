
createVariablesSummationsReport <- function(data, outputFolder, saveCharts=TRUE){
  
  # loop through models and check summations
  fullLogFile <- paste0(outputFolder, "/summationChecks/full.log")
  dir.create(paste0(outputFolder, "/summationChecks"), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(fullLogFile))
    unlink(fullLogFile)
  
  for(modelName in unique(data$model)){
    modelOutputFolder <- paste0(outputFolder, "/summationChecks/", modelName)
    print(as.character(modelName))
    write(paste0(as.character(modelName),"\n"), fullLogFile, append = TRUE)
    dir.create(modelOutputFolder, recursive = TRUE, showWarnings = FALSE) #create model output folder  
    tmp <- data %>% filter(model == modelName) %>% mutate(model = as.character(model)) 
    #capture.output(tmp <- fillMissingSummations(data %>% filter(model == modelName) %>% mutate(model = as.character(model)), summationsFile = "ECEMF"), type = "message", file = fullLogFile, append = TRUE)
    #capture.output(tmp <- fillSummationPairs(tmp, "ECEMF"), type = "message", file = fullLogFile, append = TRUE)
    capture.output(tmp <- piamInterfaces::fillSummationPairs(tmp, "ECEMF"), type = "message", file = fullLogFile, append = TRUE)
    write(paste0("\nSummation checks - ", as.character(modelName)), fullLogFile, append = TRUE)
    checkSummations(tmp, outputDirectory = modelOutputFolder, logFile= paste0(modelName, "_log.txt"), summationsFile = "ECEMF", 
                    generatePlots = saveCharts,
                    mainReg = unique(data$region), csvSeparator = ",", roundDiff = TRUE)
    log <- readLines(paste0(modelOutputFolder, "/", modelName, "_log.txt"))
    write(log, fullLogFile, append=TRUE)
    write(paste0("\n\n"), fullLogFile, append = TRUE)
  }
  
  # read priority mapping
  priorityMapping <- read.csv("./mapping/summation_priorities.csv")
  
  # post process checkSummations.csv
  pctFormat = createStyle(numFmt = "0.0%") # Create a percent style
  fullPrio <- list()
  for(number in unique(priorityMapping$prio)){
    fullPrio[[number]] <- list()
  }
  summationGroups <- getSummations("ECEMF")
  
  for(modelName in unique(data$model)){
    print(paste0("Model: ", modelName))
    modelOutputFolder <- paste0(outputFolder, "/summationChecks/", modelName)
    fulllCheck <- read.csv(paste0(modelOutputFolder, "/checkSummations.csv"))
    fulllCheck <- fulllCheck %>%
      mutate(reldiff = reldiff/100)
    
    for(number in unique(priorityMapping$prio)){
      
      prioVars <- priorityMapping %>% filter(prio == number) %>% pull(parent)
      
      # initializing summations list
      list <- list()
      remainingChecks <- fulllCheck %>% 
        filter(variable %in% prioVars,
               period %in% c(2020,2050))
      
      #if(nrow(remainingChecks) == 0){
      #  next
      #}
      
      #not reported summation vars
      missingValues <- remainingChecks %>% group_by(variable,region,scenario) %>% mutate(tSum = sum(checkSum)) %>% filter(tSum == 0) %>% ungroup() %>% select(-tSum)
      modelPeriods <- fulllCheck %>% filter(model == modelName) %>% pull(period) %>% unique()
      if(nrow(missingValues) > 0){
        zeroCheckSum <- missingValues %>%
          arrange(match(period, c(2050,2020,rev(modelPeriods[!(modelPeriods %in% c(2020,2050))])))) %>%
          group_by(variable) %>%
          filter(period == first(period))
        remainingChecks <- remainingChecks %>% anti_join(missingValues)
      }
        
      #underlined summation checks
      currPrioCheck <- remainingChecks %>% 
        filter((abs(diff) > 0.01 & abs(reldiff) > 0.01))
#        filter(!(abs(diff) < 0.1 & abs(reldiff) < 0.05))
      if(nrow(currPrioCheck) > 0){
        list <- lapply(unique(currPrioCheck$period), function(year){
          currPrioCheck %>% filter(period == year)
        })
        names(list) <- unique(currPrioCheck$period)
        remainingChecks <- remainingChecks %>% anti_join(currPrioCheck)
      }
      
      if(nrow(missingValues) > 0){
        if(nrow(zeroCheckSum) > 0){
        list[["zero checkSum"]] <- zeroCheckSum
        }
      }
      
      if(nrow(remainingChecks) > 0){
        list[["not relevant"]] <- remainingChecks
      }
      
      #reported parents
      reportedVars <- data %>%
        filter(model == modelName, 
               variable %in% unique(gsub(" [1-9]$", "", prioVars))) %>%
        na.omit() %>%
        group_by(variable) %>%
        filter(!all(value==0)) %>% pull(variable) %>% unique() %>% as.character()
      allZeroVars <- data %>%
        filter(model == modelName, 
               variable %in% unique(gsub(" [1-9]$", "", prioVars))) %>%
        group_by(variable) %>%
        filter(all(value==0)) %>% pull(variable) %>% unique() %>% as.character()
      notReportedVars <- prioVars[!prioVars %in% union(reportedVars,allZeroVars)]
      if(length(reportedVars) + length(allZeroVars) + length(notReportedVars) != length(prioVars))
        warning(paste0("Priority ", number, " parent variables available wrongly reported to model: ", modelName))
      maxLength <- max(length(reportedVars),length(allZeroVars),length(notReportedVars))
      list[["reported parents"]] <- data.frame(
        "Reported_variables" = c(reportedVars,rep(NA, maxLength - length(reportedVars))), 
        "All_zeros_variables" = c(allZeroVars,rep(NA, maxLength - length(allZeroVars))), 
        "Non_reported_Variables" = c(notReportedVars,rep(NA, maxLength - length(notReportedVars))))
      
      #reported children
      childVars <- summationGroups %>% 
        filter(parent %in% prioVars) %>% 
        pull(child) %>% unique()
      
      reportedVars <- data %>%
        filter(model == modelName, 
               variable %in% childVars) %>%
        na.omit() %>%
        group_by(variable) %>%
        filter(!all(value==0)) %>% pull(variable) %>% unique() %>% as.character()
      allZeroVars <- data %>%
        filter(model == modelName, 
               variable %in% childVars) %>%
        group_by(variable) %>%
        filter(all(value==0)) %>% pull(variable) %>% unique() %>% as.character()
      notReportedVars <- childVars[!childVars %in% union(reportedVars,allZeroVars)]
      if(length(reportedVars) + length(allZeroVars) + length(notReportedVars) != length(childVars))
        warning(paste0("Priority ", number, " child variables available wrongly reported to model: ", modelName))
      maxLength <- max(length(reportedVars),length(allZeroVars),length(notReportedVars))
      list[["reported child"]] <- data.frame(
        "Reported_variables" = c(reportedVars,rep(NA, maxLength - length(reportedVars))), 
        "All_zeros_variables" = c(allZeroVars,rep(NA, maxLength - length(allZeroVars))), 
        "Non_reported_Variables" = c(notReportedVars,rep(NA, maxLength - length(notReportedVars))))

      # full list of summations not matched
      if(nrow(currPrioCheck) > 0){
        for(per in unique(currPrioCheck$period)){
          if(!(as.character(per) %in% names(fullPrio[[number]]))){ 
            fullPrio[[number]][[as.character(per)]] <- currPrioCheck %>% filter(period == per)
          }
          else {
            fullPrio[[number]][[as.character(per)]] <- rbind(fullPrio[[number]][[as.character(per)]], currPrioCheck %>% filter(period == per))
          }
        }
      }
      
      if(length(list)>0){
        wb = createWorkbook()
        for(name in names(list)){
          sht = addWorksheet(wb, name)
          writeData(wb, name, list[[name]])
          addStyle(wb, sht, style=pctFormat, cols=10, rows=2:(nrow(list[[name]])+1), gridExpand=TRUE)
        }
        saveWorkbook(wb, paste0(modelOutputFolder, "/", modelName , "_priority", number, ".xlsx"), overwrite = TRUE)
      }
    }
    
    wb = createWorkbook()
    sht = addWorksheet(wb, "data")
    writeData(wb, "data", fulllCheck)
    addStyle(wb, sht, style=pctFormat, cols=10, rows=2:(nrow(fulllCheck)+1), gridExpand=TRUE)
    saveWorkbook(wb, paste0(modelOutputFolder, "/", modelName , "_all.xlsx"), overwrite = TRUE)
  } 
  
  for(number in unique(priorityMapping$prio)){
    wb = createWorkbook()
    for(name in names(fullPrio[[number]])){
      sht = addWorksheet(wb, name)
      writeData(wb, name, fullPrio[[number]][[name]])
      addStyle(wb, sht, style=pctFormat, cols=10, rows=2:(nrow(fullPrio[[number]][[name]])+1), gridExpand=TRUE)
    }
    saveWorkbook(wb, paste0(outputFolder, "/summationChecks/Priority", number, ".xlsx"), overwrite = TRUE)
  }
  
  for(modelName in unique(data$model)){
    modelOutputFolder <- paste0(outputFolder, "/summationChecks/", modelName)
    file.remove(paste0(modelOutputFolder, "/checkSummations.csv"))
  }
  
}

  