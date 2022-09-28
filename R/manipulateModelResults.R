
manipulateModelResults <- function(df){
  
  #remove unwanted models
  df <- df[!df$Model %in% c("WITCH 5.0"),]
  # remove negative carbon prices (TIAM-ECN case)
  df <- df[!(df$Variable == "Price|Carbon" & df$Value < 0),]
  
  #remove version from model names
  df$Model <- gsub("v\\d*\\.?\\d*\\.?\\d*", "", df$Model) # remove "v1.0.0"
  df$Model <- gsub("v\\d*\\.?\\d*", "", df$Model) # remove "v1.1"
  df$Model <- gsub(" \\d*\\.?\\d*", "", df$Model) # remove "1.1"
  df$Model <- gsub("\\s$*", "", df$Model) # remove trailing spaces
  df$Model <- gsub("\\_*$", "", df$Model) # remove trailing underscores
  
  #rename MESSAGE
  df[df$Model == "MESSAGEix-GLOBIOM",]$Model <- "MESSAGE"
  
  #Create Region aggregate for "OSeMBE v1.0.0" and "Euro-Calliope 2.0"
  `EU27 & UK` <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark",
                   "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy",
                   "Latvia", "Lithuania", "Luxembourg", "Malta", "The Netherlands", "Poland", "Portugal",
                   "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
                   "United Kingdom"
  )
  EU27regions <- `EU27 & UK`[`EU27 & UK` != "United Kingdom"]
  
  tmp <- df[(df$Model == "OSeMBE" & df$Region %in% `EU27 & UK`),]
  tmp <- tmp[!(tmp$Unit %in% c("EUR_2020/GJ")) ,]
  tmp <- tmp %>% group_by(Model,Scenario,Variable, Unit, Period) %>%
    summarize(Value = sum(Value))
  tmp$Region <- "EU27 & UK"
  tmp <- as.data.frame(tmp[,c(1,2,7,3,4,5,6)])
  
  df <- rbind(df,tmp)
  
  tmp <- df[(df$Model == "OSeMBE" & df$Region %in% EU27regions),]
  tmp <- tmp[!(tmp$Unit %in% c("EUR_2020/GJ")) ,]
  tmp <- tmp %>% group_by(Model,Scenario,Variable, Unit, Period) %>%
    summarize(Value = sum(Value))
  tmp$Region <- "EU27"
  tmp <- as.data.frame(tmp[,c(1,2,7,3,4,5,6)])
  
  df <- rbind(df,tmp)
  
  return(df)
}
