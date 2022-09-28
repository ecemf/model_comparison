
mostRecentData <- function(){
  files <- file.info(list.files(path="./data", pattern="^data_.*.csv", full.names = T))
  if (!(nrow(files))){
    stop('You need a valid csv file in the ./data folder with model data. You can set the option updateResults to TRUE to download the data directly from the iiasa database. Just set your credentials to the database also in the "credentials.json" file.')
  }
  out <- NULL
  out$path <- rownames(files)[which.max(files$mtime)]
  out$dateString <- gsub("./data/data_|.csv", "", out$path)
  out$date <- regmatches(out$dateString,regexpr("\\d{4}\\_\\d{2}\\_\\d{2}",out$dateString))
  return(out)
}

loadData <- function(filename){
  
  # load most recent data
  
  iiasa_df <- read.csv(filename,check.names=FALSE,stringsAsFactors=FALSE, dec = ".")
  
  renColNames <- c(
    "model"="Model",
    "scenario"="Scenario",
    "region"="Region",
    "variable"="Variable",
    "unit"="Unit",
    "year"="Period",
    "value"="Value" 
  )
  colnames(iiasa_df) <- renColNames[colnames(iiasa_df)]
  
  # removing unwanted scenarios
  scenNames <- c("DIAG-Base","DIAG-NPI","DIAG-C80-gr5","DIAG-C0to80-gr5","DIAG-C400-lin","DIAG-NZero","DIAG-C400-lin-LimBio","DIAG-C400-lin-LimCCS","DIAG-C400-lin-LimNuclear","DIAG-C400-lin-HighVRE","DIAG-C400-lin-HighElectrification","DIAG-C400-lin-HighH2","DIAG-C400-lin-ResidualFossil","DIAG-C400-lin-HighEff")
  iiasa_df <- iiasa_df[(iiasa_df$Scenario %in% scenNames),] 
  
  # load hist data
  histDf <- read.csv("./hist/hist.csv",check.names=FALSE,stringsAsFactors=FALSE, dec = ".")
  
  histDf <- histDf[! names(histDf) %in% c("")] # remove columns with no names
  
  histData <- histDf %>%
    gather(Period, Value, -c(1:5))
  histData$Value <- as.numeric(as.character(histData$Value))
  histData$Period <- as.integer(histData$Period)
  
  # filter
  histData <- histData[histData$Scenario %in% c("EU_ReferenceScenario_2020","James_IMF","Eurostat"),] #,"2019 UNFCCC","UNFCCC", "CEDS","2019 CEDS"
  
  #shortNames
  histData[histData$Scenario == "EU_ReferenceScenario_2020",]$Scenario <- "EU27Ref"
  histData[histData$Scenario == "James_IMF",]$Scenario <- "IMF"
  
  # add space in front to force position in stacked bar
  histData$Scenario <- paste0(" ", histData$Scenario)
  
  #merge data with hist
  df <- rbind(iiasa_df,histData)
  
  df$Value <- as.numeric(df$Value)
  df$Period <- as.integer(df$Period)
  
  # removing Na Values
  df <- df[!is.na(df$Value),]
  
  # #check for duplicated rows
  # dupl <- duplicated(df)
  # if(any(dupl)){
  #   print("duplicated rows")
  #   print(unique(df[dupl,c(1,2)]))
  # }
  
  return(df)
}
