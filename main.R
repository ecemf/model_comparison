
#cleaning up memory
#rm(list = ls())
#.rs.restartR()
#gc()

options <- list(
  updateData = TRUE,
  saveCharts = FALSE
)

#load required libraries
library(reticulate)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(svglite)
library(gtable)
library(ggplotify)
library(ggpattern)
library(quitte)

if(options$updateData){ # download new data
  print("downloading data from iiasa")
  source("./R/download_iiasa_db.R")
  download_iiasa_db()
}

#gc() # garbage collection

#prepareData
source("./R/loadData.R")
source("./R/manipulateModelResults.R")
source("./R/addExtraVariables.R")
  
dir.create("./data", showWarnings = FALSE) # create the data folder
dataFile <- mostRecentData()
rdsDataPath <- paste0("./data/data_", dataFile$dateString, ".rds")

if(file.exists(rdsDataPath)){
  print("loading previously calculated data")
  df <- readRDS(rdsDataPath)
} else {
  df <- loadData(dataFile$path)
  df <- manipulateModelResults(df)
  df <- addExtraVariables(df)
  #reorder Models
  modelsOrder <- c("Euro-Calliope", "IMAGE", "MEESA", "MESSAGE", "OSeMBE", "PROMETHEUS", "REMIND", "TIAM-ECN", "WITCH", "PRIMES") #,"PRIMES"
  df$Model <- factor(df$Model, levels = c("reference",modelsOrder))
  #reorder scenarios
  histScen <- unique(df[df$Model == "reference",]$Scenario)
  df <- df %>%
    mutate(Scenario = factor(Scenario, levels=c(histScen,c("DIAG-NPI", "DIAG-Base", "DIAG-C80-gr5", "DIAG-C0to80-gr5", "DIAG-C400-lin", "DIAG-NZero","DIAG-C400-lin-LimBio","DIAG-C400-lin-LimCCS","DIAG-C400-lin-LimNuclear","DIAG-C400-lin-HighVRE","DIAG-C400-lin-HighElectrification","DIAG-C400-lin-HighH2","DIAG-C400-lin-ResidualFossil","DIAG-C400-lin-HighEff"))))
  #df$Period <- as.integer(df$Period)
  print("saving calculated data")
  saveRDS(df,rdsDataPath)
}

#gc() # garbage collection

#loading plot scripts
source("./R/aesthetics.R") # load aesthetics
source("./R/plotFunctions.R") # load auxiliar plot functions

# reading json file that define all plots
plotList <- fromJSON("./json/plots.json")

#removing non used plots
nonUsedPlots <- c(
  "CO2 Intensity Electricity",
  "Industry and Industrial Processes CO2 Emissions",
  "Carbon Removal - Land Use",
  "Other Sectors Final Energy per carrier",
  "Residual Fossil - Fossil Final Energy",
  "Residual Fossil - Fossil Final Energy per sector"
)
for(nonUsedPLot in nonUsedPlots){
  plotList[[nonUsedPLot]] <- NULL
}


# creating all charts and saving them 
scenType <- list(
  "NPIhist"= c("DIAG-NPI", "DIAG-Base", "DIAG-C80-gr5", "DIAG-C0to80-gr5", "DIAG-C400-lin", "DIAG-NZero"),
  "NPIhist_(4scen)"= c("DIAG-NPI", "DIAG-Base", "DIAG-C400-lin", "DIAG-NZero"),
  "techConstraint"=c("DIAG-NPI", "DIAG-C400-lin", "DIAG-C400-lin-LimBio","DIAG-C400-lin-LimCCS","DIAG-C400-lin-LimNuclear"),
  "paradigmShift"=c("DIAG-NPI", "DIAG-NZero", "DIAG-C400-lin", "DIAG-C400-lin-HighVRE","DIAG-C400-lin-HighElectrification","DIAG-C400-lin-HighH2","DIAG-C400-lin-ResidualFossil","DIAG-C400-lin-HighEff")
  )

#reducing number of decimal places
df$Value <- round(df$Value,3)

dir.create("./output", showWarnings = FALSE) # create the output main folder
dir.create(paste0("./output/", dataFile$dateString), showWarnings = FALSE) # create the output folder

g <- NULL

#g <- lapply(c("EU27","EU28"), function(regChosen){
lapply(c("EU27","EU28"), function(regChosen){
  print(paste0("- ",regChosen))
#  gg <- lapply(names(scenType), function(policyScen){
  lapply(names(scenType), function(policyScen){
    print(paste0("-- ",policyScen))
    chartsPath <- paste0("./output/", dataFile$dateString, "/charts_", regChosen, "_", policyScen,".rds")
    if(file.exists(chartsPath)){
      print(paste0("--- loading charts from file."))
      #ggg <- readRDS(chartsPath)
      #names(ggg) <- names(plotList)
    } else {
      ggg <- lapply(names(plotList),function(plot){
        print(paste0("--- ",plot))
        #print(paste0("- ", regChosen, " - ",policyScen, " - ", plot))
        #overwrite save file option
        if(options$saveCharts) {
          plotList[[plot]]$arg$saveFile <- TRUE
        } else {
          plotList[[plot]]$arg$saveFile <- FALSE
        }
        #disable patterns
        plotList[[plot]]$arg$pattern <- NULL
        p <- createPlot(origDf=df,plotArgs=plotList[[plot]],plotName=plot,region=regChosen,scenarios=scenType[[policyScen]],scenName=policyScen,indivFilePath=paste0("./output/", dataFile$dateString, "/", regChosen, "/", policyScen))
        return(p)
      })
    names(ggg) <- names(plotList)
    saveRDS(ggg,paste0("./output/", dataFile$dateString, "/charts_", regChosen, "_", policyScen,".rds"))
    }
    return()
#    return(ggg)
  })
#  names(gg) <- names(scenType)
#  return(gg)
})
#names(g) <- c("EU27","EU28")

#gc() # garbage collection

############################################################

#Creating markdown documents

library(rmarkdown)

lapply(c("EU27","EU28"), function(regChosen){
  lapply(names(scenType), function(policyScen){
    rmdPath <- paste0("../output/", dataFile$dateString, "/", regChosen, "_", policyScen, "_", dataFile$date, ".html")
    if(!(file.exists(rmdPath))){
      chartsPath <- paste0("./output/", dataFile$dateString, "/charts_", regChosen, "_", policyScen, ".rds")
      chartsObject <- readRDS(chartsPath)
      render('./rmd/model_comparison.Rmd',
             output_file = rmdPath, 
             params = list(
               charts=chartsObject, 
               regionDisplayed=regChosen, 
               type=policyScen))
    }
  })
})
    
