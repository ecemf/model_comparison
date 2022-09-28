
#cleaning up memory
#rm(list = ls())
#.rs.restartR()
#gc()

options <- list(
  updateData = TRUE,
  prepareData = TRUE,
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
    

# # EU28
# # prio-1 Pure carbon pricing
# 
# reportPath <- paste0("./output/charts_", dataFile$dateString, ".rds")
# loadedCharts <- readRDS(paste0("./output/", dataFile$dateString, "/charts_", reg, "_", scen,".rds"))
# render('./rmd/model_comparison.Rmd',output_file = paste0("../output/", dataFile$dateString, "/prio1_EU28_report_(4scen).html"), params = list(charts=loadedCharts, regionDisplayed="EU28", type="NPIhist"))


#df$Value <- round(df$Value,3)
# for tests
# plot <- "CO2 Emissions"
# regChosen <- "EU28"
# policyScen <- "NPIhist"
# plotList <- plotList[names(plotList)[c(1,2)]]
#scenType <- scenType[1]
# plot <- "CO2 emissions per source"
# origDf=df
# plotArgs=plotList[[plot]]
# region=regChosen
# scenarios=scenType[[policyScen]]
# scenName=policyScen
# # 
# data=d
# total=totalSum
# saveFolder=paste0("./output/charts/", policyScen , "/" , plot)
# scen=scenarios
# period=c(2020,2030,2050)
# title=""
# legend = list(nrow=NULL)
# min=0
# max=NULL
# labels=plotList[[plot]]$arg$labels
# pattern=plotList[[plot]]$arg$pattern
# facet = list(nrow=NULL,ncol=6)
# saveFile=FALSE
# saveFolder=""


# library(parallel)
# cl <- makeCluster(detectCores())
# matrix_of_sums <- parLapply(cl, 1:nrow(dataframe_stuff_that_needs_lookup_from_simulation), function(i)
#   rowSums(simulation_results[,colnames(simulation_results) %in% 
#                                dataframe_stuff_that_needs_lookup_from_simulation[i,]]))
# stopCluster(cl)
# ans <- Reduce("cbind", matrix_of_sums)

# 
# #select Regions
# if(regChosen=="EU27"){
#   currdf <- df[((df$Region %in% c("EU27", "Europe", "Europe (excl. Turkey)", "Europe (incl. Turkey)") & !(df$Model == "Euro-Calliope") ) | (df$Model == "MEESA" & df$Region == "EU27 & UK") | (df$Model == "Euro-Calliope" & df$Region == "EU27")),]
# } else if (regChosen=="EU28"){
#   currdf <- df[(df$Region %in% c("EU27 & UK", "Europe", "Europe (excl. Turkey)", "Europe (incl. Turkey)") & !(df$Model == "Euro-Calliope") ) | (df$Model == "Euro-Calliope" & df$Region == "Europe"),] 
# }
# #copy 2019 or 2008 Eurostat values to 2020
# if(regChosen=="EU27"){
#   eurostatLabel <- "2019 Eurostat"
#   tmp <- currdf[currdf$Scenario == " Eurostat" & currdf$Period==2019,]
#   tmp$Period <- 2020
#   tmp$Scenario <- " 2019 Eurostat"
#   currdf <- rbind(currdf[!(currdf$Scenario == " Eurostat"),], tmp)
# } else if(regChosen=="EU28"){
#   eurostatLabel <- "2018 Eurostat"
#   tmp <- currdf[currdf$Scenario == " Eurostat" & currdf$Period==2018,]
#   tmp$Period <- 2020
#   tmp$Scenario <- " 2018 Eurostat"
#   currdf <- rbind(currdf[!(currdf$Scenario == " Eurostat"),], tmp)
# }  
# #select scenarios
# currdf <- currdf[(currdf$Scenario %in% c(unique(currdf[currdf$Model == "reference",]$Scenario), scenType[[policyScen]])),]
# #return if plot needs a scenario not present in the data
# if(any(!(plotList[[plot]]$arg$scen %in% unique(currdf$Scenario)))){
#   print("not created: missing scenario.")
#   return()
# } 
# #overwrite save file option
# if(options$saveCharts) {
#   plotList[[plot]]$arg$saveFile <- TRUE
# } else {
#   plotList[[plot]]$arg$saveFile <- FALSE
# }
# #create plots
# if(plotList[[plot]]$type == "line"){
#   if((policyScen=="paradigmShift" || policyScen=="techConstraint") & (!("scen" %in% names(plotList[[plot]]$arg)))){
#     d <- filterData(data=currdf[!(currdf$Scenario %in% c(paste0(" ", eurostatLabel),"DIAG-NPI")),],plotName=plot)   # filtering data for plot
#     p <- do.call(linePlot, c(list(data=d,saveFolder=paste0("./output/charts/", policyScen , "/" , plot),scen=plotList[[plot]]$arg$scen[!(plotList[[plot]]$arg$scen=="DIAG-NPI")]),plotList[[plot]]$arg))
#   } else {
#     d <- filterData(data=currdf[!(currdf$Scenario %in% c(paste0(" ", eurostatLabel))),],plotName=plot)   # filtering data for plot  
#     p <- do.call(linePlot, c(list(data=d,saveFolder=paste0("./output/charts/", policyScen , "/" , plot)),plotList[[plot]]$arg))
#   }
# } else if (plotList[[plot]]$type == "bar"){
#   if((policyScen=="paradigmShift" || policyScen=="techConstraint") & (!("scen" %in% names(plotList[[plot]]$arg)))){
#     d <- filterData(data=currdf[!(currdf$Scenario %in% c("DIAG-NPI")),],plotName=plot)   # filtering data for plot
#     totalSum <- currdf[(currdf$Variable %in%  names(plotList[[plot]]$vars)),] %>%
#       group_by(Model,Scenario,Period) %>% summarize(Value = sum(Value),.groups="drop") %>% mutate(Variable="Total")
#     p <- do.call(barPlot, c(list(data=d,total=totalSum,saveFolder=paste0("./output/charts/", policyScen , "/" , plot),scen=plotList[[plot]]$arg$scen[!(plotList[[plot]]$arg$scen=="DIAG-NPI")]),plotList[[plot]]$arg))
#   } else {
#     d <- filterData(data=currdf,plotName=plot)    # filtering data for plot
#     totalSum <- currdf[(currdf$Variable %in%  names(plotList[[plot]]$vars)),] %>%
#       group_by(Model,Scenario,Period) %>% summarize(Value = sum(Value),.groups="drop") %>% mutate(Variable="Total")
#     p <- do.call(barPlot, c(list(data=d,total=totalSum,saveFolder=paste0("./output/charts/", policyScen , "/" , plot)),plotList[[plot]]$arg))
#   }
# }
# return(p)
#})

# for(reg in c("EU27","EU28")){
#   for(scen in names(scenType)){
#     saveRDS(g[[reg]][[scen]],paste0("./output/", dataFile$dateString, "/charts_", reg, "_", scen,".rds"))
#   }
# }
#full rds
#saveRDS(g,paste0("./output/", dataFile$dateString, "/charts.rds"))


  


 
  
  

  
  
  












