#!/usr/bin/env Rscript

# to store your iiasa credentials, please run once pyam.iiasa.set_config("login"", "password"")

execute <- list(
  download =  TRUE, # download data from iiasa, filter it and create extended data sets
  report = list( # reports
    individualCharts =  TRUE, 
    historicalCharts =  TRUE,
    variablesSummations =  TRUE,
    modelChanges = TRUE,
    T1p3scenarios = TRUE,
    policyBrief = TRUE
  )
)

# load required libraries
library(reticulate)
library(quitte)
library(openxlsx)
library(tidyr)
library(dplyr)
library(piamInterfaces)
library(mip)
library(ggplot2)
library(rmarkdown)
library(graphics)
library(fs)
library(RColorBrewer)

# library(jsonlite)
# library(gridExtra)
# library(grid)
# library(svglite)
# library(gtable)
# library(ggplotify)
# library("ggdark")
# library(kableExtra)
# library(ggpattern)

# load util functions
source("./R/utils.R")

#most recent data
#time <- basename(sort(list.dirs(path = "./data", full.names = TRUE, recursive = FALSE), decreasing = T)[1])

# download data 
scen <- list(
  "data" = c("DIAG-Base", "DIAG-NPI", "DIAG-C80-gr5", "DIAG-C0to80-gr5", "DIAG-C400-lin", "DIAG-NZero", "DIAG-C400-lin-LimBio", "DIAG-C400-lin-LimCCS", "DIAG-C400-lin-LimNuclear", "DIAG-C400-lin-HighVRE", "DIAG-C400-lin-HighElectrification", "DIAG-C400-lin-HighElec-Supply", "DIAG-C400-lin-HighH2", "DIAG-C400-lin-ResidualFossil", "DIAG-C400-lin-HighEff",
             "WP1 NPI", "WP1 NetZero", "WP1 NetZero-LimBio", "WP1 NetZero-LimCCS", "WP1 NetZero-LimNuc", "WP1 NetZero-ElecPush", "WP1 NetZero-H2Push", "WP1 NetZero-SynfPush", "WP1 NetZero-HighEfficiency",
             "EMFECEMF_NPI","EMFECEMF_NZero",
             "WP5 Default-650", "WP5 Default-1150", "WP5 Russia-650", "WP5 Russia-1150","WP5 EastWest-650", "WP5 EastWest-1150", "WP5 EastWestTech-650", "WP5 EastWestTech-1150",
             "WP5 Base", "WP5 OPT-MIX", "WP5 RAP-MIX", "WP5 OPT-CP", "WP5 RAP-CP", "WP5 OPT-REG", "WP5 RAP-REG", "WP5 OPT-MIX-LimBio", "WP5 RAP-MIX-LimBio", "WP5 OPT-CP-LimBio", "WP5 RAP-CP-LimBio", "WP5 OPT-REG-LimBio", "WP5 RAP-REG-LimBio", "WP5 OPT-MIX-LimCCS", "WP5 RAP-MIX-LimCCS", "WP5 OPT-CP-LimCCS", "WP5 RAP-CP-LimCCS", "WP5 OPT-REG-LimCCS", "WP5 RAP-REG-LimCCS", "WP5 OPT-MIX-LimNuc", "WP5 RAP-MIX-LimNuc", "WP5 OPT-CP-LimNuc", "WP5 RAP-CP-LimNuc", "WP5 OPT-REG-LimNuc", "WP5 RAP-REG-LimNuc", "WP5 OPT-MIX-LimRES", "WP5 RAP-MIX-LimRES", "WP5 OPT-CP-LimRES", "WP5 RAP-CP-LimRES", "WP5 OPT-REG-LimRES", "WP5 RAP-REG-LimRES"
  ),
  "NZero" = c("WP1 NetZero")
)
if(execute$download){
  source("./R/downloadData.R")
  time <- format(Sys.time(), "%Y_%m_%d_%H.%M.%S")
  downloadMetaData(outputPath = paste0("./data/", time))
  downloadData(database = "ecemf_internal", outputPath = paste0("./data/", time), scenList = scen, regions=c("EU27 & UK (*)", "EU27 (*)")) # download new data
  
  source("./R/createExtendedData.R")
  for (dir in list.dirs(path = "./data", full.names = TRUE, recursive = FALSE)){
    # filtering previously downloaded data
    #downloadData(database = "ecemf_internal", outputPath = paste0("./data/", basename(dir)), scenList = list("NZero" = c("WP1 NetZero")), regions=c("EU27 & UK (*)", "EU27 (*)"))
    # creating extended data sets for NZero runs 
    createExtendedData(path = dir, scenList = list("NZero" = "WP1 NetZero")) # create missing extended data sets
  }
  #create extended data for all scenarios in latest download  
  latestDir <- sort(list.dirs(path = "./data", full.names = TRUE, recursive = FALSE), decreasing = T)[1]
  createExtendedData(path = dir, scenList = scen)
}

####################################
########## Create Reports ########## 
####################################

#gc() # garbage collection cleaning to free memory
#rm(list = ls()) # remove all objects in the R environment
#.rs.restartR()

if(any(unlist(execute$report))){
  # load data
  source("./R/loadData.R")
  latestData <- loadData(scenList = scen) # data$origData, data$extData
  
  # load historical data
  source("./R/historicalData.R")
  hist <- loadHistoricalData()
  
  # find latest data folder and define output folder
  dataFolder <- sort(list.dirs(path = "./data", full.names = TRUE, recursive = FALSE), decreasing = T)[1]
  outFolder <- paste0("./output/", basename(dataFolder))
  dir.create(outFolder, recursive = TRUE, showWarnings = FALSE)
}

# create individual charts reports
if(execute$report$individualCharts){
  source("./reports/individualCharts.R")
  createIndividualChartsReport(data=latestData$extData$NZero %>%
                                 filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                        period >= 2000, period <= 2050,
                                        scenario == "WP1 NetZero"),
                               histData = hist %>% 
                                 filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                        period >= 2000, period <= 2050),
                               outputFolder = outFolder)
  #createIndividualChartsReport(data=latestData$extData$NZero %>% filter(region %in% c("EU27 & UK (*)", "EU27 (*)"), period >= 2000, period <= 2050), varList = list("Emissions" = list("CO2" = list("main" = c("Energy and Industrial Processes" = "Emissions|CO2|Energy and Industrial Processes")))), histData = hist %>% filter(region %in% c("EU27 & UK (*)", "EU27 (*)"), period >= 2000, period <= 2050), outputFolder = outFolder) # for text purposes
}

# create historical charts reports
if(execute$report$historicalCharts){
  source("./reports/historicalCharts.R")
  createHistoricalChartsReport(data=latestData$extData$NZero %>%
                                 filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                        period >= 2000, period <= 2050,
                                        scenario == "WP1 NetZero"),
                               histData = hist %>% 
                                 filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                        period >= 2000, period <= 2050),
                               outputFolder = outFolder)
}

# create summation checks
if(execute$report$variablesSummations){
  source("./reports/variablesSummations.R")
  createVariablesSummationsReport(data=latestData$origData$NZero %>%
                                    filter(region == "EU27 & UK (*)",
                                           period >= 2000, period <= 2050,
                                           scenario == "WP1 NetZero"),
                                 outputFolder = outFolder)
}

# Create model changes reports
if(execute$report$modelChanges){
  fullExtData <- loadData(scenList = list("NZero" = "WP1 NetZero"), type = "all", source = "extData")
  filterTable <- fullExtData$extData$NZero %>%
    filter(region %in% c("EU27 & UK (*)", "EU27 (*)")) %>%
    na.omit() %>%
    #group_by(model, scenario, region, version) %>%
    summarize(totalSum = sum(value), .by = c(model, scenario, region, version)) %>%
    arrange(scenario,region,model,version) %>% 
    mutate(variation=(totalSum - lag(totalSum))) %>%
    filter(!variation == 0 | is.na(variation)) %>%
    select(model, scenario, region, version)
  filteredData <- fullExtData$extData$NZero %>%
    filter(region %in% c("EU27 & UK (*)", "EU27 (*)")) %>%
    right_join(filterTable, by = join_by(model, scenario, region, version)) 
  source("./reports/modelChanges.R")
  for(m in unique(filteredData$model)){
    df <- filteredData %>% 
      filter(model == m, 
             period >= 2000, period <= 2050)
    createModelChangesChartsReport(data=df,
                                   histData = hist %>% 
                                     filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                            period >= 2000, period <= 2050),
                                   outputFile = paste0(".", outFolder, "/model_changes - ", m, " - ", unique(df$scenario), " - " , toString(cleanFileName(c("EU27 & UK (*)", "EU27 (*)"))), ".html"),
                                   outputFolder = outFolder)
  }
}

# Create task 1.3 scenarios report
if(execute$report$T1p3scenarios){
  source("./reports/T1p3 scenarios.R")
  createT1p3scenariosReport(data=latestData$extData$data %>%
                  filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                         period >= 2000, period <= 2050,
                         scenario %in% c("WP1 NPI", "WP1 NetZero", "WP1 NetZero-LimBio", "WP1 NetZero-LimCCS", "WP1 NetZero-LimNuc")),
                histData = hist %>% 
                  filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                         period >= 2000, period <= 2050),
                outputFolder = outFolder)
}


# Create policy brief report
if(execute$report$policyBrief){
  source("./reports/policyBrief.R")
  createPolicyBriefReport(data=latestData$extData$data %>%
                              filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                     period >= 2000, period <= 2050,
                                     scenario %in% c("WP1 NPI", "WP1 NetZero", "WP1 NetZero-LimBio", "WP1 NetZero-LimCCS", "WP1 NetZero-LimNuc")),
                            histData = hist %>% 
                              filter(region %in% c("EU27 & UK (*)", "EU27 (*)"),
                                     period >= 2000, period <= 2050),
                            outputFolder = outFolder)
}