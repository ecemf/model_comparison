
loadHistoricalData <- function(){
  
  if(file.exists("./historical/historical.rds")){
     hist <- readRDS("./historical/historical.rds")
     return(hist)
  }
  
  # data directly from the historical mif
  histMif <- piamInterfaces::convertHistoricalData(
    mif = "./historical/historical.mif",
    project = "ECEMF",
    regionMapping = "./mapping/regionmapping_historical.csv"
  )
  
  # data directly from the historical mif using different mappings
  addHist <- piamInterfaces::convertHistoricalData(
    mif = "./historical/historical.mif",
    project = "./mapping/additional_ECEMF_vars_mapping_from_historical.csv",
    regionMapping = "./mapping/regionmapping_historical.csv"
  )
  hist <- rbind(
    histMif %>% filter(!(variable %in% unique(addHist$variable))),
    addHist
  )
  
  #estimation of EU27 & UK after 2018 based on proportion of the base year
  EU28_eurostat <- left_join(
    hist %>%
      filter(model == "Eurostat",
             region %in% c("EU27 & UK (*)", "EU27 (*)")) %>%
      pivot_wider(names_from = region, values_from = value) %>%
      rename(EU27 = `EU27 (*)`, EU28 = `EU27 & UK (*)`)
    ,
    hist %>%
      filter(model == "Eurostat",
             region %in% c("EU27 & UK (*)", "EU27 (*)"),
             period == 2018) %>%
      pivot_wider(names_from = region, values_from = value) %>%
      mutate(rel = 1 + ((`EU27 & UK (*)` - `EU27 (*)`)/`EU27 (*)`)) %>%
      na.omit() %>%
      select(-period,-`EU27 & UK (*)`,-`EU27 (*)`)
  ) %>%
    group_by(model,scenario,period,variable,unit) %>%
    mutate(value = ifelse(is.na(EU28),  EU27*rel, EU28),
           region = "EU27 & UK (*)") %>%
    select(model, scenario, region, period, variable, unit, value) %>%
    na.omit()

  hist <- rbind(
    hist %>%
      filter(!(model == "Eurostat" & region == "EU27 & UK (*)")),
    EU28_eurostat)

  #additional historical data
  regMap <- read.csv2("mapping/regionmapping_historical.csv")
  regConvert <- regMap$project_region
  names(regConvert) <- regMap$REMIND
  regConvert <- c(regConvert,"EU27 & UK" = "EU27 & UK (*)")
  addHist2 <- read.csv("./historical/hist.csv",check.names=FALSE,stringsAsFactors=FALSE, dec = ".")
  addHist2 <- addHist2[! names(addHist2) %in% c("")]
  addHist2 <- addHist2 %>% 
    as.quitte() %>% 
    mutate(region = ifelse(region %in% names(regConvert), regConvert[region], region)) %>%
    filter(region %in% c("EU27 & UK (*)", "EU27 (*)")) %>%
    gather(period, value, -c(1:6)) %>%
    na.omit() %>%
    #filter(!is.na(Value)) %>%
    mutate(model = scenario,
           scenario = "historical",
           period = suppressWarnings(as.integer(period)),
           value=suppressWarnings(as.numeric(value))) %>% 
    na.omit()
  #filter addHist2 that are not in the historical mif already
  histItems <- hist %>% select(model, region, variable) %>% group_by(model, region, variable) %>% filter(row_number() == 1)
  addHist2 <- anti_join(addHist2,histItems,by=c("model", "region", "variable")) %>%
    na.omit()
  
  hist <- rbind(
    hist,
    addHist2
  )
  
  # remove wrong data
  #hist <- hist %>% filter(!(model == "IEA" & period == 2021)) #filter(!(model == "IEA" & region == "EU27 & UK (*)" & value == 0),)

  # # remove variable with very low values
  # varsToRemove <- c("Final Energy|Bunkers|Liquids|Biomass")
  # print(paste0("Removing variables with very low values: ", toString(varsToRemove)))
  # hist <- hist %>% filter(!(variable %in% varsToRemove))
  # 
  # # remove land use data as it is apparently using different data
  # varsToRemove <- c("Emissions|CH4|AFOLU|Land","Emissions|N2O|AFOLU|Land")
  # hist <- hist %>% filter(!(variable %in% varsToRemove))
   
  # remove "Energy Service|Transportation|Freight" as it apparently contains road freight values instead
  varsToRemove <- c("Energy Service|Transportation|Freight")
  hist <- hist %>% filter(!(variable %in% varsToRemove))
   
  # # remove "Gross Emissions|CO2|Industrial Processes"
  # varsToRemove <- c("Gross Emissions|CO2|Industrial Processes")
  # hist <- hist %>%
  #   filter(!(variable %in% varsToRemove))
  # # 
  
  #remove unused models and years
  hist <- hist %>% filter(period >= 2000,
                          period <= 2050,
                          !model %in% c("EDGE_SSP1", "EDGE_SSP2", "INNOPATHS", "HRE_Baseline", "HRE_ConvDecarb", "HRE_HeatRoadmap", "ARIADNE", "EU_ReferenceScenario_2016")
  )
  
  # remove CEDS source if UNFCCC is present for the same data
  hist <- hist %>% 
    group_by(region,variable) %>% 
    mutate(modelChar = as.character(model)) %>% 
    mutate(out=ifelse(grepl("CEDS",unique(pick(modelChar))) & grepl("UNFCCC",unique(pick(modelChar))), TRUE, FALSE)) %>%
    filter(!(out & model == "CEDS")) %>% 
    select(-out,-modelChar) %>% 
    ungroup() %>%
    as.data.frame()
  #anti_join(hist,tmp)
  
  #order historical data 
  histOrder <- c("UNFCCC", "Eurostat", "JRC", "Ember", "IRENA", "BP", "UBA", 
                 "IEA", "IEA WEO 2021 APS", "IEA WEO 2021 SDS", "IEA WEO 2021 SPS", "IEA ETP 2DS", "IEA ETP B2DS", "IEA ETP RTS",
                 "WDI", "AGEB", "EDGAR6", "James_IHME", "James_IMF", "James_WB", "EEA_WEM", "EEA_WAM", "EU_ReferenceScenario_2020")
  hist <- hist %>%
    mutate(model = factor(model, levels = c(intersect(histOrder,unique(hist$model)),setdiff(unique(hist$model),histOrder)), ordered = TRUE)) %>%
    arrange(model)
  
  saveRDS(hist,"./historical/historical.rds")
  
  return(hist)
}

  