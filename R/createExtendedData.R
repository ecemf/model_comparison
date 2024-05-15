

createExtendedData <- function(path, scenList = list("nzero" = "WP1 NetZero")){
  
  filenameSuffix <- basename(path)
  
  for(group in names(scenList)){
    if(!file.exists(paste0(path, "/processed/", group,"_" ,filenameSuffix ,"_extended.rds"))){
      print(paste0("Additional calculated data for ", group, " data."))
      data <- readRDS(paste0(path, "/processed/", group,"_" ,filenameSuffix ,".rds"))
      extData <- addExtraVariables(data)
      saveRDS(extData,paste0(path, "/processed/", group,"_" ,filenameSuffix ,"_extended.rds"))
    }
  }
  
}

addExtraVariables <- function(df){
  
  #additional industry plus non-energy fe variables
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Electricity`" = "`Final Energy|Industry|Electricity`", units = "EJ/yr", completeMissing=T) #+ `Final Energy|Non-Energy Use|Electricity`
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Heat`" = "`Final Energy|Industry|Heat`", units = "EJ/yr", completeMissing=T) #+ `Final Energy|Non-Energy Use|Heat`
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Hydrogen`" = "`Final Energy|Industry|Hydrogen` + `Final Energy|Non-Energy Use|Hydrogen`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Gases`" = "`Final Energy|Industry|Gases` + `Final Energy|Non-Energy Use|Gases`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Gases|Biomass`" = "`Final Energy|Industry|Gases|Biomass` + `Final Energy|Non-Energy Use|Gases|Biomass`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Gases|Electricity`" = "`Final Energy|Industry|Gases|Electricity` + `Final Energy|Non-Energy Use|Gases|Electricity`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Gases|Fossil`" = "`Final Energy|Industry|Gases|Fossil` + `Final Energy|Non-Energy Use|Gases|Fossil`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Liquids`" = "`Final Energy|Industry|Liquids` + `Final Energy|Non-Energy Use|Liquids`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Liquids|Biomass`" = "`Final Energy|Industry|Liquids|Biomass` + `Final Energy|Non-Energy Use|Liquids|Biomass`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Liquids|Electricity`" = "`Final Energy|Industry|Liquids|Electricity` + `Final Energy|Non-Energy Use|Liquids|Electricity`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Liquids|Fossil`" = "`Final Energy|Industry|Liquids|Fossil` + `Final Energy|Non-Energy Use|Liquids|Fossil`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Solids`" = "`Final Energy|Industry|Solids` + `Final Energy|Non-Energy Use|Solids`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Solids|Biomass`" = "`Final Energy|Industry|Solids|Biomass` + `Final Energy|Non-Energy Use|Solids|Biomass`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Solids|Fossil`" = "`Final Energy|Industry|Solids|Fossil` + `Final Energy|Non-Energy Use|Solids|Fossil`", units = "EJ/yr", completeMissing=T)
  #df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Other`" = "`Final Energy|Industry|Other` + `Final Energy|Non-Energy Use|Other`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use`" = "`Final Energy|Industry` + `Final Energy|Non-Energy Use`", units = "EJ/yr", completeMissing=T)
  
  #additional Transportation (w/ bunkers)
  df <- rbind(df %>% filter(variable != "Final Energy|Transportation (w/ bunkers)"), calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)`" = "`Final Energy|Transportation` + `Final Energy|Bunkers`", units = "EJ/yr", completeMissing=T, only.new = T))
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Electricity`" = "`Final Energy|Transportation|Electricity` + `Final Energy|Bunkers|Electricity`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Hydrogen`" = "`Final Energy|Transportation|Hydrogen` + `Final Energy|Bunkers|Hydrogen`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Gases`" = "`Final Energy|Transportation|Gases` + `Final Energy|Bunkers|Gases`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Gases|Biomass`" = "`Final Energy|Transportation|Gases|Biomass` + `Final Energy|Bunkers|Gases|Biomass`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Gases|Electricity`" = "`Final Energy|Transportation|Gases|Electricity` + `Final Energy|Bunkers|Gases|Electricity`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Gases|Fossil`" = "`Final Energy|Transportation|Gases|Fossil` + `Final Energy|Bunkers|Gases|Fossil`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Liquids`" = "`Final Energy|Transportation|Liquids` + `Final Energy|Bunkers|Liquids`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Liquids|Biomass`" = "`Final Energy|Transportation|Liquids|Biomass` + `Final Energy|Bunkers|Liquids|Biomass`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Liquids|Electricity`" = "`Final Energy|Transportation|Liquids|Electricity` + `Final Energy|Bunkers|Liquids|Electricity`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Liquids|Fossil`" = "`Final Energy|Transportation|Liquids|Fossil` + `Final Energy|Bunkers|Liquids|Fossil`", units = "EJ/yr", completeMissing=T)
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Other`" = "`Final Energy|Transportation|Other` + `Final Energy|Bunkers|Other`", units = "EJ/yr", completeMissing=T)
  
  #additional share variables
  df <- calc_addVariable(df, "`Final Energy|Hydrogen Share`" = "`Final Energy|Hydrogen` / `Final Energy`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Industry|Hydrogen Share`" = "`Final Energy|Industry|Hydrogen` / `Final Energy|Industry`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Residential and Commercial|Hydrogen Share`" = "`Final Energy|Residential and Commercial|Hydrogen` / `Final Energy|Residential and Commercial`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Transportation|Hydrogen Share`" = "`Final Energy|Transportation|Hydrogen` / `Final Energy|Transportation`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Electricity Share`" = "`Final Energy|Electricity` / `Final Energy`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Industry|Electricity Share`" = "`Final Energy|Industry|Electricity` / `Final Energy|Industry`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Residential and Commercial|Electricity Share`" = "`Final Energy|Residential and Commercial|Electricity` / `Final Energy|Residential and Commercial`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Transportation|Electricity Share`" = "`Final Energy|Transportation|Electricity` / `Final Energy|Transportation`", units = "%")
  df <- calc_addVariable(df, "`Secondary Energy|VRE Share`" = "(`Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind`) / `Secondary Energy|Electricity`", units = "%")
  df <- calc_addVariable(df, "`Primary Energy|Fossil Share`" = "`Primary Energy|Fossil` / `Primary Energy`", units = "%")
  df <- calc_addVariable(df, "`Primary Energy|Biomass Share`" = "`Primary Energy|Biomass` / `Primary Energy`", units = "%")
  df <- calc_addVariable(df, "`Secondary Energy|Electricity|Nuclear Share`" = "`Secondary Energy|Electricity|Nuclear` / `Secondary Energy|Electricity`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Electricity Share`" = "`Final Energy|Industry and Non-Energy Use|Electricity` / `Final Energy|Industry and Non-Energy Use`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Hydrogen Share`" = "`Final Energy|Industry and Non-Energy Use|Hydrogen` / `Final Energy|Industry and Non-Energy Use`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Electricity Share`" = "`Final Energy|Transportation (w/ bunkers)|Electricity` / `Final Energy|Transportation (w/ bunkers)`", units = "%")
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Hydrogen Share`" = "`Final Energy|Transportation (w/ bunkers)|Hydrogen` / `Final Energy|Transportation (w/ bunkers)`", units = "%")
  
  #additional VRE and fossil aggregated variables
  df <- calc_addVariable(df, "`Secondary Energy|non VRE`" = "`Secondary Energy|Electricity|Curtailment` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Coal` + `Secondary Energy|Electricity|Gas` + `Secondary Energy|Electricity|Geothermal` + `Secondary Energy|Electricity|Hydro` + `Secondary Energy|Electricity|Hydrogen` + `Secondary Energy|Electricity|Nuclear` + `Secondary Energy|Electricity|Oil` + `Trade|Secondary Energy|Electricity|Volume`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Secondary Energy|VRE`" = "`Secondary Energy|Electricity|Solar` + `Secondary Energy|Electricity|Wind`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Fossil`" = "`Final Energy|Gases|Fossil` + `Final Energy|Liquids|Fossil` + `Final Energy|Solids|Fossil`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Non-Energy Use|Fossil`" = "`Final Energy|Non-Energy Use|Gases|Fossil` + `Final Energy|Non-Energy Use|Liquids|Fossil` + `Final Energy|Non-Energy Use|Solids|Fossil`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Industry|Fossil`" = "`Final Energy|Industry|Gases|Fossil` + `Final Energy|Industry|Liquids|Fossil` + `Final Energy|Industry|Solids|Fossil`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Residential and Commercial|Fossil`" = "`Final Energy|Residential and Commercial|Gases|Fossil` + `Final Energy|Residential and Commercial|Liquids|Fossil` + `Final Energy|Residential and Commercial|Solids|Fossil`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Transportation|Fossil`" = "`Final Energy|Transportation|Gases|Fossil` + `Final Energy|Transportation|Liquids|Fossil`", units = "EJ/yr") # + `Final Energy|Transportation|Solids|Fossil`
  df <- calc_addVariable(df, "`Final Energy|Bunkers|Fossil`" = "`Final Energy|Bunkers|Gases|Fossil` + `Final Energy|Bunkers|Liquids|Fossil`", units = "EJ/yr") #  + `Final Energy|Bunkers|Solids|Fossil`
  df <- calc_addVariable(df, "`Final Energy|Industry and Non-Energy Use|Fossil`" = "`Final Energy|Industry and Non-Energy Use|Gases|Fossil` + `Final Energy|Industry and Non-Energy Use|Liquids|Fossil` + `Final Energy|Industry and Non-Energy Use|Solids|Fossil`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Transportation (w/ bunkers)|Fossil`" = "`Final Energy|Transportation (w/ bunkers)|Gases|Fossil` + `Final Energy|Transportation (w/ bunkers)|Liquids|Fossil`", units = "EJ/yr")
  df <- calc_addVariable(df, "`Final Energy|Other Sector|Fossil`" = "`Final Energy|Other Sector|Gases|Fossil` + `Final Energy|Other Sector|Liquids|Fossil` + `Final Energy|Other Sector|Solids|Fossil`", units = "EJ/yr")
  #df <- calc_addVariable(df, "`Primary Energy|Fossil`" = "`Primary Energy|Coal` + `Primary Energy|Gas` + `Primary Energy|Oil`", units = "EJ/yr")
  
  # add variables possible to be calculated from other two variables
  df <- piamInterfaces::fillSummationPairs(df, "ECEMF")
  
  #estimated primary energy biomass use
  efficiency_factor <- c(
    "Secondary Energy|Electricity|Biomass"=2.5,
    "Secondary Energy|Heat|Biomass"       =1,
    "Secondary Energy|Liquids|Biomass"    =2,
    "Secondary Energy|Solids|Biomass"     =1,
    "Secondary Energy|Gases|Biomass"      =1.5,
    "Secondary Energy|Hydrogen|Biomass"   =1.5
  )
  df <- rbind(df,
              df %>%
                filter(variable %in% c(
                  "Secondary Energy|Electricity|Biomass",
                  "Secondary Energy|Heat|Biomass",
                  "Secondary Energy|Liquids|Biomass",
                  "Secondary Energy|Solids|Biomass",
                  "Secondary Energy|Gases|Biomass",
                  "Secondary Energy|Hydrogen|Biomass")
                ) %>%
                mutate(value = efficiency_factor[variable]*value) %>%
                mutate(variable = paste0("est|Primary Energy|Biomass|",gsub("Secondary Energy\\||\\|Biomass","",variable)))
  )
  #df <- calc_addVariable(df, "`est|Primary Energy|Biomass|Electricity and Heat`" = "`est|Primary Energy|Biomass|Electricity` + `est|Primary Energy|Biomass|Heat`", units = "EJ/yr")
  #df <- calc_addVariable(df, "`est|Primary Energy|Biomass`" = "`est|Primary Energy|Biomass|Electricity` + `est|Primary Energy|Biomass|Heat` + `est|Primary Energy|Biomass|Liquids` + `est|Primary Energy|Biomass|Solids` + `est|Primary Energy|Biomass|Gases` + `est|Primary Energy|Biomass|Hydrogen`", units = "EJ/yr")
  #df <- calc_addVariable(df, "`est|Primary Energy|Biomass|Electricity and Heat Share`" = "`est|Primary Energy|Biomass|Electricity and Heat` / `est|Primary Energy|Biomass`", units = "Mt CO2/yr per EJ/yr")
  
  #additional N2O and CH4 emission variables in GHG
  GWP <- c("CO2"=1,"CH4"=28,"N2O"=265)
  df <- rbind(df, df %>%
                filter(variable == "Emissions|CH4") %>%
                mutate(value = value * GWP["CH4"],
                       variable = "Emissions|Kyoto Gases|CH4",
                       unit = "Mt CO2e/yr"))
  df <- rbind(df, df %>%
                filter(variable == "Emissions|N2O") %>%
                mutate(value = value * GWP["N2O"]/1000,
                       variable = "Emissions|Kyoto Gases|N2O",
                       unit = "Mt CO2e/yr")) 
  
  df <- df %>% na.omit()
  
  return(df)
}
