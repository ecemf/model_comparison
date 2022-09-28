
addExtraVariables <- function(df){
  
  #Calculate extra variables
  for (model in unique(df$Model)){
    if (nrow(df[df$Variable %in% c("Secondary Energy|Electricity|Wind") & df$Model == model,]) == 0){
      tmp <- NULL
      tmp <- df[df$Variable %in% c("Secondary Energy|Electricity|Wind|Offshore", "Secondary Energy|Electricity|Wind|Onshore"),] %>%
        group_by(Model,Scenario,Region,Unit,Period) %>%
        summarize(Value = sum(Value, na.rm = TRUE),.groups="drop")
      tmp$Variable <- "Secondary Energy|Electricity|Wind"
      tmp <- as.data.frame(tmp[,c(1,2,3,7,4,5,6)])
      df <- rbind(df,tmp)
    }
  }
  
  #estimated primary energy biomass use
  efficiency_factor <- c(
    "Electricity"=2.5,
    "Heat"       =1,
    "Liquids"    =2,
    "Solids"     =1,
    "Gases"      =1.5,
    "Hydrogen"   =1.5
  )
  for(SEcarrier in c("Electricity","Heat","Liquids","Solids","Gases","Hydrogen")){
    tmp <- df[df$Variable == paste0("Secondary Energy|",SEcarrier,"|Biomass"),]
    tmp$Variable <- paste0("est|Primary Energy|Biomass|", SEcarrier)
    tmp$Value <- tmp$Value*efficiency_factor[SEcarrier]
    df <- rbind(df,tmp)
  }
  
  aggregateMap <- list(
    "Secondary Energy|non VRE" = c("Secondary Energy|Electricity|Curtailment", "Secondary Energy|Electricity|Biomass", "Secondary Energy|Electricity|Coal", "Secondary Energy|Electricity|Gas", "Secondary Energy|Electricity|Geothermal", "Secondary Energy|Electricity|Hydro", "Secondary Energy|Electricity|Hydrogen", "Secondary Energy|Electricity|Nuclear", "Secondary Energy|Electricity|Oil", "Trade|Secondary Energy|Electricity|Volume"),
    "Secondary Energy|VRE" = c("Secondary Energy|Electricity|Solar", "Secondary Energy|Electricity|Wind"),
    "Final Energy|Fossil" = c("Final Energy|Gases|Fossil","Final Energy|Liquids|Fossil","Final Energy|Solids|Fossil"),
    "Final Energy|Non-Energy Use|Fossil" = c("Final Energy|Non-Energy Use|Gases|Fossil","Final Energy|Non-Energy Use|Liquids|Fossil","Final Energy|Non-Energy Use|Solids|Fossil"),
    "Final Energy|Industry|Fossil"       = c("Final Energy|Industry|Gases|Fossil","Final Energy|Industry|Liquids|Fossil","Final Energy|Industry|Solids|Fossil"),
    "Final Energy|Residential and Commercial|Fossil" = c("Final Energy|Residential and Commercial|Gases|Fossil","Final Energy|Residential and Commercial|Liquids|Fossil","Final Energy|Residential and Commercial|Solids|Fossil"),
    "Final Energy|Transportation|Fossil" = c("Final Energy|Transportation|Gases|Fossil","Final Energy|Transportation|Liquids|Fossil","Final Energy|Transportation|Solids|Fossil"),
    "Final Energy|Bunkers|Fossil"        = c("Final Energy|Bunkers|Gases|Fossil","Final Energy|Bunkers|Liquids|Fossil","Final Energy|Bunkers|Solids|Fossil"),
    "Final Energy|Other Sector|Fossil"   = c("Final Energy|Other Sector|Gases|Fossil","Final Energy|Other Sector|Liquids|Fossil","Final Energy|Other Sector|Solids|Fossil"),
    "Primary Energy|Fossil" = c("Primary Energy|Coal","Primary Energy|Gas","Primary Energy|Oil"),
    "est|Primary Energy|Biomass|Electricity and Heat" = c("est|Primary Energy|Biomass|Electricity","est|Primary Energy|Biomass|Heat"),
    "est|Primary Energy|Biomass" = c("est|Primary Energy|Biomass|Electricity","est|Primary Energy|Biomass|Heat","est|Primary Energy|Biomass|Liquids","est|Primary Energy|Biomass|Solids","est|Primary Energy|Biomass|Gases","est|Primary Energy|Biomass|Hydrogen")
  )
  df <- df[!(df$Variable %in% names(aggregateMap)),] #remove aggregated variable alreayd reported
  for(var in names(aggregateMap)){
    tmp <- NULL
    tmp <- df[df$Variable %in% aggregateMap[[var]],] %>%
      group_by(Model,Scenario,Region,Unit,Period) %>%
      summarize(Value = sum(Value, na.rm = TRUE),.groups="drop")
    tmp$Variable <- var
    tmp <- as.data.frame(tmp[,c(1,2,3,7,4,5,6)])
    df <- rbind(df,tmp)
  }
  
  for (model in unique(df$Model)){
    if (nrow(df[df$Variable %in% c("Carbon Removal|Land Use") & df$Model == model,]) != 0){
      for(scen in unique(df$Scenario)){
        if (nrow(df[df$Variable %in% c("Carbon Removal|Land Use") & df$Model == model & df$Scenario == scen,]) != 0){
          df <- rbind(df,
                      df[df$Variable %in% c("Carbon Removal","Carbon Removal|Land Use") & df$Model == model  & df$Scenario == scen,] %>%
                        group_by(Model,Scenario,Region,Unit,Period) %>%
                        mutate(Value = Value - Value[Variable == 'Carbon Removal|Land Use']) %>% 
                        filter(Variable == "Carbon Removal") %>%
                        mutate(Variable = "Carbon Removal|Other")
          )
        }
      }
    }
  }
  
  GWP <- c("CO2"=1,"CH4"=28,"N2O"=265)
  
  df <- rbind(df,
              df[df$Variable %in% c("Emissions|CH4"),] %>%
                group_by(Model,Scenario,Region,Unit,Period) %>%
                mutate(Value = Value * GWP["CH4"], Variable = "Emissions|Kyoto Gases|CH4")
  )
  
  df <- rbind(df,
              df[df$Variable %in% c("Emissions|N2O"),] %>%
                group_by(Model,Scenario,Region,Unit,Period) %>%
                mutate(Value = Value * GWP["N2O"]/1000, Variable = "Emissions|Kyoto Gases|N2O")
  )
  
  #adding share variables
  df_quitte <- as.quitte(df)
  addedVars <- df_quitte %>% calc_addVariable("`Final Energy|Hydrogen Share`" = "`Final Energy|Hydrogen` / `Final Energy`", units = "%", only.new=T)
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Industry|Hydrogen Share`" = "`Final Energy|Industry|Hydrogen` / `Final Energy|Industry`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Residential and Commercial|Hydrogen Share`" = "`Final Energy|Residential and Commercial|Hydrogen` / `Final Energy|Residential and Commercial`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Transportation|Hydrogen Share`" = "`Final Energy|Transportation|Hydrogen` / `Final Energy|Transportation`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Electricity Share`" = "`Final Energy|Electricity` / `Final Energy`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Industry|Electricity Share`" = "`Final Energy|Industry|Electricity` / `Final Energy|Industry`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Residential and Commercial|Electricity Share`" = "`Final Energy|Residential and Commercial|Electricity` / `Final Energy|Residential and Commercial`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Final Energy|Transportation|Electricity Share`" = "`Final Energy|Transportation|Electricity` / `Final Energy|Transportation`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Secondary Energy|VRE Share`" = "`Secondary Energy|VRE` / `Secondary Energy|Electricity`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Primary Energy|Fossil Share`" = "`Primary Energy|Fossil` / `Primary Energy`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Primary Energy|Biomass Share`" = "`Primary Energy|Biomass` / `Primary Energy`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`Secondary Energy|Electricity|Nuclear Share`" = "`Secondary Energy|Electricity|Nuclear` / `Secondary Energy|Electricity`", units = "%", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`CO2 Intensity|Electricity`" = "`Emissions|CO2|Energy|Supply|Electricity` / `Final Energy|Electricity`", units = "Mt CO2/yr per EJ/yr", only.new=T))
  addedVars <- rbind(addedVars,df_quitte %>% calc_addVariable("`est|Primary Energy|Biomass|Electricity and Heat Share`" = "`est|Primary Energy|Biomass|Electricity and Heat` / `est|Primary Energy|Biomass`", units = "Mt CO2/yr per EJ/yr", only.new=T))
  colnames(addedVars) = colnames(df)
  df <- rbind(df,addedVars)

  return(df)
}
