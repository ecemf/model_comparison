
# function to create individual variable line chart
individualLineChart <- function(d, h, var){
  
  model.colors <- c(
    #model
    "IMAGE" = "#00ffff",
    "Euro-Calliope" = "#c0c0c0",
    "MEESA" = "#ff00ff",
    "MESSAGE" = "#800080",
    "OSeMBE" = "#a52a2a",
    "PRIMES" = "#0000ff",
    "PROMETHEUS" = "#ffd900",
    "REMIND" = "#ff6347",
    "LIMES" = "#ffaf47",
    "TIAM-ECN" = "#4682b4",
    "WITCH" = "#228b22"
  )
  #hist.colors <- plotstyle(as.vector(unique(hist$model)))
  
  o <- list()
  o[["var"]] <- var
  
  currData <- d %>% filter(variable == var) 
  currh <- h %>% filter(variable == var) 
  h.colors <- gray.colors(length(unique(currh$model)), start = 0, end = 0.7)
  names(h.colors) <- unique(currh$model)
  h.shape <- seq(15,15+length(unique(currh$model))-1)
  names(h.shape) <- unique(currh$model)
  h.linetype <- seq(1,length(unique(currh$model)))
  names(h.linetype) <- unique(currh$model)
  
  o[["chart"]] <- ggplot(data = rbind(currData,currh), aes(x = period, y = value, color = model)) +
    geom_hline(yintercept=0, color = "black", linewidth=1, alpha = 0.5) +
    geom_vline(xintercept=2020, color = "black", linewidth=1, alpha = 0.5,linetype="dashed") +
    geom_line(data=currData, aes( color = model), linewidth = 1) +
    geom_point(data=currh, aes(color = model, fill = model, shape = model), size = 2.5, alpha = 0.8) +
    geom_line(data=currh, aes( color = model, linetype = model), linewidth = 0.7, alpha = 0.7) +
    scale_fill_manual("References", values = h.colors, guide = guide_legend(override.aes = list(colour = h.colors))) +
    scale_shape_manual("References", values = h.shape) +
    scale_linetype_manual("References", values = h.linetype) +
    scale_color_manual("Models",
                       values = c(h.colors, model.colors),
                       breaks = unique(d$model),
                       guide = guide_legend(order = 1, title.position = "top", ncol = 1, override.aes = list(shape = NA))
    ) +
    theme_minimal(base_size = 16) + 
    theme(text=element_text(family="sans")) +
    xlab("Year") +
    ylab(as.character(unique(d %>% filter(variable == var) %>% pull(unit)))) +
    ggtitle(var) +
    theme(legend.key.width = unit(1.5, 'cm')) +
    { if("%" %in% unique(currData$unit)) scale_y_continuous(labels = scales::percent) } +
    { if("%" %in% unique(currData$unit))expand_limits(y = c(0,1)) } +
    expand_limits(x = 2000) + 
    theme(plot.background = element_rect(fill = 'white', colour = 'white'))
  
  return(o)
}

createIndividualChartsReport <- function(data, histData, outputFolder, varList=NULL, saveCharts=TRUE){
  
  if(is.null(varList)){
    # Variables list to create charts
    varList <- list(
      "Emissions" = list(
        "Kyoto Gases" = c("Total" = "Emissions|Kyoto Gases", "Energy and Industrial Processes" = "Emissions|Kyoto Gases|Energy and Industrial Processes", "Energy" = "Emissions|Kyoto Gases|Energy", "Industrial Processes" = "Emissions|Kyoto Gases|Industrial Processes", "AFOLU" = "Emissions|Kyoto Gases|AFOLU", "Land" = "Emissions|Kyoto Gases|AFOLU|Land", "Agriculture" = "Emissions|Kyoto Gases|AFOLU|Agriculture", "Waste" = "Emissions|Kyoto Gases|Waste", "Other" = "Emissions|Kyoto Gases|Other") #, "Industry and Industrial Processes" = "Emissions|Kyoto Gases|Industry and Industrial Processes"
        ,
        "CO2" = list(
          "main" = c("Total" = "Emissions|CO2", "Energy and Industrial Processes" = "Emissions|CO2|Energy and Industrial Processes", "Energy" = "Emissions|CO2|Energy", "Industrial Processes" = "Emissions|CO2|Industrial Processes", "AFOLU" = "Emissions|CO2|AFOLU", "Land" = "Emissions|CO2|AFOLU|Land", "Other" = "Emissions|CO2|Other"), #, "Agriculture" = "Emissions|CO2|AFOLU|Agriculture", "Waste" = "Emissions|CO2|Waste"
          "energy demand" = c("Total" = "Emissions|CO2|Energy|Demand", "Industry" = "Emissions|CO2|Energy|Demand|Industry", "Transport" = "Emissions|CO2|Energy|Demand|Transportation", "Bunkers" = "Emissions|CO2|Energy|Demand|Bunkers", "Buildings" = "Emissions|CO2|Energy|Demand|Residential and Commercial", "AFOFI" = "Emissions|CO2|Energy|Demand|AFOFI", "Other Sector" = "Emissions|CO2|Energy|Demand|Other Sector"),
          "energy supply" = c("Total" = "Emissions|CO2|Energy|Supply", "Electricity and Heat" = "Emissions|CO2|Energy|Supply|Electricity and Heat", "Electricity" = "Emissions|CO2|Energy|Supply|Electricity", "Heat" = "Emissions|CO2|Energy|Supply|Heat", "Hydrogen" = "Emissions|CO2|Energy|Supply|Hydrogen", "Gases" = "Emissions|CO2|Energy|Supply|Gases", "Liquids" = "Emissions|CO2|Energy|Supply|Liquids" , "Solids" = "Emissions|CO2|Energy|Supply|Solids", "Other" = "Emissions|CO2|Energy|Supply|Other"),
          "extra" = c("Transport w/ bunkers" = "Emissions|CO2|Energy|Demand|Transportation (w/ bunkers)", "International Aviation" = "Emissions|CO2|Energy|Demand|Bunkers|International Aviation", "International Shipping" = "Emissions|CO2|Energy|Demand|Bunkers|International Shipping", "Rail" = "Emissions|CO2|Energy|Demand|Transportation|Rail", "Commercial" = "Emissions|CO2|Energy|Demand|Commercial", "Residential" = "Emissions|CO2|Energy|Demand|Residential")	#  "Emissions|CO2|Industry and Industrial Processes"
        ),
        "CH4" = list(
          "main" = c("Total" = "Emissions|CH4", "Energy" = "Emissions|CH4|Energy", "AFOLU" = "Emissions|CH4|AFOLU", "Land" = "Emissions|CH4|AFOLU|Land", "Agriculture" = "Emissions|CH4|AFOLU|Agriculture", "Waste" = "Emissions|CH4|Waste", "Other" = "Emissions|CH4|Other"),
          "energy demand" = c("Industry" = "Emissions|CH4|Energy|Demand|Industry", "Buildings" = "Emissions|CH4|Energy|Demand|Residential and Commercial" , "Transport" = "Emissions|CH4|Energy|Demand|Transportation"),
          "energy supply" = c("Total" = "Emissions|CH4|Energy|Supply")
        ),
        "N2O" = c("Total" = "Emissions|N2O", "Energy" = "Emissions|N2O|Energy", "AFOLU" = "Emissions|N2O|AFOLU", "Land" = "Emissions|N2O|AFOLU|Land", "Agriculture" = "Emissions|N2O|AFOLU|Agriculture", "Waste" = "Emissions|N2O|Waste", "Other" = "Emissions|N2O|Other")
        ,
        "F-Gases" = c("Total" = "Emissions|F-Gases")
        ,
        "Gross CO2" = list(
          "main" = c("Total" = "Gross Emissions|CO2", "Energy and Industrial Processes" = "Gross Emissions|CO2|Energy and Industrial Processes", "Energy" = "Gross Emissions|CO2|Energy", "Industrial Processes" = "Gross Emissions|CO2|Industrial Processes"),
          "energy demand" = c("Total" = "Gross Emissions|CO2|Energy|Demand", "Industry" = "Gross Emissions|CO2|Energy|Demand|Industry"),
          "energy supply" = c("Total" = "Gross Emissions|CO2|Energy|Supply", "Electricity" = "Gross Emissions|CO2|Energy|Supply|Electricity", "Heat" = "Gross Emissions|CO2|Energy|Supply|Heat", "Hydrogen" = "Gross Emissions|CO2|Energy|Supply|Hydrogen", "Gases" = "Gross Emissions|CO2|Energy|Supply|Gases", "Liquids" = "Gross Emissions|CO2|Energy|Supply|Liquids" , "Solids" = "Gross Emissions|CO2|Energy|Supply|Solids")
        )
      ),
      "Final Energy"  = list(
        "by carrier" = c("Total" = "Final Energy", "Electricity" = "Final Energy|Electricity", "Heat" = "Final Energy|Heat", "Hydrogen" = "Final Energy|Hydrogen", "Gases" = "Final Energy|Gases", "Gases Biomass" = "Final Energy|Gases|Biomass", "Gases Electricity" = "Final Energy|Gases|Electricity", "Gases Fossil" = "Final Energy|Gases|Fossil" , "Liquids" = "Final Energy|Liquids", "Liquids Biomass" = "Final Energy|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Liquids|Fossil", "Solids" = "Final Energy|Solids", "Solids Biomass" = "Final Energy|Solids|Biomass", "Solids Fossil" = "Final Energy|Solids|Fossil" , "Solar" = "Final Energy|Solar"),
        "by sector" = c("Industry and Non-Energy Use" = "Final Energy|Industry and Non-Energy Use", "Industry" = "Final Energy|Industry", "Non-Energy Use" = "Final Energy|Non-Energy Use", "Transport w/ bunkers" = "Final Energy|Transportation (w/ bunkers)", "Transportation" = "Final Energy|Transportation", "Bunkers" = "Final Energy|Bunkers", "Buildings" = "Final Energy|Residential and Commercial", "Other Sector" = "Final Energy|Other Sector"),
        "industry" = list(
          "carrier" = c("Electricity" = "Final Energy|Industry|Electricity", "Heat" = "Final Energy|Industry|Heat", "Gases" = "Final Energy|Industry|Gases", "Gases Biomass" = "Final Energy|Industry|Gases|Biomass", "Gases Electricity" = "Final Energy|Industry|Gases|Electricity", "Gases Fossil" = "Final Energy|Industry|Gases|Fossil", "Liquids" = "Final Energy|Industry|Liquids", "Liquids Biomass" = "Final Energy|Industry|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Industry|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Industry|Liquids|Fossil", "Solids" = "Final Energy|Industry|Solids", "Solids Biomass" = "Final Energy|Industry|Solids|Biomass", "Solids Fossil" = "Final Energy|Industry|Solids|Fossil", "Other" = "Final Energy|Industry|Other"),
          "sub-sector" = c("Chemicals" = "Final Energy|Industry|Chemicals", "Iron and Steel" = "Final Energy|Industry|Iron and Steel", "Non-Metallic Minerals" = "Final Energy|Industry|Non-Metallic Minerals", "Pulp and Paper" = "Final Energy|Industry|Pulp and Paper", "Other Sector" = "Final Energy|Industry|Other Sector")
        ),
        "non-energy use" = list(
          "carrier" = c("Gases" = "Final Energy|Non-Energy Use|Gases", "Gases Biomass" = "Final Energy|Non-Energy Use|Gases|Biomass", "Gases Electricity" = "Final Energy|Non-Energy Use|Gases|Electricity", "Gases Fossil" = "Final Energy|Non-Energy Use|Gases|Fossil", "Liquids" = "Final Energy|Non-Energy Use|Liquids", "Liquids Biomass" = "Final Energy|Non-Energy Use|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Non-Energy Use|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Non-Energy Use|Liquids|Fossil", "Solids" = "Final Energy|Non-Energy Use|Solids", "Solids Biomass" = "Final Energy|Non-Energy Use|Solids|Biomass", "Solids Fossil" = "Final Energy|Non-Energy Use|Solids|Fossil")
        ),
        "transport" = list(
          "by carrier" = c("Electricity" = "Final Energy|Transportation|Electricity", "Hydrogen" = "Final Energy|Transportation|Hydrogen", "Gases" = "Final Energy|Transportation|Gases", "Gases Biomass" = "Final Energy|Transportation|Gases|Biomass", "Gases Electricity" = "Final Energy|Transportation|Gases|Electricity", "Gases Fossil" = "Final Energy|Transportation|Gases|Fossil", "Liquids" = "Final Energy|Transportation|Liquids", "Liquids Biomass" = "Final Energy|Transportation|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Transportation|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Transportation|Liquids|Fossil"),
          "by mode" = c("LDV" = "Final Energy|Transportation|LDV", "LDV Liquids Biomass" = "Final Energy|Transportation|LDV|Liquids|Biomass", "LDV Liquids Electricity" = "Final Energy|Transportation|LDV|Liquids|Electricity", "Truck" = "Final Energy|Transportation|Truck", "Truck Gases Fossil" = "Final Energy|Transportation|Truck|Gases|Fossil", "Rail" = "Final Energy|Transportation|Rail", "Rail Electricity" = "Final Energy|Transportation|Rail|Electricity", "Bus" = "Final Energy|Transportation|Bus", "Domestic Aviation" = "Final Energy|Transportation|Domestic Aviation", "Domestic Aviation Liquids" = "Final Energy|Transportation|Domestic Aviation|Liquids", "Domestic Aviation Liquids Fossil" = "Final Energy|Transportation|Domestic Aviation|Liquids|Fossil", "Domestic Shipping" = "Final Energy|Transportation|Domestic Shipping", "Domestic Shipping Liquids Fossil" = "Final Energy|Transportation|Domestic Shipping|Liquids|Fossil"),
          "freight" = c("Total" = "Final Energy|Transportation|Freight", "Electricity" = "Final Energy|Transportation|Freight|Electricity", "Hydrogen" = "Final Energy|Transportation|Freight|Hydrogen", "Gases" = "Final Energy|Transportation|Freight|Gases", "Liquids" = "Final Energy|Transportation|Freight|Liquids", "Liquids Biomass" = "Final Energy|Transportation|Freight|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Transportation|Freight|Liquids|Electricity", "Liquids|Fossil" = "Final Energy|Transportation|Freight|Liquids|Fossil"),
          "passengers" = c("Total" = "Final Energy|Transportation|Passenger", "Electricity" = "Final Energy|Transportation|Passenger|Electricity", "Hydrogen" = "Final Energy|Transportation|Passenger|Hydrogen", "Gases" = "Final Energy|Transportation|Passenger|Gases", "Liquids" = "Final Energy|Transportation|Passenger|Liquids", "Liquids Biomass" = "Final Energy|Transportation|Passenger|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Transportation|Passenger|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Transportation|Passenger|Liquids|Fossil") #,"Final Energy|Transportation|Rail|Passenger"
        ),
        "bunkers" = list(
          "by carrier" = c("Liquids" = "Final Energy|Bunkers|Liquids", "Liquids Biomass" = "Final Energy|Bunkers|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Bunkers|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Bunkers|Liquids|Fossil"),
          "by mode" = c("International Aviation" = "Final Energy|Bunkers|International Aviation", "International Aviation Liquids" = "Final Energy|Bunkers|International Aviation|Liquids", "International Aviation Liquids Biomass" = "Final Energy|Bunkers|International Aviation|Liquids|Biomass", "International Aviation Liquids Electricity" = "Final Energy|Bunkers|International Aviation|Liquids|Electricity", "International Aviation Liquids Fossil" = "Final Energy|Bunkers|International Aviation|Liquids|Fossil", "International Shipping" = "Final Energy|Bunkers|International Shipping", "International Shipping Liquids" = "Final Energy|Bunkers|International Shipping|Liquids", "International Shipping Liquids Biomass" = "Final Energy|Bunkers|International Shipping|Liquids|Biomass", "International Shipping Liquids Electricity" = "Final Energy|Bunkers|International Shipping|Liquids|Electricity", "International Shipping Liquids Fossil" = "Final Energy|Bunkers|International Shipping|Liquids|Fossil")
        ),
        "buildings" = list(
          "by carrier" = c("Electricity" = "Final Energy|Residential and Commercial|Electricity", "Heat" = "Final Energy|Residential and Commercial|Heat", "Hydrogen" = "Final Energy|Residential and Commercial|Hydrogen", "Gases" = "Final Energy|Residential and Commercial|Gases", "Gases Biomass" = "Final Energy|Residential and Commercial|Gases|Biomass", "Gases Electricity" = "Final Energy|Residential and Commercial|Gases|Electricity", "Gases Fossil" = "Final Energy|Residential and Commercial|Gases|Fossil", "Liquids" = "Final Energy|Residential and Commercial|Liquids", "Liquids Biomass" = "Final Energy|Residential and Commercial|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Residential and Commercial|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Residential and Commercial|Liquids|Fossil", "Solids" = "Final Energy|Residential and Commercial|Solids", "Solids Biomass"  = "Final Energy|Residential and Commercial|Solids|Biomass", "Solids Fossil" = "Final Energy|Residential and Commercial|Solids|Fossil", "Other" = "Final Energy|Residential and Commercial|Other"),
          "by type" = c("Space Heating" = "Final Energy|Residential and Commercial|Space Heating")
        ),
        "other sector" = list(
          "by carrier" = c("Electricity" = "Final Energy|Other Sector|Electricity", "Heat" = "Final Energy|Other Sector|Heat", "Gases" = "Final Energy|Other Sector|Gases", "Gases Biomass" = "Final Energy|Other Sector|Gases|Biomass", "Gases Electricity" = "Final Energy|Other Sector|Gases|Electricity", "Gases Fossil" = "Final Energy|Other Sector|Gases|Fossil", "Liquids" = "Final Energy|Other Sector|Liquids", "Liquids Biomass" = "Final Energy|Other Sector|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Other Sector|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Other Sector|Liquids|Fossil", "Solids" = "Final Energy|Other Sector|Solids", "Solids Biomass" = "Final Energy|Other Sector|Solids|Biomass", "Solids Fossil" = "Final Energy|Other Sector|Solids|Fossil")
        ),
        "industry and non-energy use" = list(
          "carrier" = c("Gases" = "Final Energy|Industry and Non-Energy Use|Gases", "Gases Biomass" = "Final Energy|Industry and Non-Energy Use|Gases|Biomass", "Gases Electricity" = "Final Energy|Industry and Non-Energy Use|Gases|Electricity", "Gases Fossil" = "Final Energy|Industry and Non-Energy Use|Gases|Fossil", "Liquids" = "Final Energy|Industry and Non-Energy Use|Liquids", "Liquids Biomass" = "Final Energy|Industry and Non-Energy Use|Liquids|Biomass", "Liquids Electricity" = "Final Energy|Industry and Non-Energy Use|Liquids|Electricity", "Liquids Fossil" = "Final Energy|Industry and Non-Energy Use|Liquids|Fossil", "Solids" = "Final Energy|Industry and Non-Energy Use|Solids", "Solids Biomass" = "Final Energy|Industry and Non-Energy Use|Solids|Biomass", "Solids Fossil" = "Final Energy|Industry and Non-Energy Use|Solids|Fossil")
        )
      ),
      "Secondary Energy" = list(
        "Electricity" = c("Total" = "Secondary Energy|Electricity", "Solar" = "Secondary Energy|Electricity|Solar", "Solar PV" = "Secondary Energy|Electricity|Solar|PV", "Wind" = "Secondary Energy|Electricity|Wind", "Wind Offshore" = "Secondary Energy|Electricity|Wind|Offshore", "Wind Onshore" = "Secondary Energy|Electricity|Wind|Onshore", "Biomass" = "Secondary Energy|Electricity|Biomass", "Biomass w/ CCS" = "Secondary Energy|Electricity|Biomass|w/ CCS", "Biomass w/o CCS" = "Secondary Energy|Electricity|Biomass|w/o CCS", "Fossil" = "Secondary Energy|Electricity|Fossil", "Fossil w/o CCS" = "Secondary Energy|Electricity|Fossil|w/o CCS", "Gas" = "Secondary Energy|Electricity|Gas", "Gas w/o CCS" = "Secondary Energy|Electricity|Gas|w/o CCS", "Oil" = "Secondary Energy|Electricity|Oil", "Oil w/o CCS" = "Secondary Energy|Electricity|Oil|w/o CCS", "Coal" = "Secondary Energy|Electricity|Coal", "Coal w/o CCS" = "Secondary Energy|Electricity|Coal|w/o CCS", "Geothermal" = "Secondary Energy|Electricity|Geothermal", "Hydro" = "Secondary Energy|Electricity|Hydro", "Nuclear" = "Secondary Energy|Electricity|Nuclear", "Other" = "Secondary Energy|Electricity|Other", "Non-Biomass Renewables" = "Secondary Energy|Electricity|Non-Biomass Renewables"),
        "Hydrogen" = c("Total" = "Secondary Energy|Hydrogen", "Biomass" = "Secondary Energy|Hydrogen|Biomass", "Electricity" = "Secondary Energy|Hydrogen|Electricity", "Fossil" = "Secondary Energy|Hydrogen|Fossil"),
        "Heat" = c("Total" = "Secondary Energy|Heat", "Biomass" = "Secondary Energy|Heat|Biomass", "Fossil" = "Secondary Energy|Heat|Fossil", "Geothermal" = "Secondary Energy|Heat|Geothermal"), #, "Solar" = "Secondary Energy|Heat|Solar"
        "Gases" = c("Total" = "Secondary Energy|Gases", "Biomass" = "Secondary Energy|Gases|Biomass", "Electricity" = "Secondary Energy|Gases|Electricity", "Fossil" = "Secondary Energy|Gases|Fossil", "Natural Gas" = "Secondary Energy|Gases|Gas"),
        "Liquids" = c("Total" = "Secondary Energy|Liquids", "Biomass" = "Secondary Energy|Liquids|Biomass", "Biomass w/o CCS" = "Secondary Energy|Liquids|Biomass|w/o CCS", "Electricity" = "Secondary Energy|Liquids|Electricity", "Fossil" = "Secondary Energy|Liquids|Fossil", "Oil" = "Secondary Energy|Liquids|Oil"),
        "Solids" = c("Total" = "Secondary Energy|Solids", "Biomass" = "Secondary Energy|Solids|Biomass", "Fossil" = "Secondary Energy|Solids|Fossil", "Coal" = "Secondary Energy|Solids|Coal")
      ),
      "Primary Energy" = list(
        "by type" = c("Solar" = "Primary Energy|Solar", "Wind" = "Primary Energy|Wind", "Biomass" = "Primary Energy|Biomass", "Hydro" = "Primary Energy|Hydro", "Geothermal" = "Primary Energy|Geothermal", "Nuclear" = "Primary Energy|Nuclear", "Gas" = "Primary Energy|Gas", "Oil" = "Primary Energy|Oil", "Coal" = "Primary Energy|Coal", "Ocean" = "Primary Energy|Ocean", "Other" = "Primary Energy|Other"),
        "biomass" = c("Total" = "Primary Energy|Biomass", "Biomass w/o CCS" = "Primary Energy|Biomass|w/o CCS", "Electricity" = "Primary Energy|Biomass|Electricity", "Electricity w/o CCS" = "Primary Energy|Biomass|Electricity|w/o CCS", "Modern" = "Primary Energy|Biomass|Modern", "Traditional" = "Primary Energy|Biomass|Traditional"),
        "Fossil" = c("Total" = "Primary Energy|Fossil", "Fossil w/o CCS" = "Primary Energy|Fossil|w/o CCS"),
        "Gas" = c("Total" = "Primary Energy|Gas", "Gas w/ CCS" = "Primary Energy|Gas|w/ CCS", "Gas w/o CCS" = "Primary Energy|Gas|w/o CCS", "Electricity" = "Primary Energy|Gas|Electricity", "Electricity w/o CCS" = "Primary Energy|Gas|Electricity|w/o CCS"),
        "Oil" = c("Total" = "Primary Energy|Oil", "Oil w/o CCS" = "Primary Energy|Oil|w/o CCS", "Oil Electricity" = "Primary Energy|Oil|Electricity"),
        "Coal" = c("Total" = "Primary Energy|Coal", "Coal w/ CCS" = "Primary Energy|Coal|w/ CCS", "Coal w/o CCS" = "Primary Energy|Coal|w/o CCS", "Electricity" = "Primary Energy|Coal|Electricity", "Electricity w/o CCS" = "Primary Energy|Coal|Electricity|w/o CCS")
      ),
      "Shares" = list(
        "final energy" = list(
          "Electricity" = c("share" = "Final Energy|Electricity Share", "Industry" = "Final Energy|Industry|Electricity Share", "Transport" = "Final Energy|Transportation|Electricity Share", "Buildings" = "Final Energy|Residential and Commercial|Electricity Share"),
          "Hydrogen" = c("share" = "Final Energy|Hydrogen Share", "Industry" = "Final Energy|Industry|Hydrogen Share", "Transport" = "Final Energy|Transportation|Hydrogen Share", "Buildings" = "Final Energy|Residential and Commercial|Hydrogen Share")
        ),
        "secondary energy" = c("VRE share" = "Secondary Energy|VRE Share", "Nuclear share" = "Secondary Energy|Electricity|Nuclear Share")
        ,
        "primary energy" = c("Biomass" = "Primary Energy|Biomass Share", "Fossil" = "Primary Energy|Fossil Share")
      ),
      "Trade" = list(
        "primary energy" = c("Biomass" = "Trade|Primary Energy|Biomass|Volume", "Gas" = "Trade|Primary Energy|Gas|Volume", "Oil"= "Trade|Primary Energy|Oil|Volume", "Coal"= "Trade|Primary Energy|Coal|Volume"),
        "secondary energy" = c("Electricity"= "Trade|Secondary Energy|Electricity|Volume", "Liquids" = "Trade|Secondary Energy|Liquids|Volume", "Liquids Biomass" = "Trade|Secondary Energy|Liquids|Biomass|Volume")
      ),
      "Energy Service" = list(
        "transport" = c("Freight" = "Energy Service|Transportation|Freight", "Freight Truck" = "Energy Service|Transportation|Freight|Truck", "Passenger" = "Energy Service|Transportation|Passenger", "Passenger LDV" = "Energy Service|Transportation|Passenger|LDV"),
        "buildings" = c("Floor Space" = "Energy Service|Residential and Commercial|Floor Space")
      ),
      "Capacity" = list(
        "electricity" = c("Total" = "Capacity|Electricity", "Solar PV" = "Capacity|Electricity|Solar|PV", "Wind Onshore" = "Capacity|Electricity|Wind|Onshore", "Geothermal" = "Capacity|Electricity|Geothermal", "Hydro" = "Capacity|Electricity|Hydro", "Nuclear" = "Capacity|Electricity|Nuclear", "Biomass" = "Capacity|Electricity|Biomass", "Gas" = "Capacity|Electricity|Gas", "Oil" = "Capacity|Electricity|Oil", "Coal" = "Capacity|Electricity|Coal")
      ),
      "CDR" = list(
        "carbon capture" = c("Total" = "Carbon Capture", "Biomass" = "Carbon Capture|Biomass", "Fossil" = "Carbon Capture|Fossil", "Direct Air Capture" = "Carbon Capture|Direct Air Capture", "Storage Biomass" = "Carbon Capture|Storage|Biomass", "Storage Fossil" = "Carbon Capture|Storage|Fossil"),
        "carbon removal" = c("Total" = "Carbon Removal", "Land Use" = "Carbon Removal|Land Use")
      ),
      "Economy" = c("GDP MER" = "GDP|MER", "GDP PPP" = "GDP|PPP", "Population" = "Population", "Consumption" = "Consumption"),
      "Price" = list(
        "carbon" = c("Carbon Price" = "Price|Carbon"),
        "industry" = c("Electricity" = "Price|Final Energy|Industry|Electricity", "Heat" = "Price|Final Energy|Industry|Heat", "Gases" = "Price|Final Energy|Industry|Gases", "Liquids" = "Price|Final Energy|Industry|Liquids", "Solids" = "Price|Final Energy|Industry|Solids"),
        "transport" = c("Electricity" = "Price|Final Energy|Transportation|Electricity", "Hydrogen" = "Price|Final Energy|Transportation|Hydrogen", "Liquids" = "Price|Final Energy|Transportation|Liquids"),
        "buildings" = c("Electricity" = "Price|Final Energy|Residential and Commercial|Electricity", "Heat" = "Price|Final Energy|Residential and Commercial|Heat", "Gases" = "Price|Final Energy|Residential and Commercial|Gases", "Liquids" = "Price|Final Energy|Residential and Commercial|Liquids", "Solids" = "Price|Final Energy|Residential and Commercial|Solids"),
        "secondary energy" = c("Electricity" = "Price|Secondary Energy|Electricity", "Hydrogen" = "Price|Secondary Energy|Hydrogen", "Gases" = "Price|Secondary Energy|Gases", "Gases Fossil" = "Price|Secondary Energy|Gases|Fossil", "Liquids Biomass" = "Price|Secondary Energy|Liquids|Biomass", "Solids Fossil" = "Price|Secondary Energy|Solids|Fossil")
      )
    )
  }
  
  createCharts <- function(dd, hh, tree, reg){
  
    recurseTree <- function(ddd, hhh, t){
      return(
        lapply(setNames(names(t),names(t)), function(name){
          if(is.list(t[[name]])){
            return(recurseTree(ddd, hhh, t[[name]]))
          } else {
            lapply(setNames(names(t[[name]]),names(t[[name]])), function(varName){
              #print(paste0("name:", name, " varName:", varName ))
              print(paste0("Creating chart for: ", t[[name]][[varName]]))
              #return(individualLineChart(ddd, hhh, t[[name]][[varName]]))
              g <- individualLineChart(ddd, hhh, t[[name]][[varName]])
              if(saveCharts) {
                ggsave(paste0(outputFolder, "/individualCharts/svg/", cleanFileName(unique(ddd$region)) , "/", gsub("\\/", "-", gsub("\\/ ", "-", gsub("\\|", "_", t[[name]][[varName]]))), ".svg"), g$chart, device="svg", width = 12, height = 6, dpi=100, units = "in")
                ggsave(paste0(outputFolder, "/individualCharts/png/", cleanFileName(unique(ddd$region)), "/", gsub("\\/", "-", gsub("\\/ ", "-", gsub("\\|", "_", t[[name]][[varName]]))), ".png"), g$chart, device="png", width = 12, height = 6, dpi=100, units = "in")
              }
              return(g)
            })
          }
        })
      )
    }
  
    o <- list()
    if(saveCharts) {
      dir.create(paste0(outputFolder, "/individualCharts/png"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(outputFolder, "/individualCharts/svg"), recursive = TRUE, showWarnings = FALSE)
    }
    o <- list()
    for(r in reg){
      if(saveCharts) {
        dir.create(paste0(outputFolder, "/individualCharts/png/", cleanFileName(r)), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(outputFolder, "/individualCharts/svg/", cleanFileName(r)), recursive = TRUE, showWarnings = FALSE)
      }
    o[[cleanFileName(r)]] <- recurseTree(dd %>% filter(region %in% r), hh %>% filter(region %in% r), tree)
    }
    return(o)
  
  }
  
  # Creating charts object
  if(!(all(file.exists(paste0(outputFolder, "/individualCharts/charts_", cleanFileName(unique(data$region)), ".rds"))))){
    out <- createCharts(data,histData,varList,unique(data$region))
    # save chart files
    for(r in unique(data$region)){
      #regionfilename <- gsub("\\(\\)| +","",gsub("[\\\\/:*?\"<>|]","",r))
      saveRDS(out[[cleanFileName(r)]],paste0(outputFolder,"/individualCharts/charts_", cleanFileName(r), ".rds"))
    }
  }
  
  # create html report
  print(paste0("Creating individual charts html report for scenario: ", unique(data$scenario), " and regions: ", toString(cleanFileName(unique(data$region)))))
  rmarkdown::render("./reports/individualCharts.Rmd", output_file = paste0(".", outputFolder, "/individualCharts - ", unique(data$scenario), " - " , toString(cleanFileName(unique(data$region))), ".html"), params = list(regions = cleanFileName(unique(data$region)), scenario = unique(data$scenario), dataFolder = paste0(".",outputFolder)))
  
}
