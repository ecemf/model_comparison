

color <- c(
  #model
  "IMAGE" = "#00ffff",
  "Euro-Calliope" = "#c0c0c0",
  "MEESA" = "#ff00ff",
  "x MEESA" = "#ff00ff",
  "MESSAGE" = "#800080",
  "OSeMBE" = "#a52a2a",
  "x OSeMBE" = "#a52a2a",
  "PRIMES" = "#0000ff",
  "PROMETHEUS" = "#ffd900",
  "REMIND" = "#ff6347",
  "TIAM-ECN" = "#4682b4",
  "WITCH" = "#228b22",
  #scenario
  "DIAG-Base" = "#f1746e",
  "DIAG-NPI" = "#b49f06",
  "DIAG-C80-gr5" = "#2eba38",
  "DIAG-C0to80-gr5" = "#38c0c4",
  "DIAG-C400-lin" = "#939dff",
  "DIAG-NZero" = "#f062e4",
  
  "DIAG-C400-lin-LimBio" = "#005900",
  "DIAG-C400-lin-LimCCS" = "#999959",
  "DIAG-C400-lin-LimNuclear" = "#ff33ff",
  
  "DIAG-C400-lin-HighVRE" = "#337fff",
  "DIAG-C400-lin-HighElectrification" = "#ffb200",
  "DIAG-C400-lin-HighH2" = "#5ed5b0",
  "DIAG-C400-lin-ResidualFossil" = "#cccccc",
  "DIAG-C400-lin-HighEff" = "#2eba38",
  
  #emi
  "Energy Supply" = "#3D4849",
  "Energy Supply - Elect and Heat" = "#FF6600",
  "Energy Supply - Electricity"= "#ffb200",
  "Energy Supply - Heat"= "#cc0000",
  "Energy Supply - Fuels" = "#4D4D93",
  "Energy Supply - Gases"= "#999959",
  "Energy Supply - Liquids"= "#0000cc",
  "Energy Supply - Solids"= "#0c0c0c",
  "Energy Supply - Other" = "#79a6d2",
  "Energy Supply - Hydrogen" = "#5ed5b0",
  "Energy Demand" = "#999959",
  "Energy Demand - Transportation" = "#a1dd70",
  "Energy Demand - Bunkers" = "#ffc0cb",
  "Energy Demand - Buildings" = "#5edfff",
  "Energy Demand - Residential and Commercial" = "#5edfff",
  "Energy Demand - Other Sector" = "#999959",
  "Demand - AFOFI" = "#005900",
  "Energy Demand - Industry" = "#5f6769",
  "Industrial Processes" = "#5c5c5c",
  "Other" = "#5b0d8f",
  "Waste" = "#A52A2A",
  "AFOLU" = "#4DAF4A",
  "AFOLU - Agriculture" = "#357933",
  "AFOLU - LULUCF" = "#6ec16b",
  #capture
  "CCS" = "#cccccc",
  "CCS - Biomass" = "#008c00",
  "CCS - Biomass Energy" = "#005900",
  "CCS - Biomass Industry" = "#377EB8",
  "CCS - Biomass Industrial Processes" = "#79a6d2",
  "CCS - Fossil" = "#cccccc",
  "CCS - Fossil Energy" = "#a6a6a6",
  "CCS - Fossil Industry" = "#4e4e4e",
  "CCS - Fossil Industrial Processes" = "#282828",
  "DAC" = "#CC6666",
  "Enhanced Weathering" = "#5b0d8f",
  "Land Use" = "#4DAF4A",
  "Other" = "#79a6d2",
  #carrier
  "Electricity" = "#ffb200",
  "Heat"        = "#cc0000",
  "Hydrogen"    = "#5ed5b0",
  "Gases"       = "#999959",
  "Gases - Fossil"  = "#999959", # #a1a161",
  "Gases - Biomass" = "#00cc00", #"#005900", # "#999959",
  "Gases - Electricity"  = "#fef3da", #"#ffb200", # "#caca8a",
  "Liquids"     = "#0000cc",
  "Liquids - Fossil"  = "#0000cc", #"#000068",
  "Liquids - Biomass" = "#005900", #"#0000cc",
  "Liquids - Electricity"   = "#ffb200", #"#5050ff",
  "Solids"      = "#0c0c0c",
  "Solids - Fossil"  = "#0c0c0c",
  "Solids - Biomass" = "#0f3c08", #"#005900",
  "Other Carrier" = "#79a6d2",
  "Geothermal" = "#e51900",
  "Solar" = "#ffcc00",
  #sector
  "Non-Energy Use" = "#ff9966",
  "Industry" = "#5f6769",
  "Residential and Commercial" = "#5edfff",
  "Transportation" = "#a1dd70",
  "Bunkers" = "#ffc0cb",
  "Other Sector" = "#999959",
  #sector fe carrier
  "Non-Energy Use - Hydrogen" = "#5ed5b0",
  "Non-Energy Use - Gases" = "#999959",
  "Non-Energy Use - Liquids" = "#0000cc",
  "Non-Energy Use - Solids" = "#0c0c0c",
  "Non-Energy Use - Other" = "#79a6d2",
  "Industry - Electricity" = "#ffb200",
  "Industry - Heat" = "#cc0000",
  "Industry - Hydrogen" = "#5ed5b0",
  "Industry - Gases" = "#999959",
  "Industry - Liquids" = "#0000cc",
  "Industry - Solids" = "#0c0c0c",
  "Industry - Other" = "#79a6d2",
  "Transport - Electricity" = "#ffb200",
  "Transport - Hydrogen" = "#5ed5b0",
  "Transport - Gases" = "#999959",
  "Transport - Liquids" = "#0000cc",
  "Transport - Other" = "#79a6d2",
  "Bunkers - Electricity" = "#ffb200",
  "Bunkers - Hydrogen" = "#5ed5b0",
  "Bunkers - Gases" = "#999959",
  "Bunkers - Liquids" = "#0000cc",
  "Bunkers - Other" = "#79a6d2",
  #sector per type
  "Passenger" = "#77c3e6",
  "Freight" = "#37474f",
  #tech
  "Solar" = "#ffcc00",
  "Solar CSP" = "#ffcc00",
  "Solar PV" = "#ffe600",
  "Wind" = "#337fff",
  "Wind Offshore" = "#4d33ff",
  "Wind Onshore" = "#337fff",
  "Other" = "#5b0d8f",
  "Ocean" = "#0000FF",
  "Biomass" = "#005900",
  "Biomass w/ CCS" = "#33ff00",
  "Biomass w/o CCS" = "#005900",
  "Hydro" = "#191999",
  "Hydro Reservoir" = "#8a8aec",
  "Hydro Pumped Storage" = "#dfdffa",
  "Hydro Run of River" = "#191999",
  "Geothermal" = "#e51900",
  "Nuclear" = "#ff33ff",
  "Gas" = "#999959",
  "Gas CCGT" = "#7c6b2a",
  "Gas w/ CCS" = "#b79e38",
  "Gas w/o CCS" = "#999959",
  "Coal" = "#0c0c0c",
  "Coal w/ CCS" = "#5c5c5c",
  "Coal w/o CCS" = "#0c0c0c",
  "Oil" = "#b30000",
  "Oil w/ CCS" = "#ff6565",
  "Oil w/o CCS" = "#b30000",
  
  "VRE" = "#337fff",
  "non VRE" = "#ececec",
  
  "Hydrogen w/ CCS"="#86e0c4",
  "Hydrogen w/o CCS"="#5ed5b0",
  "Electricity w/ CCS"="#ffc133",
  "Electricity w/o CCS"="#ffb200",  
  "Liquids w/ CCS" = "#0000ff",
  "Liquids w/o CCS" = "#0000cc",
  
  #Carbon management
  "Direct Air Capture" = "#24A19C",
  "Fossil" = "#cccccc",
  "Storage" = "#D96098",
  "Usage" = "#325288",
  #Kyoto Gases
  "CO2" = "#7e7e7e",
  "CH4" = "#ffe7a0",
  "N2O" = "#44ba97",
  "F-Gases" = "#ff9977"
)

alpha = c(
  #hist
  "EU27Ref" = 0.4,
  "Eurostat" = 1,
  "IMF" = 1,           
  "2019 Eurostat" = 1
)

#plot aesthetics
theme_singleColumn <- function(){
  theme_minimal(base_size = 16) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5),
#      legend.position="bottom",
      legend.title=element_blank(),
      legend.key.size = unit(20,"pt"),
      legend.text = element_text(size=14, margin = margin(r = 20, unit = 'pt')),
      panel.spacing = unit(1.5, "lines")
    )
}


theme_singlePlot <- function(){
  theme_minimal(base_size = 16*1.5) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5),
      legend.position="bottom",
      legend.title=element_blank(),
      legend.key.size = unit(20*2,"pt"),
      legend.text = element_text(size=14*2)
    )
}