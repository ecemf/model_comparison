library(rmarkdown)

dir.create("./output", showWarnings = FALSE) # create the output folder

# EU28
# prio-1 Pure carbon pricing

render('./R/model_comparison.Rmd',output_file = '../output/prio1_EU28_report_(4scen).html', params = list(regionDisplayed="EU27 & UK", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-Base", "DIAG-C400-lin", "DIAG-NZero"), type = "NPIhist"))
render('./R/model_comparison.Rmd',output_file = '../output/prio1_EU28_report.html', params = list(regionDisplayed="EU27 & UK", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-Base", "DIAG-C80-gr5", "DIAG-C0to80-gr5", "DIAG-C400-lin", "DIAG-NZero"), type = "NPIhist", forceSave = TRUE))

# prio-2 Technology Constraint scenarios

render('./R/model_comparison.Rmd',output_file = '../output/prio2_techConstraint_EU28_report.html', params = list(regionDisplayed="EU27 & UK", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-C400-lin", "DIAG-C400-lin-LimBio","DIAG-C400-lin-LimCCS","DIAG-C400-lin-LimNuclear"), type="techConstraint", forceSave = TRUE))

# prio-2 Paradigm Shift scenarios

render('./R/model_comparison.Rmd',output_file = '../output/prio2_paradigmShift_EU28_report.html', params = list(regionDisplayed="EU27 & UK", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-C400-lin", "DIAG-C400-lin-HighVRE","DIAG-C400-lin-HighElectrification","DIAG-C400-lin-HighH2","DIAG-C400-lin-ResidualFossil","DIAG-C400-lin-HighEff"), type="paradigmShift", forceSave = TRUE))

#EU27

# prio-1 Pure carbon pricing

render('./R/model_comparison.Rmd',output_file = '../output/prio1_EU27_report_(4scen).html', params = list(regionDisplayed="EU27", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-Base", "DIAG-C400-lin", "DIAG-NZero"), type = "NPIhist"))

render('./R/model_comparison.Rmd',output_file = '../output/prio1_EU27_report.html', params = list(regionDisplayed="EU27", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-Base", "DIAG-C80-gr5", "DIAG-C0to80-gr5", "DIAG-C400-lin", "DIAG-NZero"), type = "NPIhist"))

# prio-2 Technology Constraint scenarios

render('./R/model_comparison.Rmd',output_file = '../output/prio2_techConstraint_EU27_report.html', params = list(regionDisplayed="EU27", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-C400-lin", "DIAG-C400-lin-LimBio","DIAG-C400-lin-LimCCS","DIAG-C400-lin-LimNuclear"), type="techConstraint"))

# prio-2 Paradigm Shift scenarios

render('./R/model_comparison.Rmd',output_file = '../output/prio2_paradigmShift_EU27_report.html', params = list(regionDisplayed="EU27", updateResults=FALSE, scenarios = c("DIAG-NPI", "DIAG-C400-lin", "DIAG-C400-lin-HighVRE","DIAG-C400-lin-HighElectrification","DIAG-C400-lin-HighH2","DIAG-C400-lin-ResidualFossil","DIAG-C400-lin-HighEff"), type="paradigmShift"))
