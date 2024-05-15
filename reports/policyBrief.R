

# Create task 1.3 scenarios report
createPolicyBriefReport <- function(data, histData, outputFolder, saveCharts=TRUE){
  
  # create html report
  for(r in unique(data$region)){ 
    if(saveCharts){
      dir.create(paste0(outputFolder, "/policyBrief/", cleanFileName(r), "/png"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(outputFolder, "/policyBrief/", cleanFileName(r), "/svg"), recursive = TRUE, showWarnings = FALSE)
    }
    print(paste0("Creating policy brief report for region: ", r))
    rmarkdown::render("./reports/policyBrief.Rmd", output_file = paste0(".", outputFolder, "/policyBrief - ", cleanFileName(r), ".html"), params = list(df = data %>% filter(region == r), hist = histData %>% filter(region == r), chartsFolder = paste0(".", outputFolder, "/policyBrief/", cleanFileName(r)), save = saveCharts))
    #rmarkdown::render("./reports/policyBrief.Rmd", output_file = paste0(outputFolder, "/policyBrief - ", cleanFileName(r), ".html"), params = list(df = data %>% filter(region == r), hist = histData %>% filter(region == r), chartsFolder = paste0(outputFolder, "/policyBrief/", cleanFileName(r)), save = saveCharts))
  }
}
