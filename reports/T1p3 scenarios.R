

# Create task 1.3 scenarios report
createT1p3scenariosReport <- function(data, histData, outputFolder, saveCharts=TRUE){
  
  # create html report
  for(r in unique(data$region)){ 
    if(saveCharts){
      dir.create(paste0(outputFolder, "/T1p3scenarios/", cleanFileName(r), "/png"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(outputFolder, "/T1p3scenarios/", cleanFileName(r), "/svg"), recursive = TRUE, showWarnings = FALSE)
    }
    print(paste0("Creating task 1p3 scenario report for region: ", r))
    rmarkdown::render("./reports/T1p3 scenarios.Rmd", output_file = paste0(outputFolder, "/T1p3_scenarios - ", cleanFileName(r), ".html"), params = list(df = data %>% filter(region == r), hist = histData %>% filter(region == r), chartsFolder = paste0(outputFolder, "/T1p3scenarios/", cleanFileName(r)), save = saveCharts))
    #rmarkdown::render("./reports/T1p3 scenarios.Rmd", output_file = paste0(outputFolder, "/T1p3_scenarios - ", cleanFileName(r), ".html"), params = list(df = data %>% filter(region == r), hist = histData %>% filter(region == r), chartsFolder = paste0(outputFolder, "/T1p3scenarios/", cleanFileName(r)), save = saveCharts))
  }
}
