library(rmarkdown)

dir.create("./output", showWarnings = FALSE) # create the output folder

render('./R/model_comparison.Rmd',output_file = '../output/model_comparison_EU27.html', params = list(regionDisplayed="EU27", updateResults=FALSE))
render('./R/model_comparison.Rmd',output_file = '../output/model_comparison_EU28.html', params = list(regionDisplayed="EU27 & UK", updateResults=FALSE))
