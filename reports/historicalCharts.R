
createHistoricalChartsReport <- function(data, histData, outputFolder, saveCharts=TRUE){
  
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
  
  #create plots for each variable
  if(!(file.exists(paste0(outputFolder,"/historicalCharts/charts.rds")))){
    g <- list()
    i = 0
    ii = length(intersect(unique(histData$variable),unique(data$variable))) * length(unique(data$region))
    for(r in unique(data$region)){
      dir.create(paste0(outFolder, "/historicalCharts/png/", cleanFileName(r)), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(outFolder, "/historicalCharts/svg/", cleanFileName(r)), recursive = TRUE, showWarnings = FALSE)
      g[[cleanFileName(r)]] <- list() 
      for (var in sort(intersect(unique(histData$variable),unique(data$variable)))){
        i = i + 1
        if(all(histData %>% filter(variable == var) %>% pull(value) == 0)){
          print(paste0("Creating plot (", i , " of ", ii , ") for " , var, " canceled due to the lack of historical data."))
        } else {
          print(paste0("Creating plot (", i , " of ", ii , ") for " , var))
          currData <- data %>% filter(variable == var, region == r) 
          currHist <- histData %>% filter(variable == var, region == r) 
          hist.colors <- gray.colors(length(unique(currHist$model)), start = 0, end = 0.7)
          names(hist.colors) <- unique(currHist$model)
          hist.shape <- seq(15,15+length(unique(currHist$model))-1)
          names(hist.shape) <- unique(currHist$model)
          hist.linetype <- seq(1,length(unique(currHist$model)))
          names(hist.linetype) <- unique(currHist$model)
          g[[cleanFileName(r)]][[var]] <- ggplot(data = rbind(currData,currHist), aes(x = period, y = value, color = model)) +
            geom_hline(yintercept=0, color = "black", linewidth=1, alpha = 0.5) +
            geom_vline(xintercept=2020, color = "black", linewidth=1, alpha = 0.5,linetype="dashed") +
            geom_line(data=currData, aes( color = model), linewidth = 1) +
            geom_point(data=currHist, aes(color = model, fill = model, shape = model), size = 2.5, alpha = 0.8) +
            geom_line(data=currHist, aes( color = model, linetype = model), linewidth = 0.7, alpha = 0.7) +
            scale_fill_manual("References", values = hist.colors, guide = guide_legend(override.aes = list(colour = hist.colors))) +
            scale_shape_manual("References", values = hist.shape) +
            scale_linetype_manual("References", values = hist.linetype) +
            scale_color_manual("Models",
                               values = c(hist.colors, model.colors),
                               breaks = unique(data$model),
                               guide = guide_legend(order = 1, title.position = "top", ncol = 1, override.aes = list(shape = NA))
            ) +
            theme_minimal(base_size = 16) + 
            theme(text=element_text(family="sans")) +
            xlab("Year") +
            ylab(as.character(unique(data %>% filter(variable == var) %>% pull(unit)))) +
            ggtitle(var) +
            theme(legend.key.width = unit(1.5, 'cm')) + 
            theme(plot.background = element_rect(fill = 'white', colour = 'white'))
          ggsave(paste0(outFolder, "/historicalCharts/svg/", cleanFileName(r) , "/", gsub("\\/", "-", gsub("\\/ ", "-", gsub("\\|", "_", var))), ".svg"), g[[cleanFileName(r)]][[var]], device="svg", width = 12, height = 6, dpi=100, units = "in")
          ggsave(paste0(outFolder, "/historicalCharts/png/", cleanFileName(r), "/", gsub("\\/", "-", gsub("\\/ ", "-", gsub("\\|", "_", var))), ".png"), g[[cleanFileName(r)]][[var]], device="png", width = 12, height = 6, dpi=100, units = "in")
        }
      }
    }
    
    saveRDS(g,paste0(outputFolder,"/historicalCharts/charts.rds"))
    
    #enumerate plots with historical data
    d <- list()
    for(r in unique(data$region)){
      d[[cleanFileName(r)]] <- unique(intersect(histData %>% filter(region %in% r) %>% pull(variable) %>% unique(), data %>% filter(region %in% r) %>% pull(variable) %>% unique()))
    }
    saveRDS(d,paste0(outputFolder,"/historicalCharts/charts_enumeration.rds"))
  }
  
  # create html reports
  print(paste0("Creating historical charts html report for scenario: ", unique(data$scenario), " and regions: ", toString(cleanFileName(unique(data$region)))))
  rmarkdown::render("./reports/historicalCharts.Rmd", output_file = paste0(".", outputFolder, "/historicalCharts - ", unique(data$scenario), " - " , toString(cleanFileName(unique(data$region))), ".html"), params = list(regions = cleanFileName(unique(data$region)), scenario = unique(data$scenario), pathFolder = paste0(".",outputFolder)))
  
}
