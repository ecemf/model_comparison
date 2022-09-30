
#create plot function
createPlot <- function(origDf,plotArgs,plotName,region,scenarios,scenName,indivFilePath){
  
  # function to create line plots
  linePlot <- function(data, scen=NULL, title="", colorDim="Model", linetypeDim=NULL, xdim="Period", ydim="Value", facetDim="Scenario", legend = list(nrow=NULL), min=0, max=NULL, minPeriod=2005, maxPeriod=2050, labels=NULL, facet = list(nrow=NULL,ncol=2), percentage=FALSE, saveFile=FALSE, saveFolder=""){
    
    if(nrow(data)==0){
      return(NULL)
    }
    
    if(is.null(scen)){
      scen <- unique(data[!(data$Model == "reference"),]$Scenario)
    }
    
    data <- data[(data$Period >= minPeriod) & (data$Period <= maxPeriod),]
    plotData <- data[((!(data$Model=="reference")) & (data$Scenario %in% scen)),]
    plotData$Scenario <- factor(plotData$Scenario , levels = scen)
    plotData$Model <- factor(plotData$Model, level = levels(plotData$Model)[!levels(plotData$Model) == c("reference")])
    
    hist <- data[(data$Model=="reference"),]
    
    if(!(nrow(hist)==0)){
      tmp <- hist
      tmp$source <- tmp$Scenario
      histFull <- NULL
      for(sc in scen){
        tmp$Scenario <- sc
        histFull <- rbind(histFull,tmp)
      }
      histLabel <- histFull %>%
        group_by(source, Region, Variable) %>%
        filter(Period == max(Period))
      histFull$Scenario <- factor(histFull$Scenario , levels = scen)
      histLabel$Scenario <- factor(histLabel$Scenario , levels = scen)
    }
    
    if(!(is.null(labels))){
      fullVars <- levels(data$Variable)
      names(fullVars) <- labels
      labelsVars <- fullVars[fullVars %in% unique(data$Variable)]
    }
    
    gg <- ggplot(plotData,aes_string(x=xdim ,y=ydim, color=colorDim ,linetype=linetypeDim)) +
      geom_hline(yintercept=0, color = "black", size=1, alpha = 0.15) +
      geom_vline(xintercept=2020, color = "black", size=1, alpha = 0.15, linetype = "dashed") +
      geom_line(size=1) +
      geom_point() +
      {if((facetDim=="Variable")) facet_wrap(facetDim,drop = FALSE, nrow=facet$nrow, ncol=facet$ncol, 
                                             labeller = labeller(Variable = 
                                                                   function(Variable, Value) {
                                                                     facetLabels <- setNames(names(labelsVars),labelsVars)
                                                                     out <- facetLabels[Variable]
                                                                     return(out) 
                                                                   }))} +
      # {if(!(nrow(hist)==0) & !(facetDim=="Model") & !is.null(linetypeDim)) geom_line(data=histFull,size=1,aes(group = source),color="black",alpha = 1,lty="11") } +
      # {if(!(nrow(hist)==0) & !(facetDim=="Model") & is.null(linetypeDim)) geom_line(data=histFull,size=1,aes(group = source),color="black",alpha = 1,lty="11") } +
      {if(!(nrow(hist)==0) & !(facetDim=="Model")) geom_line(data=histFull,size=1,aes(alpha = source),color="black") } + #,lty="11" 
      {if(!(nrow(hist)==0) & !(facetDim=="Model")) geom_point(data=histFull,size=1, stroke = 1.5,shape=4,aes(alpha = source),color="black") } +
      {if(!(nrow(hist)==0) & !(facetDim=="Model")) geom_text(data=histLabel, aes(label = source), color="black", hjust ="inward", vjust=1.5) } +
      {if(!(nrow(hist)==0) & !(facetDim=="Model")) scale_alpha_manual(values = as.numeric(alpha[unique(histFull$source)]),guide="none") } +
      {if(!(facetDim=="Variable")) facet_wrap(facetDim,drop = FALSE, nrow=facet$nrow, ncol=facet$ncol)} +
      theme_singleColumn() +
      theme(
        legend.key.size = unit(30,"pt"),
        legend.text = element_text(size=16, margin = margin(r = 20, t = 10, b = 10, unit = 'pt'))
      ) +
      ylab(title) +
      #scale_x_continuous(breaks=seq(2010, 2050, 10)) +
      {if(!is.null(legend$nrow)) guides(color = guide_legend(nrow = legend$nrow),linetype = guide_legend(nrow = legend$nrow))} +
      {if(!(is.null(max))) coord_cartesian(ylim=c(NA, max))} +
      {if(!(is.null(min))) expand_limits(y=c(min))} +
      {if((percentage == TRUE) & (is.null(max))) scale_y_continuous(labels = scales::percent_format(accuracy = 1)) } + 
      {if((percentage == TRUE) & !(is.null(max)))  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, max, by = 0.2)) }
    
    if(colorDim=="Variable"){
      if(!(is.null(labels))){
        fullVars <- levels(data$Variable)
        names(fullVars) <- labels
        vars <- fullVars[unique(data$Variable)]
        gg <- gg + scale_color_manual(values = setNames(color[names(vars)],vars), labels = setNames(names(vars),vars),drop=FALSE)
      }
    } else {
      lvl <- levels(plotData[[colorDim]])
      lvl <- lvl[lvl!="reference"]
      gg <- gg +
        scale_color_manual(values = setNames(color[lvl],lvl),drop=FALSE)
    }
    
    if(saveFile){
      
      dir.create(saveFolder, recursive = T, showWarnings = FALSE)
      
      if(facetDim=="Variable"){
        varName <- setNames(names(labelsVars),labelsVars)
      }
      
      lapply(unique(plotData[[facetDim]]),function(selectedFacet){
        singlePlotData <- plotData[plotData[[facetDim]]==selectedFacet,]
        
        g <- ggplot(singlePlotData,aes_string(x=xdim ,y=ydim, color=colorDim ,linetype=linetypeDim)) +
          geom_hline(yintercept=0, color = "black", size=1, alpha = 0.15) +
          geom_vline(xintercept=2020, color = "black", size=1, alpha = 0.15, linetype = "dashed") +
          geom_line(size=1) +
          geom_point() +
          {if(!(nrow(hist)==0) & !(facetDim=="Model")) geom_line(data=histFull,size=1,aes(alpha = source),color="black") } + #,lty="11" 
          {if(!(nrow(hist)==0) & !(facetDim=="Model")) geom_point(data=histFull,size=1, stroke = 1.5,shape=4,aes(alpha = source),color="black") } +
          {if(!(nrow(hist)==0) & !(facetDim=="Model")) geom_text(data=histLabel, aes(label = source), color="black", hjust ="inward", vjust=1.5, size = 6) } +
          {if(!(nrow(hist)==0) & !(facetDim=="Model")) scale_alpha_manual(values = as.numeric(alpha[unique(histFull$source)]),guide="none") } +
          theme_singlePlot() +
          #theme(
          #  legend.key.size = unit(30,"pt"),
          #  legend.text = element_text(size=16, margin = margin(r = 20, t = 10, b = 10, unit = 'pt'))
          #) +
          ylab(title) +
          #scale_x_continuous(breaks=seq(2010, 2050, 10)) +
          {if(!is.null(legend$nrow)) guides(color = guide_legend(nrow = legend$nrow),linetype = guide_legend(nrow = legend$nrow))} +
          {if(!(is.null(max))) coord_cartesian(ylim=c(NA, max))} +
          {if(!(is.null(min))) expand_limits(y=c(min))} +
          {if(percentage) scale_y_continuous(labels = scales::percent_format(accuracy = 1)) }
        
        if(colorDim=="Variable"){
          if(!(is.null(labels))){
            fullVars <- levels(data$Variable)
            names(fullVars) <- labels
            vars <- fullVars[unique(data$Variable)]
            g <- g + scale_color_manual(values = setNames(color[names(vars)],vars), labels = setNames(names(vars),vars),drop=FALSE)
          }
        } else {
          lvl <- levels(plotData[[colorDim]])
          lvl <- lvl[lvl!="reference"]
          g <- g +
            scale_color_manual(values = setNames(color[lvl],lvl),drop=FALSE)
        }
        
        ggsave(paste0(saveFolder, "_", ifelse(facetDim=="Variable",varName[selectedFacet],gsub("\\|","_", selectedFacet)), ".svg"), g, device="svg", width = 18, height = 12, dpi=100, units = "in")
        ggsave(paste0(saveFolder, "_", ifelse(facetDim=="Variable",varName[selectedFacet],gsub("\\|","_", selectedFacet)), ".png"), g, device="png", width = 18, height = 12, dpi=100, units = "in")
        
      })
    }
    
    return(gg)
  }
  
  # function to create bar Plots
  barPlot <- function(data, scen=NULL,total=NULL,period=c(2020,2030,2050),title="", legend = list(nrow=NULL), min=0, max=NULL, labels=NULL, pattern=NULL, facet = list(nrow=NULL,ncol=6), saveFile=FALSE, saveFolder=""){
    
    if(nrow(data)==0){
      return(NULL)
    }
    
    if(is.null(scen)){
      scen <- unique(data[!(data$Model == "reference"),]$Scenario)
    }
    
    plotData <- data[((!(data$Model=="reference")) & (data$Scenario %in% scen)),]
    plotData <- plotData[(plotData$Period %in% period),]
    plotData$Scenario <- factor(plotData$Scenario , levels = scen)
    plotData$Model <- factor(plotData$Model, level = levels(plotData$Model)[!levels(plotData$Model) == c("reference")])
    
    if(nrow(plotData)==0){
      return(NULL)
    }
    
    tmp <- total[(total$Period %in% period) & (total$Model == "reference"),]
    tmp$Model <- tmp$Scenario
    histTotal <- NULL
    for(sc in scen){
      tmp$Scenario <- sc
      histTotal <- rbind(histTotal,tmp)
    }
    plotTotal <- rbind(histTotal,total[((total$Period %in% period) & (total$Scenario %in% scen) & !(total$Model == "reference")),])
    plotTotal$Scenario <- factor(plotTotal$Scenario , levels = scen)
    #plotTotal$Model <- factor(plotTotal$Model, level = levels(plotData$Model))
    plotTotal$Model <- as.character(plotTotal$Model) # clear factor
    
    hist <- data[(data$Model=="reference") & (data$Period %in% period),]
    
    if(!(nrow(hist)==0)){
      tmp <- hist
      tmp$Model <- tmp$Scenario
      histFull <- NULL
      for(sc in scen){
        tmp$Scenario <- sc
        histFull <- rbind(histFull,tmp)
      }
      histFull$Scenario <- factor(histFull$Scenario , levels = scen)
      histFull$Model <- as.character(histFull$Model) #remove factor
    }
    
    if(!(is.null(labels))){
      fullVars <- levels(data$Variable)
      names(fullVars) <- labels
      labelsVars <- fullVars[fullVars %in% unique(data$Variable)]
    }
    
    if(!(is.null(pattern))){
      fullVars <- levels(data$Variable)
      names(fullVars) <- pattern
      patternVars <- fullVars[fullVars %in% unique(data$Variable)]
    }
    
    nPer <- length(unique(plotData$Period)) 
    if(!(is.null(facet$ncol))){
      plotsPerLine <- facet$ncol
    } else {
      plotsPerLine <- (length(scen)*nPer)/facet$nrow
    }
    numberOfLines <- ceiling((length(scen)*nPer)/plotsPerLine)
    
    gg <- ggplot(plotData,aes(x=Model,y=Value,fill=Variable,pattern=Variable)) +
      geom_hline(yintercept=0, color = "black", size=1, alpha = 0.15) +
      {if(!(nrow(hist)==0) & is.null(pattern)) geom_bar(data=histFull,stat='identity', alpha = 0.6)} +
      #{if(!(nrow(hist)==0) & !is.null(pattern)) geom_bar(data=histFull,stat='identity', alpha = 0.6)} +
      #{if(!(nrow(hist)==0) & !is.null(pattern)) geom_bar_pattern(data=histFull,stat='identity', pattern_key_scale_factor = 0.3, pattern_density=ifelse((numberOfLines==1), 0.5/3, 0.5), pattern_spacing = 0.03, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4, alpha = 0.6)} +
      #{if(!(nrow(hist)==0) & !is.null(pattern)) geom_bar_pattern(data=histFull,stat='identity', pattern_key_scale_factor = 1, pattern_density=0.5, pattern_spacing = 0.1, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4, alpha = 0.6)} +
      {if(!(nrow(hist)==0) & !is.null(pattern)) geom_bar_pattern(data=histFull,stat='identity', pattern_key_scale_factor = 0.5, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4, alpha = 0.6)} +
      {if(is.null(pattern)) geom_bar(stat='identity')} +
      #{if(!is.null(pattern)) geom_bar_pattern(stat='identity', pattern_key_scale_factor = 0.3,  pattern_density=ifelse((numberOfLines==1), 0.5/3, 0.5), pattern_spacing = 0.03, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4)} +
      {if(!is.null(pattern)) geom_bar_pattern(stat='identity', pattern_key_scale_factor = 0.5, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4)} +
      #{if(!is.null(pattern)) geom_bar(stat='identity')} +
      #{if(!is.null(pattern)) geom_bar_pattern(stat='identity')} +
      {if(nrow(plotTotal)>0) geom_point(data=plotTotal,aes(x=Model,y=Value), shape=23,fill="black",color="white", alpha = 0.7,show.legend = F, size = 2, stroke = 2)} +
      theme_singleColumn() +
      {if(is.null(pattern)) theme(legend.position="bottom")} +
      facet_wrap(~Scenario+Period, scales='free_x', nrow=facet$nrow, ncol=facet$ncol, 
                 labeller = labeller(Scenario = 
                                       function(Variable, Value) {
                                         if(nPer==1){
                                           out <- Variable
                                         } else {
                                           out <- rep("",length(scen)*nPer)
                                           for (i in 1:length(scen)){
                                             out[(((i-1)*nPer)+2)] <- Variable[(((i-1)*nPer)+2)]
                                           }
                                         }
                                         return(out) 
                                       }),drop = FALSE) +
      scale_x_discrete(drop=FALSE) +
      {if(!is.null(labels)) scale_fill_manual(values = setNames(color[names(labelsVars)],labelsVars), labels = setNames(names(labelsVars),labelsVars),drop=FALSE) } +
      {if(!is.null(pattern)) scale_pattern_manual(values = setNames(c("none",names(patternVars)),c("Total",patternVars)))} +
      {if(!is.null(pattern)) guides(fill = guide_legend(override.aes = list(pattern = setNames(names(patternVars),patternVars) ) ) )} +
      {if(!is.null(pattern)) guides(pattern="none")} +
      ylab(title) +
      {if(numberOfLines > 2) theme(axis.text.x = element_text(size = 10))} +
      {if(!is.null(legend$nrow)) guides(fill = guide_legend(nrow = legend$nrow))} +
      {if(is.null(max)) expand_limits(y=c(min))} +
      {if(!(is.null(max))) expand_limits(y=c(min,max))}
    
    if((numberOfLines==1) & (length(scen)==1)){
      gg <- gg + theme(panel.spacing = unit(1.5*3, "lines"))
    }
    
    layoutPos <- NULL
    for (i in 1:(plotsPerLine/nPer)){
      layoutPos[i] <- 15 + 12*(i-1)
    }
    gg <- spaceFacet(gg,layout=layoutPos)
    
    if(saveFile){
      
      dir.create(saveFolder, recursive = T, showWarnings = FALSE)
      
      facetDim <- "Scenario"
      lapply(unique(plotData[[facetDim]]),function(selectedFacet){
        singlePlotData <- plotData[plotData[[facetDim]]==selectedFacet,]
        if(!(nrow(hist)==0)) singleHistData <- histFull[histFull[[facetDim]]==selectedFacet,]
        if(nrow(plotTotal)>0) singleplotTotal <- plotTotal[plotTotal[[facetDim]]==selectedFacet,]
        
        g <- ggplot(singlePlotData,aes(x=Model,y=Value,fill=Variable,pattern=Variable)) +
          geom_hline(yintercept=0, color = "black", size=1, alpha = 0.15) +
          {if(!(nrow(hist)==0) & is.null(pattern)) geom_bar(data=singleHistData,stat='identity', alpha = 0.6)} +
          {if(!(nrow(hist)==0) & !is.null(pattern)) geom_bar_pattern(data=singleHistData,stat='identity', pattern_key_scale_factor = 0.3, pattern_density=ifelse((numberOfLines==1), 0.5/3, 0.5), pattern_spacing = 0.03, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4, alpha = 0.6)} +
          {if(is.null(pattern)) geom_bar(stat='identity')} +
          {if(!is.null(pattern)) geom_bar_pattern(stat='identity', pattern_key_scale_factor = 0.3,  pattern_density=ifelse((numberOfLines==1), 0.5/3, 0.5), pattern_spacing = 0.03, pattern_colour = "white", pattern_fill = "white", pattern_alpha=0.4)} +
          {if(nrow(singleplotTotal)>0) geom_point(data=singleplotTotal,aes(x=Model,y=Value), shape=23,fill="black",color="white", alpha = 0.7,show.legend = F, size = 3, stroke = 2)} +
          theme_singlePlot() +
          facet_wrap(~Period, scales='free_x', nrow=facet$nrow, ncol=facet$ncol,drop = FALSE) +
          scale_x_discrete(drop=FALSE) +
          {if(!is.null(labels)) scale_fill_manual(values = setNames(color[names(labelsVars)],labelsVars), labels = setNames(names(labelsVars),labelsVars),drop=FALSE) } +
          {if(!is.null(pattern)) scale_pattern_manual(values = setNames(c("none",names(patternVars)),c("Total",patternVars)))} +
          {if(!is.null(pattern)) guides(fill = guide_legend(override.aes = list(pattern = setNames(names(patternVars),patternVars) ) ) )} +
          #{if(!is.null(pattern)) guides(fill = guide_legend(override.aes = list(pattern = setNames(names(patternVars),patternVars)) , nrow = legend$nrow ) )} +
          {if(!is.null(pattern)) guides(pattern="none")} +
          ylab(title) +
          {if(numberOfLines > 2) theme(axis.text.x = element_text(size = 10))} +
          {if(!is.null(legend$nrow)) guides(fill = guide_legend(nrow = legend$nrow))} +
          {if(is.null(max)) expand_limits(y=c(min))} +
          {if(!(is.null(max))) expand_limits(y=c(min,max))} + 
          theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size=14))
        
        ggsave(paste0(saveFolder, "_", gsub("\\|","_", selectedFacet), ".svg"), g, device="svg", width = 18, height = 12, dpi=100, units = "in")
        ggsave(paste0(saveFolder, "_", gsub("\\|","_", selectedFacet), ".png"), g, device="png", width = 18, height = 12, dpi=100, units = "in")
        
      })
    }
    
    
    return(gg)
  }
  
  #function to space facets and remove crop from facet titles
  spaceFacet <- function(gg,size=3,layout=c(15,27)){
    gt = ggplot_gtable(ggplot_build(gg))
    #gtable_show_layout(gt)
    for (item in layout){
      if(item <= length(gt$widths)){
        gt$widths[item] = size*gt$widths[item]
      }
    }
    #remove crop from titles
    for(i in which(grepl("strip-r|strip-t", gt$layout$name))){
      gt$grobs[[i]]$layout$clip <- "off"
    }
    return(as.ggplot(gt))
  }
  
  
  # function to filter data recursively
  filterData <- function (data, l=NULL) {
    
    loopList <- function (lt, varList, dd) { # function to filter only vars that have values, in the highest node levels possible
      for (node in names(lt)){
        if(length(intersect(node,unique(dd$Variable)))==0){ #remove node if it has no data
          varList <- varList[varList != as.character(node)]
        }
        if(!(is.null(names(lt[[node]])))){ # node is not a terminal node
          if(length(intersect(unique(unlist((strsplit(names(unlist(lt[[node]])),"\\.")))),unique(dd$Variable)))>0){ #remove node if any children has data
            varList <- varList[varList != as.character(node)]
          }
          varList <- loopList(lt[[node]], varList, dd)
        }
      }
      return(varList)
    }
    
    vars <- unique(unlist((strsplit(names(unlist(l$vars)),"\\."))))
    
    out <- do.call(rbind,
                   lapply(unique(data$Model), function(model){
                     do.call(rbind,
                             lapply(unique(data$Scenario), function(scenario){
                               return(data[data$Model == model & data$Scenario == scenario & data$Variable %in% loopList(lt=l$vars, varList=vars, dd=data[data$Model == model & data$Scenario == scenario,]),])
                             })
                     )
                   })
    )
    
    #out$Variable <- factor(out$Variable , levels = unique(unlist((strsplit(names(unlist(plotList[[plotName]]$vars)),"\\.")))))
    out$Variable <- factor(out$Variable , levels = vars)
    out$Model <- factor(out$Model , levels = levels(data$Model))
    out$Scenario <- factor(out$Scenario , levels = levels(data$Scenario))
    #intersect(unique(unlist((strsplit(names(unlist(plotList[[plotName]]$vars)),"\\.")))),unique(out$Variable))) #order
    
    return(out)
    
  }
  
  
  #select Regions
  if(region=="EU27"){
    currdf <- origDf[((origDf$Region %in% c("EU27", "Europe", "Europe (excl. Turkey)", "Europe (incl. Turkey)") & !(origDf$Model == "Euro-Calliope") ) | (origDf$Model == "MEESA" & origDf$Region == "EU27 & UK") | (origDf$Model == "Euro-Calliope" & origDf$Region == "EU27")),]
  } else if (region=="EU28"){
    currdf <- origDf[(origDf$Region %in% c("EU27 & UK", "Europe", "Europe (excl. Turkey)", "Europe (incl. Turkey)") & !(origDf$Model == "Euro-Calliope") ) | (origDf$Model == "Euro-Calliope" & origDf$Region == "Europe"),] 
  } else {
    currdf <- origDf[(origDf$Region %in% region),] 
  }
  #copy 2019 or 2008 Eurostat values to 2020
  if(region=="EU27"){
    if(plotArgs$type == "bar"){
      eurostatLabel <- "2019 Eurostat"
      tmp <- currdf[currdf$Scenario == " Eurostat" & currdf$Period==2019,]
      tmp$Period <- 2020
      tmp$Scenario <- " 2019 Eurostat"
      currdf <- rbind(currdf[!(currdf$Scenario == " Eurostat"),], tmp)
    }
  } else if(region=="EU28"){
    if(plotArgs$type == "bar"){
      eurostatLabel <- "2018 Eurostat"
      tmp <- currdf[currdf$Scenario == " Eurostat" & currdf$Period==2018,]
      tmp$Period <- 2020
      tmp$Scenario <- " 2018 Eurostat"
      currdf <- rbind(currdf[!(currdf$Scenario %in% c(" Eurostat"," EU27Ref")),], tmp)
    } else {
      currdf <- currdf[!(currdf$Scenario %in% c(" EU27Ref")),]
    }
  }  
  #select scenarios
  currdf <- currdf[(currdf$Scenario %in% c(as.character(unique(currdf[currdf$Model == "reference",]$Scenario)), scenarios)),]
  #currdf$Scenario <- factor(currdf$Scenario , levels = c(as.character(unique(currdf[currdf$Model == "reference",]$Scenario)), scenarios))
  #return if plot needs a scenario not present in the data
  if(any(!(plotArgs$arg$scen %in% unique(currdf$Scenario)))){
    print("plot not created due to missing scenario.")
    return()
  } 
  #create plots
  if(plotArgs$type == "line"){
    if((scenName=="paradigmShift" || scenName=="techConstraint") & (!("scen" %in% names(plotArgs$arg)))){
    #  d <- filterData(data=currdf[!(currdf$Scenario %in% c(paste0(" ", eurostatLabel),"DIAG-NPI")),],l=plotArgs)   # filtering data for plot
      d <- filterData(data=currdf[!(currdf$Scenario %in% c("DIAG-NPI")),],l=plotArgs)   # filtering data for plot
      p <- do.call(linePlot, c(list(data=d,saveFolder=paste0(indivFilePath , "/" , plotName),scen=plotArgs$arg$scen[!(plotArgs$arg$scen=="DIAG-NPI")]),plotArgs$arg))
    } else {
    #  d <- filterData(data=currdf[!(currdf$Scenario %in% c(paste0(" ", eurostatLabel))),],l=plotArgs)   # filtering data for plot  
      d <- filterData(data=currdf,l=plotArgs)   # filtering data for plot  
      p <- do.call(linePlot, c(list(data=d,saveFolder=paste0(indivFilePath , "/" , plotName)),plotArgs$arg))
    }
  } else if (plotArgs$type == "bar"){
    if((scenName=="paradigmShift" || scenName=="techConstraint") & (!("scen" %in% names(plotArgs$arg)))){
      d <- filterData(data=currdf[!(currdf$Scenario %in% c("DIAG-NPI")),],l=plotArgs)   # filtering data for plot
      totalSum <- currdf[(currdf$Variable %in%  names(plotArgs$vars)),] %>%
        group_by(Model,Scenario,Period) %>% summarize(Value = sum(Value),.groups="drop") %>% mutate(Variable="Total")
      p <- do.call(barPlot, c(list(data=d,total=totalSum,saveFolder=paste0(indivFilePath , "/" , plotName),scen=plotArgs$arg$scen[!(plotArgs$arg$scen=="DIAG-NPI")]),plotArgs$arg))
    } else {
      d <- filterData(data=currdf,l=plotArgs)    # filtering data for plot
      totalSum <- currdf[(currdf$Variable %in%  names(plotArgs$vars)),] %>%
        group_by(Model,Scenario,Period) %>% summarize(Value = sum(Value),.groups="drop") %>% mutate(Variable="Total")
      p <- do.call(barPlot, c(list(data=d,total=totalSum,saveFolder=paste0(indivFilePath, "/" , plotName)),plotArgs$arg))
    }
  }
  return(p)
} 
