HRPacePlot <- function(analysisdata, HR, rv){
    data <- analysisdata()
  
    plot <- ggplot(data) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=Inf), fill=HR$color[5],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=HR$thresh[5]), fill=HR$color[4],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=HR$thresh[4]), fill=HR$color[3],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=HR$thresh[3]), fill=HR$color[2],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=HR$thresh[2]), fill=HR$color[1],
                xmin=-Inf, xmax=Inf, alpha=1) +
      scale_y_continuous(name='Heart Rate (bpm)', breaks=seq(min(data$hr), max(data$hr), 2),
                         limits=c(HR$thresh[2], max(max(data$hr)+1, HR$thresh[5])),
                         sec.axis = dup_axis(breaks=(HR$thresh[1:5] + HR$thresh[2:6])/2,
                                             labels=HR$label[1:5], name='Heart Rate Zone')) +
      geom_hline(yintercept = HR$thresh[2:5]) +
      scale_x_continuous(trans = "reverse", name='Pace (min/km)',
                         breaks=seq(floor(min(data$pace)), ceiling(max(data$pace)), 1/12),
                         labels=format_pace(seq(floor(min(data$pace)), ceiling(max(data$pace)), 1/12))) + # reverse
      theme(text = element_text(size=20)) +
      geom_point(aes(pace, hr))
  
    if(nrow(data) >= 10){
      span <- 0.75
      m <- loess(hr ~ pace, data = data, span=span)
      md <- data.frame(pace=data$pace, hr=m$fitted)
      plot <- plot +
        geom_smooth(aes(pace, hr), formula=y~x, method='loess', span=span) + # TODO run loess only once
        geom_smooth(aes(pace, hr), formula=y~x, method='lm', se=FALSE, color='red', linetype='dashed')
     
      hrdpval <- rv$hrdpCache
      if( !is.null(hrdpval) ){
        data <- data.frame(x=hrdpval$pace, y=hrdpval$hr)
        plot <- plot + 
          geom_point(aes(x,y,size=5), data=data) +
          guides(size=FALSE)
      }
    } else {
      plot <- plot +
        ggtitle('Not enough data')
    }
    plot
}


RacePlot <- function(fitdata, hlCallbackJs){
  data <- fitdata()

  drawCallbackJs <- 'function(g, i){
    Shiny.setInputValue("rangeStart", Dygraph.dateString_(g.xAxisRange()[0], "UTC"))
    Shiny.setInputValue("rangeStop", Dygraph.dateString_(g.xAxisRange()[1], "UTC"))
  }'
  formatPaceJs <- 'function(value, opts, seriesName, dygraph, row, col){
    valm = Math.floor(value)
    vals = Math.round((value - valm) * 60)
    vals = ("00" + vals).substr(-2,2)
    return(valm + ":" + vals)
  }'
  
  dygraph(xts(data[c('pace', 'altitude')],
              order.by=data$timestamp), group='global') %>%
    dyCallbacks(highlightCallback=hlCallbackJs,
                drawCallback=drawCallbackJs) %>%
    dyOptions(fillAlpha = 0.3, connectSeparatedPoints = T) %>%
    dySeries("altitude", axis = 'y2', strokeWidth = 0, fillGraph = T) %>%
    dyRangeSelector() %>%
    dyLegend(width=400) %>%
    dyAxis("y", valueRange=c(0, 1.2 + max(data$pace)), valueFormatter=formatPaceJs) %>%
    dyAxis("y2", valueRange=c(min(data$altitude),
                              min(data$altitude) + (max(data$altitude) - min(data$altitude)) * 2)) %>%
    dyAxis("x", drawGrid = FALSE)
}

ZonePlot <- function(fitdata, HR, hlCallbackJs){
  data <- fitdata()
  
  tickerJs <- 'function(min, max, pixels, opts, dygraph, vals){return([])}'

  dygraph(xts(data[c('hr')], order.by=data$timestamp),
          group='global') %>%
    dyOptions(colors=c('black'), fillAlpha=0.1, fillGraph=TRUE) %>%
    dyCallbacks(highlightCallback=hlCallbackJs) %>%
    dyAxis("x", drawGrid=FALSE) %>%
    dyAxis('y', valueRange=c(HR$thresh[1], HR$thresh[6] + 1), ticker=tickerJs) %>%
    dyLimit(HR$thresh[1], HR$thresh[1], labelLoc="left", color="black") %>%
    dyLimit(HR$thresh[2], HR$thresh[2], labelLoc="left", color="black") %>%
    dyLimit(HR$thresh[3], HR$thresh[3], labelLoc="left", color="black") %>%
    dyLimit(HR$thresh[4], HR$thresh[4], labelLoc="left", color="black") %>%
    dyLimit(HR$thresh[5], HR$thresh[5], labelLoc="left", color="black") %>%
    dyShading(HR$thresh[1], HR$thresh[2], color=HR$color[1], axis="y") %>%
    dyShading(HR$thresh[2], HR$thresh[3], color=HR$color[2], axis="y") %>%
    dyShading(HR$thresh[3], HR$thresh[4], color=HR$color[3], axis="y") %>%
    dyShading(HR$thresh[4], HR$thresh[5], color=HR$color[4], axis="y") %>%
    dyShading(HR$thresh[5], HR$thresh[6], color=HR$color[5], axis="y")
}