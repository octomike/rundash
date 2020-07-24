HRPacePlot <- function(analysisdata, hrColors, hrBreaks, hrLabels, rv){
    data <- analysisdata()
  
    plot <- ggplot(data) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=Inf), fill=hrColors[5],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=hrBreaks()[5]), fill=hrColors[4],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=hrBreaks()[4]), fill=hrColors[3],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=hrBreaks()[3]), fill=hrColors[2],
                xmin=-Inf, xmax=Inf, alpha=1) +
      geom_rect(mapping=aes(ymin=-Inf, ymax=hrBreaks()[2]), fill=hrColors[1],
                xmin=-Inf, xmax=Inf, alpha=1) +
      scale_y_continuous(name='Heart Rate', breaks=unique(data$hr),
                         limits=c(hrBreaks()[2], hrBreaks()[6]),
                         sec.axis = dup_axis(breaks=(hrBreaks()[1:5] + hrBreaks()[2:6])/2,
                                             labels=hrLabels)) +
      geom_hline(yintercept = hrBreaks()[2:5]) +
      scale_x_continuous(trans = "identity") + # reverse
      geom_point(aes(speed, hr))
  
    if(nrow(data) >= 10){
      span <- 0.75
      m <- loess(hr ~ speed, data = data, span=span)
      md <- data.frame(speed=data$speed, hr=m$fitted)
      plot <- plot +
        geom_smooth(aes(speed, hr), formula=y~x, method='loess', span=span) + # TODO run loess only once
        geom_smooth(aes(speed, hr), formula=y~x, method='lm', se=FALSE, color='red', linetype='dashed')
     
      hrdpval <- rv$hrdpCache
      if( !is.null(hrdpval) ){
        p0 <- c(hrdpval$speed, hrdpval$hr)
        data <- data.frame(x=c(3.5, hrdpval$speed), y=c(140, hrdpval$hr))
        print(data)
        print(predict(m, hrdpval$speed))
        plot <- plot + 
          geom_point(aes(x,y, size=5), data=data) +
          geom_line(aes(x,y), data=data)
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
  
  dygraph(xts(data[c('pace', 'altitude')],
              order.by=data$timestamp), group='global') %>%
    dyCallbacks(highlightCallback=hlCallbackJs,
                drawCallback=drawCallbackJs) %>%
    dySeries("altitude", axis = 'y2') %>%
    dyRangeSelector() %>%
    dyAxis("y", valueRange=c(0, max(data$pace))) %>%
    dyAxis("y2", valueRange=c(min(data$altitude),
                              min(data$altitude) +
                      (max(data$altitude) - min(data$altitude)) *2)) %>%
    dyAxis("x", drawGrid = FALSE)
}

ZonePlot <- function(fitdata, hrBreaks, hrColors, hlCallbackJs){
  data <- fitdata()
  hrB <- hrBreaks()
  
  tickerJs <- 'function(min, max, pixels, opts, dygraph, vals){return([])}'

  dygraph(xts(data[c('hr')], order.by=data$timestamp),
          group='global') %>%
    dyOptions(colors=c('black'), fillAlpha=0.1, fillGraph=TRUE) %>%
    dyCallbacks(highlightCallback=hlCallbackJs) %>%
    dyAxis("x", drawGrid=FALSE) %>%
    dyAxis('y', valueRange=c(hrB[1], hrB[6] + 1), ticker=tickerJs) %>%
    dyLimit(hrB[1], hrB[1], labelLoc="left", color="black") %>%
    dyLimit(hrB[2], hrB[2], labelLoc="left", color="black") %>%
    dyLimit(hrB[3], hrB[3], labelLoc="left", color="black") %>%
    dyLimit(hrB[4], hrB[4], labelLoc="left", color="black") %>%
    dyLimit(hrB[5], hrB[5], labelLoc="left", color="black") %>%
    dyShading(hrB[1], hrB[2], color=hrColors[1], axis="y") %>%
    dyShading(hrB[2], hrB[3], color=hrColors[2], axis="y") %>%
    dyShading(hrB[3], hrB[4], color=hrColors[3], axis="y") %>%
    dyShading(hrB[4], hrB[5], color=hrColors[4], axis="y") %>%
    dyShading(hrB[5], hrB[6], color=hrColors[5], axis="y")
}