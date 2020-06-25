run_rename_select <- function(data){
  
  cols <- list()
  drops <- c()
  if('enhanced_speed' %in% names(data) && 'speed' %in% names(data)){
    drops <- c(drops, 'speed')
  }
  if('enhanced_altitude' %in% names(data) && 'altitude' %in% names(data)){
    drops <- c(drops, 'altitude')
  }
  cols$timestamp = 'timestamp'
  cols$lat = 'position_lat'
  cols$long = 'position_long'
  cols$distance = 'distance'
  cols$hr <- 'heart_rate'
  cols$speed <- 'enhanced_speed'
  cols$altitude <- 'enhanced_altitude'

  data %>%
    select(-drops) %>%
    rename(!!!cols) %>%
    select(!!!names(cols))
}

run_fix_resolution <- function(data){

  durations <- lead(data$timestamp) - data$timestamp
  durations <- durations[1 : (length(durations) - 1)]
  if ( mean(durations[length(durations)-1]) == 1 ) {
    return(data)
  }
  message(mean(durations[length(durations)-1]))
  message('Attempting to fix low-resolution data')
  datafull <- data.frame(matrix(ncol=length(names(data)),
                                nrow=0))
  colnames(datafull) <- names(data)
  for(td in 1:(nrow(data)-1) ){
    currrow <- data[td,]
    nextrow <- data[td+1,]
    dur <- as.numeric(durations[td], units='secs')
    # add start record
    datafull <- rbind(datafull, currrow)
    # interpolate intermediate records
    for(tinterp in 1:(dur-1)){
      irow <- list(timestamp = currrow$timestamp + tinterp,
                   lat = currrow$lat + tinterp * (nextrow$lat - currrow$lat) / dur,
                   long = currrow$long + tinterp * (nextrow$long - currrow$long) / dur,
                   distance = currrow$distance + tinterp * (nextrow$distance - currrow$distance) / dur,
                   speed = currrow$speed + tinterp * (nextrow$speed - currrow$speed) / dur,
                   altitude = currrow$altitude + tinterp * (nextrow$altitude - currrow$altitude) / dur,
                   hr = round(currrow$hr + tinterp * (nextrow$hr - currrow$hr) / dur))
      datafull <- rbind(datafull, irow)
    }
  }
  # append the last record and return
  rbind(datafull, data[nrow(data),])
}

run_fix_units <- function(data){
  data %>%
    mutate(timestamp=as.POSIXct(timestamp, tz='UTC', origin="1989-12-31"))
}

run_fix_elevation <- function(data, skip=TRUE, cache=NULL){
  if (skip) {
    return(data)
  }
  if (!is.null(cache)) {
    return(data %>% mutate(altitude=cache))
  } else {
    prj_dd <- "+proj=longlat +datum=WGS84 +no_defs"
    suppressWarnings({
      res <- get_elev_point(data.frame(x=data$long, y=data$lat), prj=prj_dd, src='aws', z=10)
    })
    return(data %>% mutate(altitude=res$elevation))
  }
}

run_add_masks <- function(data, slopeWindow, hrWindow, speedWindow, slopeLimit, hrLimit, speedLimit){
  data %>%
    mutate(slope = (altitude - lag(altitude)) /
                  as.numeric(timestamp - lag(timestamp), units='secs')) %>%
    mutate(hrfo = (hr - lag(hr)) /
                  as.numeric(timestamp - lag(timestamp), units='secs')) %>%
    mutate(speedfo = (speed - lag(speed)) /
                  as.numeric(timestamp - lag(timestamp), units='secs')) %>%
    replace_na(list(slope=0, hrfo=0, speedfo=0)) %>%
    mutate(slopemask = abs(rollmean(slope, k=slopeWindow, align='right', fill=0)) <= slopeLimit) %>%
    mutate(hrmask = abs(rollmean(hrfo, k=hrWindow, align='right', fill=0)) <= hrLimit) %>%
    mutate(speedmask = abs(rollmean(speedfo, k=speedWindow, align='right', fill=0)) <= speedLimit) %>%
    mutate(combinedmask = speedmask & hrmask & slopemask)
}

run_add_pace <- function(data){
  data %>%
    mutate(pace=60/3.6/sapply(speed, function(x) { max(x, 75/36) } ))
}

run_add_hrzone <- function(data, breaks, labels){
  # note that breaks for cut() differ in their meaning from breaks in shiny App
  breaks <- c(-Inf, breaks[2:(length(breaks)-1)], Inf)
  df <- data %>%
    mutate(hrzone = cut(hr, breaks=breaks, labels=labels))
  df
}

run_filter_fitrange <- function(df, start=NULL, stop=NULL){
  dfmin <- min(df$timestamp)
  dfmax <- max(df$timestamp)
  start <- as.POSIXct(start, tz='UTC', origin="1989-12-31")
  stop <- as.POSIXct(stop, tz='UTC', origin="1989-12-31")
  if (start > dfmax || start < dfmin ) {
    cat(sprintf('Invalid start %s\n', start))
    cat(sprintf('min/max: %s/%s\n', dfmin, dfmax))
    return(df)
  }
  if (stop > dfmax || stop < dfmin ) {
    cat(sprintf('Invalid stop %s\n', stop))
    cat(sprintf('min/max: %s/%s\n', dfmin, dfmax))
    return(df)
  }
  if ( is.null(start) || is.null(stop) || start > stop ) {
    cat(sprintf('start/stop invalid: %s/%s\n', start, stop))
    return(df)
  }
  df %>% filter(timestamp >= start, timestamp <= stop)
}

format_pace <- function(pace){
  pre <- floor(pace)
  sprintf('%d:%02d', pre, floor(60 * (pace - pre)))
}

calc_hrdp <- function(data, span=0.7){
  data <- data %>% arrange(speed)
  if(nrow(data) < 10){
    stop('not enough data')
  }
  gdata <- data %>%
              group_by(hr) %>%
              summarise(var=var(speed)) %>%
              drop_na %>%
              pull
  gvar <- var(data$speed)
  meanvar <- mean(gdata)
  if( 1.5 * meanvar >= gvar ) {
    stop('data too scattered')
  }

  # let's, go
  hrdp <- list()

  # fit linear model through all data and get coefficients
  modellm <- lm(hr ~ speed, data = data)
  intercept <- modellm$coefficients[[1]]
  slope <- modellm$coefficients[[2]]

  # fit non-linear as in plot
  model <- loess(hr ~ speed, data = data, span=span)
  data$hrfit <- model$fitted
  # get distance from non-linear fitted values to linear fit
  data$dist <- ( data$hrfit - data$speed * slope - intercept) / sqrt(slope**2 + 1)
  data$distfo <- (data$dist - lag(data$dist)) / (data$speed - lag(data$speed))
  # zero points in first order estimate
  data <- data %>% drop_na() %>% mutate(zeros = as.logical((distfo >0) - (lag(distfo)>0)) )

  # collect results
  hrdp$speed <- data %>% filter(dist > 0, zeros==T) %>% tail(1) %>% select('speed') %>% pull()
  if( length(hrdp$speed) == 0 ){
    stop('could not find a local maximum')
  }
  hrdp$pace <- 60/3.6/hrdp$speed
  hrdp$hr <- data %>% filter(speed==hrdp$speed) %>% select('hrfit') %>% tail(1) %>% pull() %>% round()
  hrdp
}