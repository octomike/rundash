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

run_add_gain <- function(data){
  data %>%
    mutate(gain=altitude - lag(altitude)) %>%
    replace_na(list(gain=0))
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