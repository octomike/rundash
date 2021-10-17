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
    select(-all_of(drops)) %>%
    rename(!!!cols) %>%
    select(!!!names(cols))
}

run_fix_resolution <- function(data){

  durations <- head(lead(data$timestamp) - data$timestamp, -1)
  if ( all(durations == 1) ) {
    return(data)
  }
  if( median(durations) == 1 ){
    message('Filling stops in your run')
  } else {
    message(sprintf('Detected low-resolution data (rate = %f s), interpolating', mean(durations)))
  }

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
    if(dur==1){
      next
    }
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

run_add_features <- function(data){
    data <- data %>%
              mutate(slope = (altitude - lag(altitude)) /
                             as.numeric(timestamp - lag(timestamp), units='secs')) %>%
              mutate(hrchange = (hr - lag(hr)) /
                             as.numeric(timestamp - lag(timestamp), units='secs'))
    data$slope[1] <- 0
    data$hrchange[1] <- 0
    data
}


run_add_masks <- function(data, slopeWindow, hrWindow, speedWindow, slopeLimit, hrLimit, speedLimit){
  data %>%
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
  breaks <- c(-Inf, breaks[2:5], Inf)
  df <- data %>%
    mutate(hrzone = cut(hr, breaks=breaks, labels=labels))
  df
}

run_filter_fitrange <- function(df, start=NULL, stop=NULL){
  dfmin <- min(df$timestamp)
  dfmax <- max(df$timestamp)
  if ( is.null(start) || is.null(stop) ) {
    cat(sprintf('start/stop invalid: %s/%s\n', start, stop))
    return(df)
  }
  start <- as.POSIXct(start, tz='UTC', origin="1989-12-31")
  stop <- as.POSIXct(stop, tz='UTC', origin="1989-12-31")
  if ( start > stop ) {
    cat(sprintf('start > stop %s/%s\n', start, stop))
    return(df)
  }
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
  df %>% filter(timestamp >= start, timestamp <= stop)
}

format_pace <- function(pace){
  pre <- floor(pace)
  sprintf('%d:%02d', pre, round(60 * (pace - pre)))
}

calc_hrdp <- function(data, lmfit, mfit){

  #save(data, lmfit, mfit, file='debug.Rdata')
  # some custom checks
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

  # let's go
  hrdp <- list()

  # get values from pre-fitted linear and non-linear models
  intercept <- lmfit$coefficients[[1]]
  slope <- lmfit$coefficients[[2]]
  print(head(data))
  data$speed <- lmfit$x
  data$hrfit <- mfit$fitted
  data <- data %>% arrange(speed) %>% select(c('hrfit', 'speed')) %>% distinct()
  print(lmfit$coefficients)

  # get distance from non-linear fitted values to linear fit
  data$dist <- ( data$hrfit - data$speed * slope - intercept) / sqrt(slope**2 + 1)
  data$distfo <- (data$dist - lag(data$dist)) / (data$speed - lag(data$speed))
  # zero points in first order estimate
  data <- data %>% drop_na() %>% mutate(zeros = as.logical((distfo >0) - (lag(distfo)>0)) )

  # collect results
  print(data %>% filter(dist > 0, zeros==T))
  hrdp$speed <- data %>% filter(dist > 0, zeros==T) %>% arrange('speed') %>% head(1) %>% pull('speed')
  if( length(hrdp$speed) == 0 ){
    stop("no local maximum")
  }
  hrdp$pace <- 60/3.6/hrdp$speed
  hrdp$hr <- data %>% filter(speed==hrdp$speed) %>% select('hrfit') %>% tail(1) %>% pull()
  hrdp
}

svm_prepare_data <- function(data, slopes, hrs){
  base_features <- c('hr', 'speed', 'slope', 'hrchange')
  svmdata <- data %>% select(all_of(base_features))
  max_buckets <- floor(log2(nrow(svmdata)/2))
  for(i in 0:min(slopes, max_buckets)){
    k <- 2**(max_buckets - i)
    ms <- rollmean(svmdata$slope, k,  align='right')
    if(k>1){
      ms <- c( cumsum(svmdata$slope[1:(k-1)])/(1:(k-1)), ms)
    }
    svmdata <- cbind(svmdata, ms)
  }
  for(i in 0:min(hrs, max_buckets)){
    k <- 2**(max_buckets - i)
    ms <- rollmean(svmdata$hrchange, k,  align='right')
    if(k>1){
      ms <- c( cumsum(svmdata$hrchange[1:(k-1)])/(1:(k-1)), ms)
    }
    svmdata <- cbind(svmdata, ms)
  }
  names(svmdata) <- c(base_features, paste0('slope', seq(0, slopes)),
                                     paste0('hrchange', seq(0, hrs)))
  svmdata %>% select(-c('hrchange', 'slope')) %>% drop_na()
}

svm_predicted <- function(model, data, slopes, hrs){
  # generate fitted values for speed based on 0-elevation predictions
  xlen <- nrow(data)
  xhat <- data.frame(speed=seq(min(data$speed), max(data$speed), length.out=xlen))
  for(i in 1:(2 + slopes + hrs)){
    xhat <- cbind(xhat, numeric(xlen)) # zeros
  }
  names(xhat) <- c('speed', paste0('slope', seq(0, slopes)), paste0('hrchange', seq(0, hrs)))
  data.frame(x=xhat$speed, y=predict(model, xhat))
}


svm_lin <- function(data, slopes=5, hrs=5, C=NULL){
  data <- svm_prepare_data(data, slopes, hrs)
  if( is.null(C) ){
    C <- 10^seq(-10,2)
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
  } else {
    fitControl <- trainControl(method = "cv", number = 10)
  }

  params <- expand.grid(C=C)
  model <- train(hr ~ . , data=data, method='svmLinear',
              trControl=fitControl, tuneGrid=params)

  res <- svm_predicted(model, data, slopes, hrs)
  model$fitted <- res$y
  model$x <- res$x

  # get intercept/slope to append $coefficients
  x0 <- data.frame(t(numeric(1 + 2 + slopes + hrs)))
  names(x0) <- c('speed', paste0('slope', seq(0, slopes)), paste0('hrchange', seq(0, hrs))) 
  intercept <- predict(model, x0)
  slope <- (res$y[2] - res$y[1]) / (res$x[2] - res$x[1])
  model$coefficients <- c(intercept, slope)
  model$error <- model$results$RMSE
  model$slopes <- slopes
  model$hrs <- hrs
  model
}

svm_radial <- function(data, slopes=5, hrs=5, C=NULL, sigma=NULL){
  data <- svm_prepare_data(data, slopes, hrs)
  # found params
  # https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
  if( is.null(C) || is.null(sigma) ){
    #sigma <- 10^seq(-7,-2, length.out=10)
    #sigma <- 2^seq(-15, 3)
    sigma <- 2^seq(-5, -1)
    #C <- 2^seq(-15, 5)
    C <- 2^seq(-3, 3)
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2,
                               returnData = FALSE, trim = TRUE)
  } else {
    fitControl <- trainControl(method = "cv", number = 10)
  }

  registerDoFuture()
  params <- expand.grid(C=C, sigma=sigma)
  model <- train(hr ~ . , data=data, method='svmRadialSigma',
              trControl=fitControl, tuneGrid=params)

  res <- svm_predicted(model, data, slopes, hrs)
  model$fitted <- res$y
  model$x <- res$x
  model$error <- model$results$RMSE
  model$slopes <- slopes
  model$hrs <- hrs
  model
}