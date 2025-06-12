#' @title Algorithmically identify "dead" critters
#' @description
#' Identify indices in an animal movement track which meet a specified minimum 
#' displacement (step length) threshold value for a specified minimum duration (time)
#' 
#' @param sl A vector containing the numeric step lengths to be evaluated.
#' @param dt A vector containing the time stamps in POSIX format.
#' @param min_sl A number specifiying the minimum step length (displacement) to be identified.
#' @param min_dur A number specifying the minimum duration of consecutive steps meeting the the 'min_sl' critera.
#' @param time_unit Text specifying the measurement unit of 'min_dur'. Default is "hours".  
#' 
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#' 
#' @examples
#' # Create example data
#' sl <- c(rep(5,3),4,3,rep(6,3),2,1,3,2,1,8,4,7,rep(0,8),2,1,3)
#' dt <- as.POSIXct("2024-01-01") + cumsum(sample(1:86400, length(sl), replace = TRUE))
#' df <- data.frame(sl = sl,dt=dt)
#' head(df)
#' 
#' # Run
#' ## "dead" defined as less than 3 meter step length consecutively for at least 8 hours
#' find_dead_critters(sl = df$sl, dt = df$dt, min_sl = 3, min_dur= 8, time_unit="hours")
#' ##  start_ind end_ind duration
#' ## 1        12      13 20.86917
#' ## 2        17      26 89.95500
#' 
find_dead_critters <- function(sl,dt,min_sl,min_dur,time_unit="hours"){
  # Set condition
  condition <- sl < min_sl
  min_dur <- as.difftime(min_dur, units = time_unit)
  # run-length encoding
  r <- rle(condition)
  tr <- which(r$values)
  starts <- cumsum(c(1,head(r$lengths, -1)))
  # get indices of runs
  rind <- mapply(function(start,len) c(start,start+len-1),
                 starts[tr],r$lengths[tr],SIMPLIFY = F)
  # filter diff time
  timdiff <- Filter(function(idx) difftime(dt[idx[2]],dt[idx[1]],units = 'sec') >= min_dur, rind)
  
  res_df <- data.frame(start_ind = sapply(timdiff, `[`, 1),
                       end_ind = sapply(timdiff, `[`, 2),
                       duration = sapply(timdiff, function(i) as.numeric(difftime(dt[i[2]],dt[i[1]], units=units(min_dur)))))
  return(res_df)
}