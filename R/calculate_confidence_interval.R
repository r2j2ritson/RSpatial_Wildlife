#Change ‘ci’ parameter to ‘0.90’ for 90% Confidence Interval
calculate_confidence_interval <- function(x,ci = .95) {
  df <- data.frame(UCL = mean(x,na.rm=T) + qnorm(1- ((1-ci)/2))*sd(x,na.rm=T) / sqrt(length(na.omit(x))),
                   Mean = mean(x,na.rm=T),
                   LCL = mean(x,na.rm=T) - qnorm(1- ((1-ci)/2))*sd(x,na.rm=T) / sqrt(length(na.omit(x))))
  return(df)
}
