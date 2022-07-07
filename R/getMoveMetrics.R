## Useful Functions for Calculating Movement Metrics ##
### functions are adapted from R code from L. Borger (UBC Animal Movement Workshop, 2018)
### R. Ritson, 2022

# Calculate first record of each individual
assign_firstRec <- function(df,id,timestamp){
  df <- df[order(df[id],df[timestamp]),]
  foo <- which(df[[id]][1:(nrow(df)-1)] != df[[id]][2:(nrow(df))])
  df$firstRec <- rep(0,nrow(df))
  df$firstRec[foo+1] <- 1
  df$firstRec[1] <- 1
  return(df)
}

# Calculate step length between each location (projected coordinates only)
assign_steplength <- function(df,id,timestamp,coord_x,coord_y){
  if(c("firstRec") %in% colnames(df) == F){
    df <- df[order(df[id],df[timestamp]),]
    foo <- which(df[[id]][1:(nrow(df)-1)] != df[[id]][2:(nrow(df))])
    df$firstRec <- rep(0,nrow(df))
    df$firstRec[foo+1] <- 1
    df$firstRec[1] <- 1
  }
  foo <- sqrt((df[[coord_x]][1:(nrow(df)-1)] - df[[coord_x]][2:(nrow(df))])^2 + 
              (df[[coord_y]][1:(nrow(df)-1)] - df[[coord_y]][2:(nrow(df))])^2)
  df$sl <- c(NA,foo)
  df$sl <- ifelse(df$firstRec == 1,NA,df$sl)
  return(df)
}

# Calculate time difference between each location (timestamp column must be POSIX)
assign_timediff <- function(df,id,timestamp,units){
  if(c("firstRec") %in% colnames(df)==F){
    df <- df[order(df[id],df[timestamp]),]
    foo <- which(df[[id]][1:(nrow(df)-1)] != df[[id]][2:(nrow(df))])
    df$firstRec <- rep(0,nrow(df))
    df$firstRec[foo+1] <- 1
    df$firstRec[1] <- 1
  }
    foo <- difftime(df[[timestamp]][2:nrow(df)],df[[timestamp]][1:(nrow(df)-1)],units=units)
    foo <- c(NA,foo)
    foo <- ifelse(df$firstRec==1,NA,foo)
    df$dt <- foo
    return(df)
}

# Calculate hourly movement rate for each location 
assign_hourlyrate <- function(df,id="Animal_Season",timestamp = "DateTime",coord_x="IDTM_X",coord_y="IDTM_Y",units="hours"){
  if(any((c("firstRec","sl","dt") %in% colnames(df))==F)){
    df <- df[order(df[id],df[timestamp]),]
    foo <- which(df[[id]][1:(nrow(df)-1)] != df[[id]][2:(nrow(df))])
    df$firstRec <- rep(0,nrow(df))
    df$firstRec[foo+1] <- 1
    df$firstRec[1] <- 1
    foo <- sqrt((df[[coord_x]][1:(nrow(df)-1)] - df[[coord_x]][2:(nrow(df))])^2 + 
                  (df[[coord_y]][1:(nrow(df)-1)] - df[[coord_y]][2:(nrow(df))])^2)
    df$sl <- c(NA,foo)
    df$sl <- ifelse(df$firstRec == 1,NA,df$sl)
    foo <- difftime(df[[timestamp]][2:nrow(df)],df[[timestamp]][1:(nrow(df)-1)],units=units)
    foo <- c(NA,foo)
    foo <- ifelse(df$firstRec==1,NA,foo)
    df$dt <- foo
  }
  df$Rt_hrly <- ifelse(df$firstRec==1, NA, df$sl / df$dt) 
  return(df)
}
