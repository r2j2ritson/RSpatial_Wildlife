#' @title Convert aspects as function of degrees from an anchor direction
#' @description Transform an aspect direction as a measure from an anchor direction
#'
#' @param aspect The aspect number (in degrees) that you want to convert. 
#' @param anchor A number (in degrees) in which you want the new aspect to reference.
#' 
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#'
#' @examples
#' ## Not Run:
#' # Calculate Aspects with different anchors
#' df <- data.frame(Aspect = seq(0,360,1)) # Aspect
#' df$Degrees_from_South <- convert_aspect(df$Aspect, 180) # Degrees from South (0 as due South, 180 degrees)
#' df$Degrees_from_SouthWest <- convert_aspect(df$Aspect, 225) # Degrees from Southwest (0 as due SouthWest, 225 degrees)
#' head(df)
#'
#' # Plot Degree Aspects
#' library(ggplot2)
#' data <- data.frame(dir = df$Aspect, mag = df$Aspect)
#' normal <- ggplot(data, aes(x=dir, y=mag)) + 
#'   geom_bar(stat='identity') + 
#'   coord_polar() + 
#'   ggtitle("Normal Aspect") +
#'   scale_x_continuous(breaks = c(0,45,90,135,180,225,270,315))
#' normal
#'
#' data <- data.frame(dir = df$Aspect, mag = df$Degrees_from_South)
#' dfs <- ggplot(data, aes(x=dir, y=mag)) + 
#'   geom_bar(stat='identity') + 
#'   coord_polar() + 
#'   ggtitle("Degrees from South") +
#'   scale_x_continuous(breaks = c(0,45,90,135,180,225,270,315))
#' dfs
#'
#' data <- data.frame(dir = df$Aspect, mag = df$Degrees_from_SouthWest)
#' dfsw <- ggplot(data, aes(x=dir, y=mag)) + 
#'   geom_bar(stat='identity') + 
#'   coord_polar() + 
#'   ggtitle("Degrees from South-West") +
#'   scale_x_continuous(breaks = c(0,45,90,135,180,225,270,315))
#' dfsw
#'
#' df$Degrees_from_North <- convert_aspect(df$Aspect, 360) # Degrees from North
#' data <- data.frame(dir = df$Aspect, mag = df$Degrees_from_North)
#' dfn <-ggplot(data, aes(x=dir, y=mag)) + 
#'   geom_bar(stat='identity') + 
#'   coord_polar() + 
#'   ggtitle("Degrees from North") +
#'   scale_x_continuous(breaks = c(0,45,90,135,180,225,270,315))
#' dfn
#'
#' convert_aspect(aspect = 0, anchor = 180) #Due North is 180 degrees from due South
#' convert_aspect(aspect = 0, anchor = 225) #Due North is 135 degrees from due SouthWest
#'
#' ## End Not Run
#'
#' @export convert_aspect
convert_aspect <- function(aspect, anchor){
  #Calculate Aspect
  asp <- abs(180-abs(aspect + (180 - anchor)))
  return(asp)
}
