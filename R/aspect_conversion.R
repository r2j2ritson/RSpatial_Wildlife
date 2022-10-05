#' @title Convert aspects as function of degrees from an anchor direction
#' @description Transform an aspect direction as a measure from an anchor direction
#'
#' @param aspect The aspect number (in degrees) that you want to convert. 
#' @param anchor A number (in degrees) in which you want the new aspect to reference
#' 
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#'
#' @examples
#' ## Not Run:
#' df <- data.frame(Aspect = seq(0,360,1)) # Aspect
#' df$Degrees_from_South <- abs(df$Aspect - 180) # Degrees from South (0 is due South, 180 degrees)
#' df$Degrees_from_SouthWest <- abs(180-abs(df$Aspect - 45)) # Degrees from Southwest (0 is due southwest, 225 degrees)
#' df
#'
#' dfs <- convert_aspect(aspect = 0, anchor = 180) #Due North is 180 degrees from due South
#' dfsw <- convert_aspect(aspect = 0, anchor = 225) #Due North is 135 degrees from due SouthWest
#'
#' ## End Not Run
#'
#' @export convert_aspect
convert_aspect <- function(aspect, anchor){
  #Calculate Aspect
  asp <- abs(180-abs(aspect + (180 - anchor)))
  return(asp)
}
