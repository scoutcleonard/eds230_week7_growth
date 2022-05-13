#' Forest Growth Model
#' Function to estimate forest growth
#' 
#' @param Time time since start
#' @param C Forest size (kg of carbon)
#' @param parms - as list with 4 values
#' @param r early exponential growth rate
#' @param g linear growth rate once canopy closure has been reached
#' @param K Carrying capacity (kg of carbon)
#' @param thresh Canopy closure threshold (kg of carbon)
#' 
#' @return Derivative with forest size (kg of carbon) with time

dgrowth = function(Time, C, parms) {
  if (C < parms$thresh) {
    # Exponential growth below threshold
    rate = parms$r * C
  } else {
    # Linear growth above threshold
    rate = parms$g * (1 - (C / parms$K))
  }
  
  return(list(rate))
}