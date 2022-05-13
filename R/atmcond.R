#' Atmospheric conductance calculation
#'
#' @param h vegetation height (cm)
#' @param Vm windspeed (cm/s)
#' @param Zm height at which windspeed is measured (cm)
#' @param Kd ??
#' @param K0 ??
#'
#' @return atmospheric conductance 
atmcond = function(h, Vm, Zm = h + 200, Kd = 0.7, K0 = 0.1) {
  Zd = Kd * h
  Z0 = K0 * h
  Cat = Vm / (6.25 * (log((Zm - Zd) / Z0) ^ 2))
  
  return(list(val = Cat))
}
