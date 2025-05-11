#' Compute Atmospheric Conducatance
#'
#' @param v windspeed (m/s)
#' @param h vegetation height (m)
#' @param zm_add height at which windspeed was measured - usually 200cm above vegetation (m)
#' @param kd displacement scalar - typically 0.7
#' @param ko roughness scalar - typically 0.1 
#' @author Jaden Orli
#' @return atmospheric conductance (Ca) in mm/sec


#write a function to calculate atmospheric conductance based on windspeed and vegetation height
atmospheric_condunctance <- function(v, h, zm_add = 2.0, ko, kd) {
  
  
  zd <- kd * h
  zo <- ko * h
  
  zm <- h + zm_add
  
  zd <- ifelse(zd < 0, 0, zd)
  Ca <- ifelse(zo > 0,
               v / (6.25 * log((zm - zd) / zo)**2), 0)
  
  #convert the conductance from m to mm 
  Ca <- Ca * 1000
  
  #return the conductance value
  return(Ca)
}