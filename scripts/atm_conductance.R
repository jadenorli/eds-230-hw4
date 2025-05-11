#'
#'




#write a function to calculate atmospheric conductance based on windspeed and vegetation height
atmospheric_condunctance <- function(v, h, zm_add = 2.0, ko = 0.1, kd = 0.7) {
  
  
  zd <- kd * h
  zo <- ko * h
  
  zm <- h + zm_add
  
  zd <- ifelse(zd < 0, 0, zd)
  Ca <- ifelse(zo > 0,
               v / (6.25 * log((zm - zd) / zo)**2), 0)
  
  #convert the conductance from m to mm 
  Ca <- Ca * 1000
  return(Ca)
}