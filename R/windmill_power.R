#' Compute windmill power
#' @param p is air density
#' @param A is rotar swept area m^2
#' @param Cp is power coefficient
#' @param V is wind velocity m/s
#' @param Ng is generator efficiency
#' @param Nb is gearbox efficiency

windmill_power = function(p = 1.2, A, Cp = 0.35, V, Ng=.3, Nb = 0.5, price = .12, hidePrice, hidePower) {
  #calculate 
  wind_power = (.5*p*A*Cp*(V^3)*Ng*Nb)/1000
  price = (wind_power * price)
  
  if (A<.01)
    return("Check swept area (A) input")
  
  if (V<0)
    return("Check wind speed (V) input")
  
  output = list(windPower = wind_power, revenue = price)
  if(hidePrice) output <- output$windPower#hide A
  if(hidePower) output <- output$revenue
  return(output)
}
