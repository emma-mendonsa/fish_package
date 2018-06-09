#' Fish: Specific Growth
#'
#' This function estimates specific fish growth as a function of temperature, using an equation from Bjoornsson et al., 2007. The function requires parameters a, b, c, d, as described in Bjoornsson et al., 2007, Table 2, and developed from geometric mean weights of cod
#' @param a Parameter a, Default is -0.4970
#' @param b Parameter b, Default is 0.1656
#' @param c Parameter c, Default is 0.08588
#' @param d Parameter d, Default is -0.004266
#' @param temp_range Water temperature (Celcius), Default is 22
#' @author Emma Siegfried
#' @return Specific Fish Growth
#' @references Björnsson, B., Steinarsson, A. & Árnason, T. Growth model for Atlantic cod (Gadus morhua): Effects of temperature and body weight on growth rate. Aquaculture 271, 216–226 (2007).


fish_spc_growth = function(a = -0.4970, b = 0.1656, c = 0.08588, d = -0.004266, temp_range = 22){

  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  d = as.numeric(d)

  Temp = as.numeric(temp_range)

  Growth = a + b*Temp + c*Temp^2 + d*Temp^3

  table_growth = cbind.data.frame(Temp, Growth)
  table_growth = table_growth %>%
    arrange(-desc(temp_range))

  #return(list("The estimated fish growth rate over the given temperature:" = table_growth))
  return(table_growth)
}
