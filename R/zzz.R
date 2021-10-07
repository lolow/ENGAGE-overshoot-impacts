# Useful functions

scen_ssp <- function(scenario) {
  if (str_detect(scenario,'SSP')) {
    return(tolower(str_extract(scenario,'SSP\\d')))
  }
  return('ssp2')
}

npvalue <- function(year, value, dr, years) {
  return(sum(approx(year,value,years,rule = 2)$y *  (1 / (1 + dr)^(years - year[1]))))
}

long_table <- function(.x, ids, varname = "year", valname = "value") {
  return(melt(.x, id.vars = ids, variable.factor = F, variable.name = varname, value.name = valname ))
}

ma <- function (x, order, centre = TRUE) 
{
  if (abs(order - round(order)) > 1e-08) {
    stop("order must be an integer")
  }
  if (order%%2 == 0 && centre) {
    w <- c(0.5, rep(1, order - 1), 0.5)/order
  }
  else {
    w <- rep(1, order)/order
  }
  return(filter(x, w))
}