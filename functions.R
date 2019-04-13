library(reshape2)

#' @title future_value
#' @description computes the future value of an investment
#' @param amount initial invested amount, numerical
#' @param rate annual rate of return, numerical
#' @param years, number of years, numerical
#' @return value, the future value 
#' 
future_value <- function(amount, rate, years){
  value <- amount * ((1 + rate) ** years)
  return(value)
}



#' @title annuity
#' @description computes the future value of annuity
#' @param contrib contributed amount, numerical
#' @param rate annual rate of return, numerical
#' @param years number of years, numerical
#' @return value, the future value of annuity
annuity <- function(contrib, rate, years){
  value <- contrib * ((((1+rate)**years)-1)/rate)
  return(value)
}


#' @title growing_annuity
#' @description computes the future value of growing annuity
#' @param contrib contributed amount, numerical
#' @param rate annual rate of return, numerical
#' @param years number of years, numerical
#' @param growth the growth rate, numerical
#' @return value, the future value of growing annuity

growing_annuity <- function(contrib, rate, growth, years){
  value <- contrib * ((((1+rate) **years) - ((1+growth) ** years))/(rate-growth))
  return(value)
}


#' @title modalitiesFrame
#' @description creates a dataframe with all the values we want
#' @param contrib contributed amount, numerical
#' @param rate annual rate of return, numerical
#' @param years number of years, numerical
#' @param growth the growth rate, numerical
#' @return modalities, the dataframe with our 3 different modalities with given parameters
modalitiesFrame <- function(initial, returnRate, annualContribution, growthRate, years){

  year <- seq(from = 0, to = years)
  no_contrib <- rep(NA, years)
  fixed_contrib <- rep(NA, years)
  growing_contrib <- rep(NA,years)
  
  no_contrib[1] <- initial
  fixed_contrib[1] <- initial
  growing_contrib[1] <- initial
  
  
  for(i in 2:length(year)){
    no_contrib[i] <- future_value(initial, rate = returnRate, years = i-1)
    fixed_contrib[i] <- future_value(initial, rate = returnRate, years = i-1) + annuity(contrib = annualContribution, rate = returnRate, years = i-1)
    growing_contrib[i] <- future_value(initial, rate = returnRate, years = i-1)  + growing_annuity(contrib = annualContribution, rate = returnRate, growth = growthRate, years = i-1)
  }
  
  modalities <- data.frame(year = year, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
  return(modalities)
  
}

