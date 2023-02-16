
library(tidyverse)

# task 1 

pretty_pvalue <- function(pval) {
  if(pval < 0.001) {
    pretty_pval <- "< 0.001"
  } else {
    pretty_pval <- round(pval, 3)
  }
  return(pretty_pval)
}


pretty_pvalue(0.138921)

# task 2 

pretty_pvalue <- function(pval, threshold, digits ) { 
  if(pval < threshold) {
    pretty_pval <- paste("<", threshold)
  } else {
    pretty_pval <- round(pval, digits ) 
  }
  return(pretty_pval)
}


pretty_pvalue(0.000138921, 0.07)

pretty_pvalue(.1478296578947, 0.07, digits = 1)

pretty_pvalue(.1478296578947, 0.07, digits = 2)

pretty_pvalue(.1478296578947, 0.07)


# task 3 

pretty_pvalue <- function(pval, threshold = 0.001, digits = 2) { # set roudning to 2 by defauls
  if(pval < threshold) {
    pretty_pval <- paste("<", threshold)
  } else {
    pretty_pval <- round(pval, digits ) 
  }
  return(pretty_pval)
}

pretty_pvalue(0.000138921, 0.07)

pretty_pvalue(0.000138921)

pretty_pvalue(.1478296578947, 0.07, digits = 1)

pretty_pvalue(.1478296578947, 0.07, digits = 5)

pretty_pvalue(.1478296578947, 0.07)

# task 4 

pretty_pvalue <- function(pval, threshold = 0.001, digits = 2) { # set roudning to 2 by defauls
  
  if(!is.numeric(pval)) {
    stop("p-value is clearly not numeric duuude")
  }
  
  if(pval < threshold) {
    pretty_pval <- paste("<", threshold)
  } else {
    pretty_pval <- round(pval, digits ) 
  }
  return(pretty_pval)
}


pretty_pvalue("This is baaaaad ")

# optional 

pretty_pvalue <- function(pval, threshold = 0.001, digits = 2) { # set roudning to 2 by defauls
  
  if(!is.numeric(pval) | !is.numeric(threshold)) {
    stop("Either p-value or the threshold is clearly not numeric duuude")
  }
  
  if(is.nan(pval) | is.infinite(pval)){
    stop("pval is not a number, Inf or -Inf occured")
  }
  
  if(pval < 0 | pval > 1 ) {
    stop("p-value is outside of range ")
  }
  
  if(is.na(pval)) {
    stop("p-value is NA (it's missing...) ")
  }
  
  if(pval < threshold) {
    pretty_pval <- paste("<", threshold)
  } else {
    pretty_pval <- round(pval, digits ) 
  }

  if(threshold > 0.1){
    warning("threshold is larger than typical for determining statistical significance.")
  }
  
  return(pretty_pval)
  
}


pretty_pvalue("This is baaaaad ")

pretty_pvalue(pval = 1.1)

pretty_pvalue(pval = )

pretty_pvalue(pval = NA)

pretty_pvalue(pval = 100/0)

pretty_pvalue(pval = .25738954679458, threshold = 0.15, digits = 4)


###################################################################

library(testthat)

f <- function(x) {
  if (x < 0) {
    warning("*x* is already negative")
    return(x)
  }
  -x
}

f(-1)

expect_warning(
  f(-1), "test passed"
  )

expect_warning(f(-1), "already")
