
#' @export
extractNumeric <- function(dframe){
  dframe[sapply(dframe, is.numeric)] 
}


#' @export
naCount <- function(dframe){
  sapply(dframe, function(column){sum(is.na(column))})
}


#' @export
meanAll <- function(dframe){
  round(sapply(dframe, mean, na.rm = T), digits = 2)
}


#' @export
medianAll <- function(dframe){
  round(sapply(dframe, median, na.rm = T), digits = 2)
} 


#' @export
sdAll <- function(dframe){
  round(sapply(dframe, sd, na.rm = T), digits = 2)
}

#' @export
minAll <- function(dframe){
  round(sapply(dframe, min, na.rm = T), digits = 2)
}

#' @export
maxAll <- function(dframe){
  round(sapply(dframe, max, na.rm = T), digits = 2)
}

#' @export
skewAll <- function(dframe){
  round(sapply(dframe, skew, na.rm = T), digits = 2)
}

#' @export
kurtosisAll <- function(dframe){
  round(sapply(dframe, kurtosi, na.rm = T), digits = 2)
}

