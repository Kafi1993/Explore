#' Divide by Pattern
#' 
#' Selects only those columns of a dataframe, which match the given pattern.
#' Optionally the pattern is an index-value after which columns are selected.
#' 
#'@author Joschka Cremers
#'@param dframe data.frame, typically psychological.
#'@param pattern selects those items which match the specified pattern. May also be column-index-values.
#'@param n.col specify a maximum number of columns, that shall be returned.
#'
#'@usage divideByPattern(n.col = 15)
#'
#'@return Returns a data.frame which contains only the selected columns up to the maximum of n.col columns.
#' 
#'@export
divideByPattern <- function(dframe, pattern, n.col = 15){
  if(is.character(pattern) == TRUE){
    dframe <- dframe[,grep(pattern, names(dframe), value=T)]
  }
  else if(is.numeric(pattern) == TRUE){
    dframe <- dframe[,pattern]
  }
  else{
    stop('"pattern" must only contain character values or vectorial index-values.')
  }
  
  dframe <- extractNumeric(dframe)
  if(NCOL(dframe)>n.col){
    dframe <- dframe[,1:n.col]
  }
  return(dframe)
}