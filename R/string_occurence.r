#' string_occurence
#'
#' @description Extract on a character vector the  frequency of matches relative to textual pattern.
#'
#' @param  x  character  vector of date correponding to Tweet within unique ID.
#' @param  pattern  Pattern to be search.
#' @return  Return a vector with pattern frequency
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  match
#'
#'
#'
#' @export
#'
#'

string_occurence=function(x,pattern) {sum(sapply(regmatches(x, gregexpr(pattern, x,ignore.case=TRUE)), length))}
