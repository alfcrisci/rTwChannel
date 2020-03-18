#' string_occurence_vector
#'
#' @description Extract on a character vector the occurence of textual pattern
#'
#' @param  x  character  vector.
#' @param  pattern  Pattern to be searched.
#' @return Return a vector with pattern frequency
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  match
#'
#'
#'
#' @export
#'
#'

string_occurence_vector=function(x,pattern) {sapply(regmatches(x, gregexpr(pattern, x,ignore.case=TRUE)), length)}
