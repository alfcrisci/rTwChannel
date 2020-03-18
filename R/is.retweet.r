#' is.retweet
#'
#' @description Check if a tweet message have retweet characters
#'
#' @param  x   Character  Message of tweet
#' @return     Logical value for each message : TRUE/FALSE.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

is.retweet=function(x) {res=grepl("^(.*)(:*)RT",x);return(ifelse(res==TRUE,1,0))}
