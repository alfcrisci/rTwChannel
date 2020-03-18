#' trim_head
#'
#' @description Remove head  caracters  before user in  retweeted messages
#'
#' @param  x   Character  Text  of tweet
#' @return     Return Text of tweet without head.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_head <- function(x) {
  sub('^(.*)?@', '', x)
}
