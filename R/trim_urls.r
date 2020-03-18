#' trim_urls
#'
#' @description Remove links in a tweet message.
#'
#' @param  x   Character  Message of tweet
#' @return    Return the message of tweet without links.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_urls <- function(x) {

  stringr::str_replace_all(x, 'http[^[:blank:]]+', '')
}
