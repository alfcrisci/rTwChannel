#' trim_hashtags
#'
#' @description Remove hashtag in a tweet message.
#'
#' @param  x   Character  Message of tweet
#' @return     Message of tweets without hashtag
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_hashtags <- function(x) {
  stringr::str_replace_all(x, '(#[[:alnum:]_]*)', '')
}
