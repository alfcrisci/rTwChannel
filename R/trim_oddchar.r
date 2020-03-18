#' trim_oddchar
#'
#' @description Remove unicode characters troughout UTF-8 conversion
#'
#' @param  x   Character  Message of tweet
#' @return     Return message of tweet converted in UTF-8.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_oddchar <- function(x) {
  # remove odd charactors
  iconv(x, to = 'UTF-8')
}
