#' trim_at
#'
#' @description Remove at caracter from users mentioned in  tweet message
#'
#' @param  x   Character  Message of tweet
#' @return     Message of tweet without @@ simbol.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export


trim_at <- function(x) {
  sub('@', '', x)
}
