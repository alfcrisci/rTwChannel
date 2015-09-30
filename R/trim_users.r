#' trim_users
#'
#' @description Remove users as mentions from  tweet message
#'
#' @param  x   Character  Message of tweet
#' @return     Return the message of tweet without mentions.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  retweet
#'
#'
#'
#' @export

trim_users <- function(x) {
  stringr::str_replace_all(x, '(@[[:alnum:]_]*)', '')
}
