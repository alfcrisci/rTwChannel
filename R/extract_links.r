#' extract_links
#'
#' @description Extract url mentioned in a message of tweet.
#'
#' @param  x   Character  Messages of channel's tweets.
#' @return     Return the list of mentioned users
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mentions
#'
#' @references  qdap R packages
#
#'
#' @export

extract_links=function(x) {
   res=qdapRegex::rm_url(res,extract=T)
  return(res)
}
