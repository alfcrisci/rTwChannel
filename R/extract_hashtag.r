#' extract_hashtag
#'
#' @description Extract hashtag mentioned in a message of tweet.
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

extract_hashtag=function(x) {
  res=strip(x, digit.remove = FALSE,char.keep = c("_","#"))
  res=qdapRegex::rm_hash(res,extract=T)
  return(res)
}
