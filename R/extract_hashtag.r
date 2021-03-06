#' extract_hashtag
#'
#' @description Extract hashtag mentioned in a message of tweet.
#'
#' @param  x   Character  Messages of channel's tweets.
#' @return     Return the list of mentioned users
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  mentions
#'
#' @references  qdap R packages
#
#' @importFrom qdapRegex rm_hash
#' @export

extract_hashtag=function(x) {
  res=strip(x, digit.remove = FALSE,char.keep = c("_","#"))
  res=rm_hash(res,extract=T)
  return(res)
}
