#' extract_links
#'
#' @description Extract url mentioned in a message of tweet.
#'
#' @param  x   Character  Messages of channel's tweets.
#' @return     Return the list of mentioned users
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  links
#'
#' @references  qdap R packages
#
#' @importFrom qdapRegex rm_url
#' @export

extract_links=function(x) {
   res=rm_url(x,extract=T)
   return(res)
}
