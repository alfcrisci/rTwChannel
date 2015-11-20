#' retweeted_users
#'
#' @description Extract any user from native retweets from a tweet message
#'
#' @param  x   Character  Messages of channel's tweets
#' @return    A vector of retweted users
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  user, retweet
#'
#' @references  qdapRegex R packages
#'
#' @export

retweeted_users=function(x) {
  pat<-"?RT @([:alnum:]*[_]*[:alnum:]*):"
  res = unlist(qdapRegex::rm_default(x, pattern=pat,extract=T))
  res=ifelse(length(res)>1,res[length(res)],res[1])
  res = gsub(":","",gsub("^RT @","",res))
  return(res)
}
