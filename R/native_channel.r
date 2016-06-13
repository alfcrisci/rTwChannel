#' native_channel
#'
#' @description extract a channel object only with original tweet 
#'
#' @param  channel_obj  data.frame  channel object
#' @param  naming  character  stream data.frame type
#' @return channel_obj data.frame Return a data.frame 
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  channel
#'
#'
#'
#' @export
#'
#'

native_channel = function(channel_obj,naming="DISIT") {

    if ( naming == "TAGS") {
    
    channel_obj$created <- lubridate::dmy_hms(channel_obj$time)
    channel_obj=channel_obj[which(!is.na(channel_obj$created)),]
    channel_obj$data <- as.Date(channel_obj$created)
    channel_obj$screeName=channel_obj$from_user
    channel_obj$id=as.numeric(channel_obj$id_str)
    channel_obj$lang=channel_obj$user_lang
    channel_obj$from_user<-NULL
    channel_obj$user_lang<-NULL
    channel_obj$message<-NULL
    channel_obj$created_at<-NULL
    channel_obj$retweetCount<-rep(0,nrow(channel_obj))
    channel_obj$entities_str<-NULL
    channel_obj$retweetCount<-rep(0,nrow(channel_obj))
    channel_obj$favoriteCount<-rep(0,nrow(channel_obj))
    channel_obj$ls_hash_full<-rep(NA,nrow(channel_obj))
    channel_obj$ls_links=rep(NA,nrow(channel_obj))
    channel_obj$time<-NULL
    channel_obj=channel_obj[rev(1:nrow(channel_obj)),]
  }
  
  
  if ( naming == "DISIT") {
    
    channel_obj$text=channel_obj$message
    channel_obj$data=as.character(as.Date(channel_obj$publicationTime))
    channel_obj$screeName=channel_obj$twitterUser
    channel_obj$created=channel_obj$publicationTime
    channel_obj$ls_hash_full=channel_obj$hashtagsOnTwitter
    channel_obj$ls_links=channel_obj$links
    channel_obj$id=channel_obj$twitterId
    channel_obj$message<-NULL
    channel_obj$hashtagsOnTwitter<-NULL
    channel_obj$twitterId<-NULL
    channel_obj$links<-NULL
    channel_obj$hour=lubridate::hour(channel_obj$publicationTime)
    channel_obj$month=lubridate::month(channel_obj$publicationTime)
    channel_obj$publicationTime<-NULL
  
  }
  
  
    
  if ( naming == "twitter") {
    
    channel_obj$data=as.Date(channel_obj$created)
    channel_obj$hour=lubridate::hour(channel_obj$created)
    channel_obj$month=lubridate::month(channel_obj$created)
    channel_obj$text=iconv(channel_obj$text,"utf-8")
    channel_obj$isRetweet=as.integer(channel_obj$isRetweet)
    channel_obj$screeName=channel_obj$screenName
    channel_obj$screenName<-NULL
  }

  ls_retweet = unlist(lapply(channel_obj$text,FUN=function(x) is.retweet(x)))
  channel_obj = channel_obj[which(ls_retweet==FALSE),]
  return(channel_obj)
}
