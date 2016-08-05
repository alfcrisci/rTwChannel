#' fastChannelstat
#'
#' @description Function working on channel extracting many informative stats  from twitter timelines  parsed as channel.
#'
#' @param  x  Data.frame Channel time streams with tweets
#' @param  check_duplicates Logical Check if exist duplicated tweets. 
#' @param  stream Character Kind of channel tweets collection. Default is empty. 
#' @return RTW_TW Numeric Number of full tweets ( tweets and retweets).
#' @return TW Numeric Number of original tweets.
#' @return RTW Numeric Number of original retweets.
#' @return ratioRTW_TW Numeric Ratio full ( tweets and retweets) vs original tweets.
#' @return ini_date_full  Date channel initial.
#' @return end_date_full Date channel final.
#' @return activity_days Numeric day of activity channel.
#' @return activity_days_native  Numeric day of activity channel.
#' @return period_extent Numeric Days covering channle period.
#' @return relative_activity Numeric Full activity ratio.
#' @return RTW_TW_daily  Numeric Full number of tweets.
#' @return TW_daily  Numeric Native number of tweets.
#' @return retweetCount  Numeric  Sum of retweets.
#' @return favoriteCount  Numeric Sum of favorites.
#' @return N_full_users  Numeric Number of full users.
#' @return U_full_users  Numeric Number of full unique users.
#' @return N_full_hashtag  Numeric  Number of hashtag used.
#' @return U_full_hashtag  Numeric  Number of unique hashtag used.
#' @return N_full_mentions Numeric Number of mentions.
#' @return U_full_mentions  Numeric Number of unique mentions. 
#' @return N_full_links  Numeric  Number of links.
#' @return U_full_links  Numeric  Number of unique links.
#' @return mostRT_msg Character Most retweeted message  ( tweets or retweets).
#' @return N_native_users  Numeric  Number of native users.
#' @return U_native_users  Numeric  Number of native unique users.
#' @return N_native_hashtag  Numeric  Number of hashtag used.
#' @return U_native_hashtag  Numeric  Number of unique hashtag used.
#' @return N_native_mentions  Numeric Number of mentions. 
#' @return U_native_mentions  Numeric Number of unique mentions. 
#' @return N_native_links  Numeric Number of links.
#' @return U_native_links  Numeric Number of native unique users.
#' @return mostRT_msg_native Character Most retweeted message native.
#' @return M_ch_counts_native Numeric Mean Number of original tweets favorite.
#' @return N_replies Numeric Number of original tweets favorite.
#' @return M_ch_counts_full Numeric mean 
#' @return most_favorited_messages Character Most favorited message.
#' @return most_mentioned Character Most mentioned user.
#' @return most_retweeted Character Most mentioned user.
#' @return most_favorited Character 
#' @return N_favor_full Numeric Number of original tweets favorite.
#' @return Nfavor_native Numeric Number of original tweets favorite.
#' @return N_geo Numeric Number of Geolocated tweets.
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  channel,daily stats
#'
#'
#'
#' @export
#'


fastChannelstat<-function(x,check_duplicates=FALSE,stream="")
  {
   name_one=c("RTW_TW","TW","RTW","ini_date_full","end_date_full","ratioRTW_TW","activity_days","activity_days_native","TW_daily","period_extent","relative_activity","RTW_TW_daily","retweetCount","favoriteCount","N_native_users","U_native_users","N_native_hashtag","U_native_hashtag","N_native_mentions",
             "U_native_mentions","N_native_links","U_native_links","mostRT_msg_native","M_ch_counts_native",    
             "N_full_users","U_full_users","N_full_hashtag","U_full_hashtag","N_full_mentions","U_full_mentions",        
             "N_full_links","U_full_links","mostRT_msg","N_replies","M_ch_counts_full","N_favor_full","Nfavor_native","most_favorited_messages",
             "most_mentioned","most_retweeted","most_favorited","N_geo")
  res_df=data.frame(t(rep(NA,length(name_one))))
  names(res_df)=name_one
  
  if (stream=="DISIT") {
                           x$screenName=x$twitterUser 
                           x$isRetweet=x$retweet
                           x$text=x$message
  }
  
  if (nrow(x)==0) { return(res_df)}
 
  N_geo=NA
 
  if ( length(grep("geo_lat",names(x)))==1) { 
        geo_lat=as.vector(x[c("geo_lat")]) 
        N_geo=length(which(geo_lat>0))
  }
  
   
 
  
  
  
  if (nrow(x)==1) { 
                    res_df$RTW_TW=1;
                    res_df$TW=ifelse(x$isRetweet==1,0,1);
                    res_df$RTW=res_df$RTW_TW-res_df$TW;
                    res_df$ratioRTW_TW=res_df$TW/res_df$RTW_TW;
                    res_df$ini_date_full=x$date
                    res_df$end_date_full=x$date
                    res_df$activity_days=1
                    res_df$activity_days_native=ifelse(x$isRetweet==1,0,1);
                    res_df$period_extent=1;
                    res_df$relative_activity=1;
                    res_df$RTW_TW_daily=1
                    res_df$TW_daily=ifelse(x$isRetweet==1,0,1);
                    res_df$retweetCount=x$retweetCount
                    res_df$favoriteCount=x$favoriteCount
                    res_df$N_full_users=ifelse(x$isRetweet==1,1,0)
                    res_df$U_full_users=res_df$N_full_users
                    res_df$N_full_hashtag=length(strsplit(x$hashtagsOnTwitter, "\\s+"))
                    res_df$U_full_hashtag=res_df$N_full_hashtag
                    res_df$N_full_mentions=length(strsplit(x$mentions, "\\s+"))
                    res_df$U_full_mentions=res_df$N_full_mentions
                    res_df$N_full_links=length(strsplit(x$links, "\\s+"))
                    res_df$U_full_links=res_df$N_full_links
                    res_df$mostRT_msg=x$text
                    res_df$N_native_users=ifelse(x$isRetweet==1,0,1)
                    res_df$U_native_users=ifelse(x$isRetweet==1,0,1)
                    res_df$N_native_hashtag=ifelse(x$isRetweet==1,0,1)
                    res_df$U_native_hashtag=ifelse(x$isRetweet==1,0,1)
                    res_df$N_native_mentions=ifelse(x$isRetweet==1,0,res_df$N_full_mentions)
                    res_df$U_native_mentions=ifelse(x$isRetweet==1,0,res_df$N_full_mentions)
                    res_df$N_native_links=ifelse(x$isRetweet==1,0,res_df$N_full_links)
                    res_df$U_native_links=ifelse(x$isRetweet==1,0,res_df$N_full_links)
                    res_df$mostRT_msg_native=x$text
                    res_df$M_ch_counts_full=mean(as.numeric(nchar(gsub(" ","",x$text))))
                    res_df$M_ch_counts_native=ifelse(x$isRetweet==1,0,mean(as.numeric(nchar(gsub(" ","",x$text)))))
                    res_df$N_replies=length(grep("^@",x$text))
                    res_df$most_favorited_messages=x$text
                    res_df$most_mentioned=x$text
                    res_df$most_retweeted=x$text
                    res_df$most_favorited=x$text
                    res_df$N_favor_full=ifelse(x$favoriteCount>0,1,0);
                    res_df$Nfavor_native=ifelse(x$isRetweet==1,0,res_df$N_favor_full)
                    res_df$N_geo=N_geo
                    return(res_df)
                    
                    }
  
  if (check_duplicates==T) {x=x[which(duplicated(x$twitterId)==F),]}
 
  x_native=x[which(x$isRetweet==0),]
  
  res_df$RTW_TW=nrow(x);
  res_df$TW=nrow(x_native);
  res_df$RTW=res_df$RTW_TW-res_df$TW;
  res_df$ratioRTW_TW=res_df$TW/res_df$RTW_TW;
  rangeday=c(x$date[1],x$date[nrow(x)])
  
  if (length(unique(x$date))>1)
      {rangeday=range(as.Date(x$date))
      };
  
  res_df$ini_date_full=rangeday[1]
  res_df$end_date_full=rangeday[2]
  res_df$activity_days=length(unique(x$date));
  res_df$activity_days_native=0
  res_df$TW_daily=0
  
  if (nrow(x_native) > 0) {
                          res_df$activity_days_native=length(unique(x_native$date));
                          res_df$TW_daily=res_df$TW/res_df$activity_days_native;
                           
                          }
  
  res_df$period_extent=1
  
  if (length(unique(x$date))>1)
      {res_df$period_extent=as.numeric(diff(rangeday))+1;} 
  
  res_df$relative_activity=round(res_df$activity_days/res_df$period_extent,2);
  
  res_df$RTW_TW_daily=res_df$RTW_TW/res_df$activity_days;
  
  res_df$retweetCount=sum(x$retweetCount)
  
  res_df$favoriteCount=sum(x$favoriteCount)

  full_mentions=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x$mentions), sep=" "))), "\\s+")
  full_links=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x$links), sep=" "))), "\\s+")
  full_users=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x$twitterUser), sep=" "))), "\\s+")
  full_hashtag=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x$hashtagsOnTwitter), sep=" "))), "\\s+")
  
  res_df$N_native_users=NA
  res_df$U_native_users=NA
  res_df$N_native_hashtag=NA
  res_df$U_native_hashtag=NA
  res_df$N_native_mentions=NA
  res_df$U_native_mentions=NA
  res_df$N_native_links=NA
  res_df$U_native_links=NA
  res_df$mostRT_msg_native=NA
  res_df$Nfavor_native=NA
  
  if ( nrow(x_native) > 0) {
                          native_mentions=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x_native$mentions), sep=" "))), "\\s+")
                          native_links=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x_native$links), sep=" "))), "\\s+")
                          native_users=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x_native$twitterUser), sep=" "))), "\\s+")
                          native_hashtag=strsplit(gsub(" NA"," ",do.call(paste, c(as.list(x_native$hashtagsOnTwitter), sep=" "))), "\\s+")
 
                          res_df$N_native_users=suppressWarnings(try(length(native_users[[1]])))
                          res_df$U_native_users=suppressWarnings(try(length(unique(native_users[[1]]))))
                          res_df$N_native_hashtag=suppressWarnings(try(length(native_hashtag[[1]])))
                          res_df$U_native_hashtag=suppressWarnings(try(length(unique(full_hashtag[[1]]))))
                          res_df$N_native_mentions=suppressWarnings(try(length(native_mentions[[1]])))
                          res_df$U_native_mentions=suppressWarnings(try(length(unique(native_mentions[[1]]))))
                          res_df$N_native_links=suppressWarnings(try(length(native_links[[1]])))
                          res_df$U_native_links=suppressWarnings(try(length(unique(native_links[[1]]))))
                          res_df$mostRT_msg_native=paste(unique(paste(x$text[which(x_native$retweetCount == max(x$retweetCount))])),collapse = " ")
                          res_df$M_ch_counts_native=mean(as.numeric(nchar(gsub(" ","",x_native$text))))
                          res_df$Nfavor_native=length(which(x_native$favoriteCount>0));
                          
                          
  }
  
  res_df$N_full_users=length(full_users[[1]])
  res_df$U_full_users=length(unique(full_users[[1]]))
  
  res_df$N_full_hashtag=length(full_hashtag[[1]])
  res_df$U_full_hashtag=length(unique(full_hashtag[[1]]))
  
  res_df$N_full_mentions=length(full_mentions[[1]])
  res_df$U_full_mentions=length(unique(full_mentions[[1]]))
  
  res_df$N_full_links=length(full_links[[1]])
  res_df$U_full_links=length(unique(full_links[[1]]))
  
  res_df$mostRT_msg=paste(unique(paste(x$text[which(x$retweetCount == max(x$retweetCount))])),collapse = " ")
  
  
  res_df$N_replies=length(grep("^@",x$text));
  
  res_df$M_ch_counts_full=mean(as.numeric(nchar(gsub(" ","",x$text))));
  
  res_df$most_favorited_messages=paste(unique(paste(x$text[which(x$favoriteCount == max(x$favoriteCount))])),collapse = " ")
  res_df$N_favor_full=length(which(x$favoriteCount>0));
  
  ind_mentioned=as.data.frame.array(table(full_mentions))
  ind_retweeted=try(x[c("screenName","retweetCount")])
  ind_favorited=x[c("screenName","favoriteCount")]
  ind_retweeted_sum=try(aggregate(ind_retweeted$retweetCount,by=list(ind_retweeted$screenName),FUN = sum))
  ind_favorited_sum=aggregate(ind_favorited$favoriteCount,by=list(ind_favorited$screenName),FUN = sum)
 
  res_df$most_mentioned=paste(names(ind_mentioned[which(ind_mentioned == max(ind_mentioned)),]),collapse = " ")
  res_df$most_retweeted=try(paste(ind_retweeted_sum$Group.1[which(ind_retweeted_sum$x== max(ind_retweeted_sum$x))],collapse = " "))
  res_df$most_favorited=paste(ind_favorited_sum$Group.1[which(ind_favorited_sum$x== max(ind_favorited_sum$x))],collapse = " ")
  
   
  return(res_df)

}



