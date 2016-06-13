#' channel_analytic
#'
#' @description Extract many informative stats and object  from a set of tweet messages parsed as channel
#'
#' @param  channel_obj  Data.frame Dataset of tweets
#' @param  use_channel_dates Logical Use temporal indication of channel
#' @param  start_date   Character Date of analisys starting.
#' @param  end_date   Character Date of  analisys ending.
#' @param  Ntop Integer indicate the maximum number for top statistics
#' @param  temporal_check Logical indicate if exist time consistency between dates and data
#' @param  Nmin Integer indicate the minimal data numerosity
#' @param  naming Character Indicate which naming framework is adopted.
#' @param  only_original_tweet Logical Taking into account only original. Default all tweets are considered.
#' @param  lowercase logical Consider  all text as lower case. Default is TRUE.
#' @param  stopword Character stopword set to be use to calculate word frequency matrix. Default italian stopwords of R tm package.
#' @param  corpus_hashtag logical Corpus not taking into account the hashtag.
#' @param  account_tw User account if naming parameter is an "account_statistics"
#' @return Return a R list object  for channel analisys
#' @return **channel_stat** : channel summaries of following parameters.
#' @return *        *N_tweets* : Number of tweet within unique ID
#' @return *        N_retweets  (channel_stat):Number of retweet
#' @return *        N_native_tweets (channel_stat):Number of original tweets
#' @return *        N_hash  (channel_stat):Number of hashtags detected in channel
#' @return *        N_mention (channel_stat): Number of mention detected in channel
#' @return *        N_links  (channel_stat):Number of links detected in channel
#' @return *        Nuniq_authors (channel_stat):Number of unique authors
#' @return *        Nuniq_hash (channel_stat): Number of unique hashs
#' @return *        Nuniq_mentions (channel_stat): Number of unique mentions
#' @return *        Nuniq_links (channel_stat): Number of unique links
#' @return *        Nmean_links (channel_stat): Mean links for tweet
#' @return *        Nfull_retweet (channel_stat): Number of all retweets given by platorm
#' @return *        Nfull_retweet_missing (channel_stat): Number of tweet given without platform statistics
#' @return *        Nreplies (channel_stat): Number of of replies
#' @return *        Nfavor (channel_stat): Number of of cumulative favorites given by platform
#' @return *        Ntweets0links  (channel_stat): Number of tweets with no links
#' @return *        Ntweets1link (channel_stat): N of tweets with 1 link
#' @return *        NtweetsNlinks (channel_stat): Number of tweets more than links
#' @return *        Ntweets0mentions (channel_stat): Number of  tweets with no mentions
#' @return *        Ntweets1mention (channel_stat): Number of  tweets with 1 mention
#' @return *        NtweetsNmentions (channel_stat): Number of  tweets more than mentions
#' @return *        Ntweets0hashs (channel_stat): Number of  tweets with no hashtags
#' @return *        Ntweets1hash (channel_stat): Number of  tweets with 1 hashtag
#' @return *        NtweetsNhashs (channel_stat): Number of tweets more than hashtags
#' @return **table_message** :  Frequency data.frame of message
#' @return **table_hash** :  Frequency data.frame of hashtag
#' @return **table_mentions** :  Frequency data.frame of mentions
#' @return **table_authors** :  Frequency data.frame of authors
#' @return **table_replies** :  Frequency data.frame of replies
#' @return **table_authors_retweeted**  :  Frequency data.frame of authors retweeted
#' @return **table_authors_retweeter** :  Frequency data.frame of authors that is a retweeter
#' @return **rank_authors_retweet** :  Frequency data.frame of retweet by using platform data
#' @return **rank_message_retweet** :  Frequency data.frame of message by using platform data
#' @return **top_message** :  TopN messages in channel
#' @return **top_authors** :  TopN authors in channel
#' @return **top_hash** :  TopN hashtag
#' @return **top_mentions** :  TopN user mentioned
#' @return **top_replies** :  TopN user have made replies
#' @return **top_authors_retweeted** :  TopN user that have retweeted in channel
#' @return **top_authors_retweeter** :  TopN user that have made retweet in channel
#' @return **topfull_authors_retweeted** :  TopN author that have retweeted in platform
#' @return **topfull_message_retweeted** :  TopN message that have retweeted in platform
#' @return **daily_stat** :  Daily Temporal data of channel statistic data
#' @return **authors_date** :  DateTime authors activity in channel
#' @return **links_date** :  DateTime authors activity in channel
#' @return **hash_date** :  DateTime hashtag presence in channel
#' @return **mentions_date** :  DateTime mentions presence in channel
#' @return **unique_message** :  Unique message in channel
#' @return **unique_authors** :  Unique authors in channel
#' @return **unique_hash** :  Unique hashtag in channel
#' @return **unique_mentions** :  Unique mentions in channel
#' @return **unique_authors_retweeted** :  Unique retweeted user in channel
#' @return **unique_authors_retweeter** :  Unique retweeter user in channel
#' @return **uniquefull_authors_retweeted** :  Unique retweeted user in platform
#' @return **uniquefull_message_retweeted** :  Unique retweeted message in platform
#' @return **graph_retweet_df** :   Data used for retweet graph
#' @return **graph_hash_df** :  Data used for hashtag graph
#' @return **graph_mentions_df** :  Data for used mention graph
#' @return **replies_df** :  Data for replies
#' @return **graph_retweet** :   Retweet graph object as igraph R object
#' @return **graph_mentions** :   Mention graph object as igraph R object
#' @return **authors_favorite** :   rank of authors favorite
#' @return **favorite_message_top** :   TopN favorite message
#' @return **channel_data** :   Channel_data
#' @return **channel_corpus** :   Tm Corpus of messages without mentions and links and optionally without hashtag
#' @return **word_freq_matr** :   qdap wfm object Word frequency matrix.
#' @return **account_stats** :   Statistic account's activity by date.
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  channel,stats
#'
#'
#'
#' @export
#'
#'

channel_analytic=function(channel_obj,use_channel_dates=TRUE, start_date=NULL, end_date=NULL,Ntop=11,temporal_check=FALSE,
                          Nmin=25,naming="",only_original_tweet=FALSE,lowercase=TRUE,stopword = tm::stopwords("it"),
                          account_tw="", corpus_hashtag=TRUE) 
                          
                          {
  
  
 
  #####################################################################################
  # Data checks
  
  rows=nrow(channel_obj)
  
  if (rows < Nmin) { stop("Channel with too few records.")};
  
  message(paste(" Channel:", deparse(substitute(channel_obj)),"\n",
                "Elements:", rows ,"\n", 
                "Ntop:", Ntop  ,"\n",
                "Temporal Check:",temporal_check,"\n",
                "Minimum data:",Nmin,"\n",
                "Type stream:",naming,"\n",
                "Native Channel:",only_original_tweet,"\n",
                "Lowering case message's text:",lowercase,"\n"))
  
  if ( naming == "account_analitics")  {message(paste("Account Twitter:",account_tw,"\n"))}
  
  if ( (naming == "account_analitics") &&   (account_tw == "") ) { stop("Channel analitics need an Twitter account!")};
  
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
  
  if (naming == "account_analitics")
    {
    channel_obj=channel_obj[,1:22]
    name_user_tweet_activity=c("id","link_tweet","text","dateTime","impress","interazioni","inter_rate",
                               "retweetCount","repliesCount","favoriteCount","clickonuserprofile","clickonlink",
                               "clickonlinkhash","details","clickonPermalinks","open_app","n_install_app",
                               "followsCount","email_send","tel_calls","mediaVisCount","interVisCount") 
    names(channel_obj)=name_user_tweet_activity
    channel_obj$data=as.Date(channel_obj$dateTime)
    channel_obj$hour=lubridate::hour(channel_obj$dateTime)
    channel_obj$month=lubridate::month(channel_obj$dateTime)
    channel_obj$screeName=account_tw
    
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
  
  
                            
  #####################################################################################
  # Temporal filter of channel
  if ( use_channel_dates == TRUE) 
    {
      start_date=head(channel_obj$data[which(channel_obj$data== as.character(min(as.Date(channel_obj$data))))],1);
      end_date=tail(channel_obj$data[which(channel_obj$data== as.character(max(as.Date(channel_obj$data))))],1);
    }
  
  if (as.Date(start_date) > as.Date(end_date)) { stop(" End Date is older than Start date. ")};
  
  if ( temporal_check==TRUE) 
    {
    
       if (as.Date(start_date) < as.Date(head(channel_obj$data,1))) { stop("Start Date of analisys not defined." )};
       if (as.Date(end_date) > as.Date(tail(channel_obj$data,1))) { stop("End Date of analisys not defined." )};
       channel_obj=channel_obj[min(which(channel_obj$data==as.character(start_date))):max(which(channel_obj$data==as.character(end_date))),]
  
  }
  
  #####################################################################################
  # Create data.frames for other count statistics.
  
  ls_retweet=unlist(lapply(channel_obj$text,FUN=function(x) is.retweet(x)))
 
  if (only_original_tweet==TRUE) { channel_obj=channel_obj[which(ls_retweet==0),]  }
  
  ls_retweet=unlist(lapply(channel_obj$text,FUN=function(x) is.retweet(x)))
  
  ls_retweeted_authors=as.character(rep(NA,nrow(channel_obj)))
  for ( i in 1:length(ls_retweeted_authors)) {ls_retweeted_authors[i]=retweeted_users(channel_obj$text[i]) }
 
 
                                 
  
  
   ####################################################################################
  # Create lists to be used for count statistics.
  ls_links=lapply(channel_obj$text,function(x) qdapRegex::rm_url(x, extract=TRUE))
  
  
 
  if ( lowercase == TRUE) {
                           channel_obj$text=tolower(channel_obj$text)
  }
  
  ls_hash=lapply(channel_obj$text,FUN=function(x) qdapRegex::rm_hash(x,extract=T))
  ls_tag=lapply(channel_obj$text,FUN=function(x) extract_mentions(x))
  ls_lenhash=unlist(lapply(ls_hash,FUN=function(x) ifelse(is.na(x),0, length(qdapRegex::rm_hash(x,extract=T)[[1]]))))
  ls_lenlinks=unlist(lapply(ls_links,FUN=function(x) ifelse(is.na(x),0, length(qdapRegex::rm_url(x, extract=TRUE)[[1]]))))
  ls_lentag=unlist(lapply(ls_tag,FUN=function(x) ifelse(is.na(x),0, length(extract_mentions(x)[[1]]))))
  ls_words=unlist(lapply(channel_obj$text,FUN=function(x) qdap::word_count(x)))
  
  message("Text message are processed!\n")
                                                                      
  #######################################################################################
  # Create data.frame date,retweeted_authors and authors.
                         
  ls_retweeted_df=na.omit(data.frame(data=channel_obj$data,
                             retweeted_authors=ls_retweeted_authors,
                             authors=channel_obj$screeName))
                             
  
  ####################################################################################
  # Extract replies and organize a frame
  
  replies_id=grep("^@",channel_obj$text)
  channel_obj$replies=NA
  channel_obj$replies[replies_id]=1
  ls_replies_df=data.frame(data=channel_obj$data,authors=channel_obj$screeName,replies=channel_obj$replies)
  
  ####################################################################################
  # Replies stats
  
  fullretweet_day=aggregate(as.numeric(channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)]),list(channel_obj$data[which(!duplicated(channel_obj$text)==TRUE)]),sum,na.rm = TRUE)
  names(fullretweet_day)=c("date","retweetCount")
  fullretweet_day$date=as.Date(fullretweet_day$date)
  
  fullreplies_day=aggregate(as.numeric(channel_obj$replies),list(channel_obj$data),sum,na.rm = TRUE)
  names(fullreplies_day)=c("date","repliesCount")
  fullreplies_day$date=as.Date(fullreplies_day$date)
  
  fullretweet_missing=length(which(is.na(channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)])))
  fullretweet_channel_stat_sum=sum(as.numeric(channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)]),na.rm=T)
  replies_channel_stat_sum=length(replies_id)
  
  
  
  #######################################################################################
  # Create data.frame date,message and authors.
  
  ls_favorite_df=data.frame(data=channel_obj$data[which(!duplicated(channel_obj$text)==TRUE)],
                            message=channel_obj$text[which(!duplicated(channel_obj$text)==TRUE)],
                            authors=channel_obj$screeName[which(!duplicated(channel_obj$text)==TRUE)],
                            favoriteCount=channel_obj$favoriteCount[which(!duplicated(channel_obj$text)==TRUE)],
                            is.retweet=ls_retweet[which(!duplicated(channel_obj$text)==TRUE)])
  
  day_favorite=aggregate(as.numeric(ls_favorite_df$favoriteCount),list(ls_favorite_df$data),sum)
  names(day_favorite)<-c("date","N_favor")
  day_favorite$date=as.Date(day_favorite$date)
  
  
  ls_favorite_df=ls_favorite_df[order(-as.numeric(ls_favorite_df$favoriteCount)),]
  
  
  rank_authors_favorite=aggregate(as.numeric(channel_obj$favoriteCount[which(!duplicated(channel_obj$text)==TRUE)]),
                                  list(channel_obj$screeName[which(!duplicated(channel_obj$text)==TRUE)])
                                  ,sum)
  rank_authors_favorite=rank_authors_favorite[order(-rank_authors_favorite[,2]),]
  names(rank_authors_favorite)<-c("authors","favoriteCount")
  
  #########################################################################
  
  
  ls_message_df=data.frame(data=channel_obj$data[which(!duplicated(channel_obj$text)==TRUE)],
                          message=channel_obj$text[which(!duplicated(channel_obj$text)==TRUE)],
                          authors=channel_obj$screeName[which(!duplicated(channel_obj$text)==TRUE)],
                          retweetCount=channel_obj$retweetCount[which(!duplicated(channel_obj$text)==TRUE)],
                          is.retweet=ls_retweet[which(!duplicated(channel_obj$text)==TRUE)])
  
  rank_authors_retweet=aggregate(as.numeric(ls_message_df$retweetCount),list(ls_message_df$authors),sum)
  rank_authors=rank_authors_retweet[order(-rank_authors_retweet[,2]),]
  names(rank_authors_retweet)<-c("authors","retweetCount")
  
  
  rank_message_retweet=aggregate(as.numeric(ls_message_df$retweetCount),list(ls_message_df$message),sum)
  rank_message_retweet=rank_message_retweet[order(-rank_message_retweet[,2]),]
  names(rank_message_retweet)<-c("message","retweetCount")
  
  rank_authors_retweet=rank_authors_retweet[ order(-rank_authors_retweet[,2]), ]
  rank_message_retweet=rank_message_retweet[ order(-rank_message_retweet[,2]), ]
  
  ##########################################################################################################################################
  # retrieve other information from channel stack.
  
  rank_message_retweet$retweeted_authors=retweeted_users(rank_message_retweet$message)
  id_na_message_retweet=which(is.na(rank_message_retweet$retweeted_authors))
  not_retweet_with_authors=as.character(rank_message_retweet$message[id_na_message_retweet])
  
  for ( i in seq_along(not_retweet_with_authors)) 
  {
    rank_message_retweet$retweeted_authors[id_na_message_retweet[i]]=as.character(channel_obj$screeName[min(which(channel_obj$text ==not_retweet_with_authors[i]))])
  }
  
  
  rank_message_retweet$data=NA
  
  
  for ( i in seq_along(rank_message_retweet$message)) 
  {
    rank_message_retweet$data[i]=as.character(channel_obj$data[min(which((channel_obj$text %in%  rank_message_retweet$message[i] )==T))])
  }
  
  
  #######################################################################################
  # Create data.frame date,authors and is retweet.
  
  ls_authors_df=data.frame(data=channel_obj$data,authors=channel_obj$screeName,ls_retweet)
  names(ls_authors_df)=c("data","authors","retweet")
  
  
  
  #####################################################################################
  # Create data.frame date and hashtag.
  
  
  ls_hash_long=list()
  for ( i in seq_along(ls_hash)) {ls_hash_long[[i]]=cbind(as.character(channel_obj$data[i]),unlist(ls_hash[[i]]),ls_retweet[i],as.character(channel_obj$screeName[i]))}
  ls_hash_df=as.data.frame(do.call(rbind,ls_hash_long))
  names(ls_hash_df)=c("data","hashtag","retweet","authors")
  
  #####################################################################################
  # Create data.frame date and mentions.
  
  ls_tag_long=list()
  for ( i in seq_along(ls_tag)){ls_tag_long[[i]]=cbind(as.character(channel_obj$data[i]),unlist(ls_tag[[i]]),ls_retweet[i],as.character(channel_obj$screeName[i]))}
  ls_tag_df=as.data.frame(do.call(rbind,ls_tag_long))
  names(ls_tag_df)=c("data","mention","retweet","authors")
  ls_tag_df$mention=gsub("^@","",ls_tag_df$mention)
  
  #####################################################################################
  # Create data.frame date and links.
  
  ls_links_long=list()
  for ( i in seq_along(ls_links)) {ls_links_long[[i]]=cbind(as.character(channel_obj$data[i]),unlist(ls_links[[i]]),ls_retweet[i],as.character(channel_obj$screeName[i]))}
  ls_links_df=as.data.frame(do.call(rbind,ls_links_long))
  names(ls_links_df)=c("data","links","retweet","authors")
  
  ###########################################################################################
  # Creating arrays date and elements
  
  authors_unique=na.omit(unique(ls_authors_df[,1:2]))
  links_unique=na.omit(unique(ls_links_df[,1:2]))
  hash_unique=na.omit(unique(ls_hash_df[,1:2]))
  tag_unique=na.omit(unique(ls_tag_df[,1:2]))
  
  authors_purged=na.omit(ls_authors_df)
  links_purged=na.omit(ls_links_df)
  hash_purged=na.omit(ls_hash_df)
  tag_purged=na.omit(ls_tag_df)
   
  #####################################################################################

  lenhash_df=data.frame(data=as.Date(channel_obj$data),lenhash=ls_lenhash)
  lenhash_df_day_mean=aggregate(lenhash_df$lenhash,list(lenhash_df$data),mean)
  names(lenhash_df_day_mean)=c("date","Nmean_hashtag")
  
  lentag_df=data.frame(data=as.Date(channel_obj$data),lentag=ls_lentag)
  lentag_df_day_mean=aggregate(lentag_df$lentag,list(lentag_df$data),mean)
  names(lentag_df_day_mean)=c("date","Nmean_mentions")
  
  lenwords_df=data.frame(data=as.Date(channel_obj$data),lenwords=ls_words)
  lenwords_df_day_mean=aggregate(lenwords_df$lenwords,list(lenwords_df$data),mean)
  names(lenwords_df_day_mean)=c("date","Nmean_words")
  
  lenlinks_df=data.frame(data=as.Date(channel_obj$data),lenlinks=ls_lenlinks)
  lenlinks_df_day_mean=aggregate(lenlinks_df$lenlinks,list(lenlinks_df$data),mean)
  names(lenlinks_df_day_mean)=c("date","Nmean_links")
  
  #####################################################################################
  # retweet stats the ratio is ever ntive retweet/ native
  
  check_retweet=sum(ls_retweet)
  retweet_df=data.frame(data=channel_obj$data,is.retweet=ls_retweet)
  retweet_df_stats=data.frame(native_tweets=rep(0,length(levels(as.factor(retweet_df$data)))),
                              native_retweets=rep(0,length(levels(as.factor(retweet_df$data)))))
  
  
  
  if ((only_original_tweet==FALSE) && (check_retweet == length(ls_retweet))) { retweet_df_stats$native_retweets=as.data.frame.array(table(retweet_df$data,retweet_df$is.retweet))[,1]
                                                                             }
  if ((only_original_tweet==FALSE) && (check_retweet > 0)) {retweet_df_stats$native_tweets=as.data.frame.array(table(retweet_df$data,retweet_df$is.retweet))[,1];
                                                            retweet_df_stats$native_retweets=as.data.frame.array(table(retweet_df$data,retweet_df$is.retweet))[,2];
                                                          }
  
  if (only_original_tweet==TRUE) {retweet_df_stats$native_tweets=as.data.frame.array(table(retweet_df$data,retweet_df$is.retweet))[,1]}
  
 
 
  retweet_df_stats$ratio=retweet_df_stats$native_retweets/retweet_df_stats$native_tweets
  retweet_df_stats$ratio[which(retweet_df_stats$ratio==Inf)]=NA
  retweet_df_stats$ratio[which(retweet_df_stats$ratio==NaN)]=NA
  retweet_df_stats$ratio[which(is.na(retweet_df_stats$ratio))]=0
  
  retweet_stat=data.frame(date=as.Date(rownames(as.data.frame.array(table(retweet_df$data,retweet_df$is.retweet)))),
                          native_tweets=retweet_df_stats$native_tweets,
                          native_retweets=retweet_df_stats$native_retweets,
                          retweet_ratio=retweet_df_stats$ratio)
  
  
  ########################################################################################
  # Creating daily stats
  
  lenauthorsunique_day=as.data.frame.array(table(authors_unique$data))
  lenauthorsunique_day_df=data.frame(date=as.Date(rownames(lenauthorsunique_day)),Nunique_authors=as.vector(lenauthorsunique_day[,1]))
  
  lenauthors_day=as.data.frame.array(table(authors_purged$data))
  lenauthors_day_df=data.frame(date=as.Date(rownames(lenauthors_day)),Nday_authors=as.vector(lenauthors_day[,1]))
  
  
  #########################################################################
  
  lenlinksunique_day=as.data.frame.array(table(links_unique$data))
  lenlinksunique_day_df=data.frame(date=as.Date(rownames(lenlinksunique_day)),Nuniq_links=as.vector(lenlinksunique_day[,1]))
  
  lenlinks_day=as.data.frame.array(table(links_purged$data))
  lenlinks_day_df=data.frame(date=as.Date(rownames(lenlinks_day)),Nday_links=as.vector(lenlinks_day[,1]))
  
  
  #########################################################################
  
  
  lenhashunique_day=as.data.frame.array(table(hash_unique$data))
  lenhashunique_day_df=data.frame(date=as.Date(rownames(lenhashunique_day)),Nuniq_hash=as.vector(lenhashunique_day[,1]))
  
  lenhash_day=as.data.frame.array(table(hash_purged$data))
  lenhash_day_df=data.frame(date=as.Date(rownames(lenhash_day)),Nday_hash=as.vector(lenhash_day[,1]))
  
  
  #########################################################################
  
  lentagunique_day=as.data.frame.array(table(tag_unique$data))
  lentagunique_day_df=data.frame(date=as.Date(rownames(lentagunique_day)),Nuniq_mention=as.vector(lentagunique_day[,1]))
  
  lentag_day=as.data.frame.array(table(tag_purged$data))
  lentag_day_df=data.frame(date=as.Date(rownames(lentag_day)),Nday_mention=as.vector(lentag_day[,1]))
  
  
  #########################################################################
  # Create daily channel stats
  
  # Create a continuous data series
  
  ts_date=data.frame(date=seq.Date(as.Date(start_date),as.Date(end_date),1))
  
  daily_stat=merge(ts_date,retweet_stat,all.x=T)
  daily_stat=merge(daily_stat,lenauthors_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenhash_day_df,all.x=T)
  daily_stat=merge(daily_stat,lentag_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenlinks_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenhash_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lentag_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lenwords_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lenlinks_df_day_mean,all.x=T)
  daily_stat=merge(daily_stat,lenauthorsunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenhashunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,lentagunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,lenlinksunique_day_df,all.x=T)
  daily_stat=merge(daily_stat,fullretweet_day,all.x=T)
  daily_stat=merge(daily_stat,fullreplies_day,all.x=T)
  daily_stat=merge(daily_stat,day_favorite,all.x=T)
  ####################à
  daily_stat$retweet_ratio=round(as.numeric(daily_stat$retweet_ratio),2)
  daily_stat$Nmean_hashtag=round(as.numeric(daily_stat$Nmean_hashtag),2)	
  daily_stat$Nmean_mentions=round(as.numeric(daily_stat$Nmean_mentions),2)	
  daily_stat$Nmean_words=round(as.numeric(daily_stat$Nmean_words),2)
  daily_stat$Nmean_links=round(as.numeric(daily_stat$Nmean_links),2)
  
  message("Daily stats calculated!\n")
  
  #################################################################################
  # Frequency analisys
  
  table_message=as.data.frame.array(sort(table(channel_obj$text),decreasing=T))
  
  table_message=data.frame(message=rownames(table_message),
                           Freq=as.vector(table_message))
  
  names(table_message)<-c("message","freq")
  
  rownames(table_message)<-NULL

  table_message$data=NA
  table_message$authors=NA
  
  
  ind=sapply(table_message$message,function(x) match(x,channel_obj$text))
  
  for ( i in 1:length(ind)) {
    table_message$data[i]=as.character(channel_obj$data[ind[i]])
    table_message$authors[i]=as.character(channel_obj$screeName[ind[i]])
  }
  
  table_message=na.omit(table_message)
  table_message$retweeted_authors=NA
  
  for ( i in 1:nrow(table_message)) {
    table_message$retweeted_authors[i]=retweeted_users(as.character(table_message$message[i]))
  }
  
  message("Table_message stats calculated!\n")
  
  
  ##########################################################################
  if ((only_original_tweet==FALSE ) && (naming!="account_statistics")) 
  {
  table_authors_retweeted=as.data.frame.array(sort(table(ls_retweeted_df$retweeted_authors),decreasing=T))
  
  table_authors_retweeted=data.frame(authors=rownames(table_authors_retweeted),
                                     Freq=as.vector(table_authors_retweeted))
                                     
  names(table_authors_retweeted)<-c("authors_retweeted","freq")
  rownames(table_authors_retweeted)<-NULL
  message("Table authors retweeted stats calculated!\n")
  
  }
  
  if (only_original_tweet==TRUE || (naming=="account_statistics"))
  {
  table_authors_retweeted=data.frame(authors_retweeted=NA,freq=NA)
  };
  
  ##########################################################################
  if ((only_original_tweet==FALSE ) && (naming!="account_statistics")) 
  {
  
  table_authors_retweeter=as.data.frame.array(sort(table(ls_retweeted_df$authors),decreasing=T))
  
  table_authors_retweeter=data.frame(authors=rownames(table_authors_retweeter),
                                     Freq=as.vector(table_authors_retweeter))
  
  names(table_authors_retweeter)<-c("authors_retweeter","freq")
  rownames(table_authors_retweeter)<-NULL
  
  message("Table authors retweeter stats calculated!\n")
 
  }
  
  
  if ((only_original_tweet==TRUE) || (naming=="account_statistics")) 
  {
  table_authors_retweeter=data.frame(authors_retweeter=NA,freq=NA)
  };
  
  
  ##########################################################################
  
  
  table_authors=as.data.frame.array(sort(table(ls_authors_df$authors),decreasing=T))
  table_authors=data.frame(authors=rownames(table_authors),
                           Freq=as.vector(table_authors))
  names(table_authors)<-c("authors","freq")
  rownames(table_authors)<-NULL
  
  ##########################################################################
  
  table_hash=as.data.frame.array(sort(table(ls_hash_df$hashtag),decreasing=T))
  table_hash=data.frame(hashtag=rownames(table_hash),
                        Freq=as.vector(table_hash))
  names(table_hash)<-c("hashtag","freq")
  rownames(table_hash)<-NULL
  
  ##########################################################################
  
  table_mentions=as.data.frame.array(sort(table(ls_tag_df$mention),decreasing=T))
  table_mentions=data.frame(users=rownames(table_mentions),
                            Freq=as.vector(table_mentions))
  names(table_mentions)<-c("mentions","freq")
  rownames(table_mentions)<-NULL
  
  ##########################################################################
  
  table_replies=as.data.frame.array(sort(table(ls_replies_df$authors),decreasing=T))
  table_replies=data.frame(users=rownames(table_replies),
                           Freq=as.vector(table_replies))
  names(table_replies)<-c("replies","freq")
  rownames(table_replies)<-NULL
  
  message("Tables of authors and replies are calculated!\n")
  
  #########################################################################
  # Create full channel stats
  
  full_stat=data.frame(N_tweets=length(channel_obj$text),
                       N_retweets=sum(retweet_stat$native_retweets,na.rm=T),
                       N_native_tweets=sum(retweet_stat$native_tweets,na.rm=T),
                       N_hash=nrow(hash_purged),
                       N_mention=nrow(tag_purged),
                       N_links=nrow(links_purged),
                       Nuniq_authors=length(unique(ls_authors_df[,2])),
                       Nuniq_hash=length(unique(ls_hash_df[,2])),
                       Nuniq_mentions=length(unique(ls_tag_df[,2])),
                       Nuniq_links=length(unique(ls_links_df[,2])),
                       Nmean_links=round(mean(lenlinks_df_day_mean$Nmean_links),2),
                       Nfull_retweet=fullretweet_channel_stat_sum,
                       Nfull_retweet_missing=fullretweet_missing,
                       Nreplies=replies_channel_stat_sum,
                       Nfavor=sum(ls_favorite_df$N_favor,na.rm=T),
                       Ntweets0hashs = length(which(ls_lenhash==0)),
                       Ntweets1hashs = length(which(ls_lenhash==1)),
                       NtweetsNhashs = length(which(ls_lenhash>1)),
                       Ntweets0mentions = length(which(ls_lentag==0)),
                       Ntweets1mentions = length(which(ls_lentag==1)),
                       NtweetsNmentions = length(which(ls_lentag>1)),
                       Ntweets0links = length(which(ls_lenlinks==0)),
                       Ntweets1links = length(which(ls_lenlinks==1)),
                       NtweetsNlinks = length(which(ls_lenlinks>1))
                    
  )
  message("Full stats of channel are done!\n")
  
  
  ############################################################################################################
  # Create a mention graph
  
  graph_mentions_df=na.omit(ls_tag_df)
  mat_men_graph=na.omit(data.frame(whopost=graph_mentions_df[,4],whomentioned=graph_mentions_df[,2]))
  men_graph = igraph::graph.edgelist(as.matrix(na.omit(mat_men_graph)))
  E(men_graph )$weight <- 1
  men_graph <- igraph::simplify(men_graph, remove.loops=FALSE)
 
   message("Mention Graph of channel are done!\n")
  
  ############################################################################################################
  # Create a retweet graph
  
  rt_graph=NULL
  
  if (naming!="account_statistics") 
  { 
          rt_graph= igraph::graph.edgelist(as.matrix(cbind(ls_retweeted_df[,3],ls_retweeted_df[,2])))
          E(rt_graph )$weight <- 1
          rt_graph <- igraph::simplify(rt_graph, remove.loops=FALSE)
          message("Retweet  Graph of channel are done!\n")
          
  }
  
  ############################################################################################################
  # Get corpus and termdocfrequency matrix as qdap object
  
  corpus=getCorpus(channel_obj$text,hashtag=corpus_hashtag)
  word_freq_matr=qdap::wfm(corpus,stopwords=stopword)
  message("Corpus of words and frequency qdap matrix  of channel are done!\n")
  
  ########################################################################################
  
  channel_obj$hashtagCount=lenhash_df$lenhash
  channel_obj$linksCount=lenlinks_df$lenlinks
  channel_obj$mentionCount=lentag_df$lentag
  
  ########################################################################################
  
  res=list(channel_stat=full_stat,
           table_message=table_message,
           table_hash=table_hash,
           table_mentions=table_mentions,
           table_authors=table_authors,
           table_replies=table_replies,
           table_authors_retweeted=table_authors_retweeted,
           table_authors_retweeter=table_authors_retweeter,
           rank_authors_retweet=rank_authors_retweet,
           rank_message_retweet=rank_message_retweet,
           top_message=table_message[1:Ntop,],
           top_authors=table_authors[1:Ntop,],
           top_hash=table_hash[1:Ntop,],
           top_mentions=table_mentions[1:Ntop,],
           top_replies=table_replies[1:Ntop,],
           top_authors_retweeted=table_authors_retweeted[1:Ntop,],
           top_authors_retweeter=table_authors_retweeter[1:Ntop,],
           topfull_authors_retweeted=rank_authors_retweet[1:Ntop,],
           topfull_message_retweeted=rank_message_retweet[1:Ntop,],
           daily_stat=daily_stat,
           authors_date=authors_purged,
           links_date=links_purged,
           hash_date=hash_purged,
           mentions_date=tag_purged,
           unique_message=unique(table_message[,1]),
           unique_authors=unique(table_authors[,1]),
           unique_hash=unique(table_hash[,1]),
           unique_mentions=unique(table_mentions[,1]),
           unique_authors_retweeted=unique(table_authors_retweeted[,1]),
           unique_authors_retweeter=unique(table_authors_retweeter[,1]),
           uniquefull_authors_retweeted=unique(rank_authors_retweet[,1]),
           uniquefull_message_retweeted=unique(rank_message_retweet[,1]),
           graph_retweet_df=ls_retweeted_df,
           graph_hash_df=na.omit(ls_hash_df),
           graph_mentions_df=na.omit(ls_tag_df),
           replies_df=ls_replies_df,
           graph_retweet=rt_graph,
           graph_mentions=men_graph,
           authors_favorite=rank_authors_favorite,
           favorite_message_top=head(ls_favorite_df,Ntop),
           channel_data=channel_obj,
           account_stats=NULL,
           channel_corpus=corpus,
           word_freq_matr=word_freq_matr
         
  )
  
  if (naming=="account_statistics") 
  { stats_activity=aggregate(channel_obj[,5:22], list(channel_obj$data), sum)
    names(stats_activity)[1]="data"
    rownames(stats_activity)=stats_activity$data
    res$account_stats=stats_activity 
  }
  
  return(res)
}
