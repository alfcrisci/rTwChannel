#' channel_analytic
#'
#' @description Extract many informative stats and object  from a set of tweet messages parsed as channel
#'
#' @param  channel_obj  data.frame Dataset of tweets.
#' @param  use_channel_dates logical Use temporal indication of channel.
#' @param  start_date   character Date of analisys starting.
#' @param  end_date   character Date of  analisys ending.
#' @param  Ntop integer indicate the maximum number for top statistics.
#' @param  Nmin integer indicate the minimal data numerosity.
#' @param  naming character Indicate which naming framework is adopted. "TAGS","DISIT","rtweet","account_analitics" are names allowed.
#' @param  only_original_tweet logical Taking into account only original. Default all tweets are considered.
#' @param  lowercase logical Consider  all text as lower case. Default is TRUE.
#' @param  corpus_lang character language used in Corpora analisys done. Default is "it".
#' @param  stopword logical if stopword set to is  removed from word frequency matrix. Default italian stopwords of R tm package.
#' @param  corpus_hashtag logical Corpus analisys not considering the hashtag.
#' @param  account_tw character User account if naming parameter is an "account_statistics".
#' @param  graph_analisys Igraph object Graph analisys done. default is FALSE.
#' @param  corpus_analisys qDap object wfm for Corpora analisys done. Default is FALSE.
#' @return Return a R list object  for channel analisys.
#' @return channel_stat list: Channel summaries of following parameters.
#' @return daily_stat data.frame : Daily channel statistics.
#' @return table_message data.frame :Frequency data  of messages.
#' @return table_hash data.frame : Frequency data  hashtags.
#' @return table_mentions data.frame : Frequency data of mentions.
#' @return table_links data.frame : Frequency data.frame of urls.
#' @return table_authors data.frame : Frequency data of authors.
#' @return table_authors_retweeter data.frame : Frequency data.frame of authors that are active retweeter.
#' @return retweeted_authors data.frame : Frequency of retweeted authors.
#' @return favorite_authors data.frame : Frequency data of favorited authors.
#' @return favorite_message data.frame : Rank of Favorite messages.
#' @return rank_message_retweeted data.frame : Frequency of retweeted message.
#' @return top_message data.frame : TopN messages in channel.
#' @return top_authors data.frame : TopN authors in channel.
#' @return top_hash data.frame : TopN hashtag.
#' @return top_mentions data.frame : TopN user mentioned.
#' @return top_links data.frame : TopN links.
#' @return topfull_authors_retweeter data.frame : TopN author that have retweeted.
#' @return topfull_message_retweeted data.frame : TopN message that have retweeted.
#' @return graph_retweet igraph object: Simplified Retweet graph.
#' @return graph_mentions igraph object: Simplified Mention graph object as igraph R object.
#' @return channel_data data.frame: Channel_data used for analisys with other ancillary variables.
#' @return channel_corpus wfm object:  Corpus of messages without mentions and links and optionally without hashtag.
#' @return word_freq_matr qdap wfm object : Word frequency matrix.
#' @return account_stats data.frame : Statistic account's activity by date.
#'
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  channel stats
#'
#'
#' @importFrom igraph graph.edgelist simplify
#' @importFrom lubridate dmy_hms month hour ymd_hms
#' @export
#'
#'

channel_analytic=function(channel_obj,use_channel_dates=TRUE, start_date=NULL, end_date=NULL,Ntop=11,
                          Nmin=25,naming="",only_original_tweet=FALSE,lowercase=TRUE,corpus_lang= "it",stopword=TRUE,
                          account_tw="",corpus_hashtag=TRUE,graph_analisys=FALSE,corpus_analisys=FALSE) 
                          
                          {
                          
                      
 
  #####################################################################################
  # Data checks
  
  if ((naming == "account_analitics") &&   (account_tw == "") ) { stop("Channel analitics need an Twitter account!")};
  
  if ( naming == "account_analitics")  {message(paste("Account Twitter:",account_tw,"\n"))}
 
  
  
  ##############################################################################################################
  # Check minimal dimensionality
  
  if (nrow(channel_obj) < Nmin) { stop("Channel with too few records.")};
  
  
  
  
 
  ############################################################################################################################
  # Format and rename data
  
  if ( naming == "TAGS") {
    channel_obj$text=gsub("[\x80-\xFF]","",channel_obj$message) # remove multibyte
    channel_obj$created <- dmy_hms(channel_obj$time)
    channel_obj=channel_obj[which(!is.na(channel_obj$created)),]
    channel_obj$date <- as.Date(dmy_hms(channel_obj$created))
    channel_obj$screenName=tolower(channel_obj$from_user)
    channel_obj$id=as.numeric(channel_obj$id_str)
    channel_obj$twitterId=as.numeric(channel_obj$id_str)
    channel_obj$lang=channel_obj$user_lang
    channel_obj$from_user<-NULL
    channel_obj$user_lang<-NULL
    channel_obj$message<-NULL
    channel_obj$created_at<-NULL
    channel_obj$retweetCount<-rep(NA,nrow(channel_obj))
    channel_obj$entities_str<-NULL
    channel_obj$favoriteCount<-rep(NA,nrow(channel_obj))
    channel_obj$ls_hash_full<-rep(NA,nrow(channel_obj))
    channel_obj$ls_links=rep(NA,nrow(channel_obj))
    channel_obj$publicationTime <- channel_obj$time
    channel_obj$time<-NULL
    channel_obj=channel_obj[rev(1:nrow(channel_obj)),]
    channel_obj$hour=hour(ymd_hms(channel_obj$publicationTime))
    channel_obj$month=month(ymd_hms(channel_obj$publicationTime))
     channel_obj$mentions = tolower(unlist(lapply(extract_mentions(channel_obj$text), 
                                       function(x) paste(x, collapse = " "))))
     channel_obj$mentions[which( channel_obj$mentions=="na")]=""                                  
     channel_obj$links = tolower(unlist(lapply(extract_links(channel_obj$text), 
                                    function(x) paste(x, collapse = " "))))
     channel_obj$links[which(channel_obj$links=="na")]=""                                 
     channel_obj$hashtagsOnTwitter = tolower(unlist(lapply(extract_hashtag(channel_obj$text), 
                                                function(x) paste(x, collapse = " "))))
     channel_obj$locationUser=NA
     channel_obj$place=NA
     channel_obj$time_zone=NA
     channel_obj$class_users=NA
     channel_obj$class_hashtag=NA
     channel_obj$class_message=NA
     channel_obj$class_custom=NA
     channel_obj$geo_lat=as.numeric(channel_obj$latitude)
     channel_obj$geo_long=as.numeric(channel_obj$longitude)
    
  }
  
  
  if ( naming == "DISIT") {
    
    channel_obj$text=gsub("[\x80-\xFF]","",channel_obj$message) # remove multibyte
    channel_obj$date=as.character(as.Date(lubridate::ymd_hms(channel_obj$publicationTime)))
    channel_obj$screenName=tolower(channel_obj$twitterUser)
    channel_obj$created=channel_obj$publicationTime
    channel_obj$id=channel_obj$twitterId
    channel_obj$message<-NULL
    channel_obj$hour=hour(ymd_hms(channel_obj$publicationTime))
    channel_obj$month=month(ymd_hms(channel_obj$publicationTime))
    channel_obj$isRetweet=channel_obj$retweet
    channel_obj$retweet<-NULL
    channel_obj$class_users=NA
    channel_obj$class_hashtag=NA
    channel_obj$class_message=NA
    channel_obj$class_custom=NA
    
  }
  

    
  if ( naming == "rtweet") {
     channel_obj$created_at=as.character(channel_obj$created_at)
     channel_obj$screenName=tolower(channel_obj$screen_name);
     channel_obj$screen_name=NULL
     channel_obj$twitterId = as.numeric(channel_obj$status_id)
     channel_obj$status_id=NULL
     channel_obj$date = as.Date(channel_obj$created_at)
     channel_obj$hour = hour(channel_obj$created_at)
     channel_obj$month = month(channel_obj$created_at)
     channel_obj$text = gsub("[\x80-\xff]", "", channel_obj$text)
     channel_obj$isRetweet = as.numeric(channel_obj$is_retweet)
     channel_obj$publicationTime = channel_obj$created_at
     channel_obj$retweetCount=channel_obj$retweet_count
     channel_obj$favoriteCount=channel_obj$favorite_count
     channel_obj$mentions = tolower(unlist(lapply(extract_mentions(channel_obj$text), 
                                       function(x) paste(x, collapse = " "))))
     channel_obj$mentions[which( channel_obj$mentions=="na")]=""                                  
     channel_obj$links = tolower(unlist(lapply(extract_links(channel_obj$text), 
                                    function(x) paste(x, collapse = " "))))
     channel_obj$links[which(channel_obj$links=="na")]=""                                 
     channel_obj$hashtagsOnTwitter = tolower(unlist(lapply(extract_hashtag(channel_obj$text), 
                                                function(x) paste(x, collapse = " "))))
     channel_obj$locationUser = NA
     channel_obj$place = channel_obj$place_name
     channel_obj$time_zone = NA
     channel_obj$class_users = NA
     channel_obj$class_hashtag = NA
     channel_obj$class_message = NA
     channel_obj$class_custom = NA
     channel_obj$geo_long = ifelse(is.na(channel_obj$coordinates),NA,as.numeric(channel_obj$coordinates[,1]))
     channel_obj$geo_lat =  ifelse(is.na(channel_obj$coordinates),NA,as.numeric(channel_obj$coordinates[,5]))
     channel_obj$coordinates =NULL
   
  }
  
  if (naming == "account_analitics")
  {
    channel_obj=channel_obj[,1:22]
    name_user_tweet_activity=c("id","link_tweet","text","dateTime","impress","interazioni","inter_rate",
                               "retweetCount","repliesCount","favoriteCount","clickonuserprofile","clickonlink",
                               "clickonlinkhash","details","clickonPermalinks","open_app","n_install_app",
                               "followsCount","email_send","tel_calls","mediaVisCount","interVisCount") 
    names(channel_obj)=name_user_tweet_activity
    channel_obj$date=as.Date(channel_obj$dateTime)
    channel_obj$hour=hour(channel_obj$dateTime)
    channel_obj$month=month(channel_obj$dateTime)
    channel_obj$screenName=account_tw
    channel_obj$text=gsub("[\x80-\xFF]","",x$text)
    channel_obj$mentions=tolower(unlist(lapply(extract_mentions(x$text),function(x) paste(x,collapse = " "))))
    channel_obj$links=unlist(lapply(extract_links(x$text),function(x) paste(x,collapse = " ")))
    channel_obj$hashtagsOnTwitter=tolower(unlist(lapply(extract_hashtag(x$text),function(x) paste(x,collapse = " "))))
    
    
  }
  
  #####################################################################################
  # Create sequence of dates
  
  seq_date_channel=seq(as.Date(range(channel_obj$date)[1]),as.Date(range(channel_obj$date)[2]),by=1)
  rangeseq=range(seq_date_channel)
  
  if (use_channel_dates==TRUE) { start_date=rangeseq[1]; end_date=rangeseq[2]};
  
  if (((class(as.Date(start_date))!="Date") ||  (class(as.Date(end_date))!="Date")) & (use_channel_dates==FALSE)) { stop("Start or Ending date to be defined!")};
  
  if (as.Date(start_date) > as.Date(end_date)) { stop(" End Date is older than Start date! ")};
                                                
  #####################################################################################
  # Impose date time ordering
  
  channel_obj=channel_obj[order(channel_obj$date),]
  #####################################################################################
  # Lowering case message
  
  if ( lowercase == TRUE) {
    channel_obj$text=tolower(channel_obj$text)
  }
  
   
  #####################################################################################
  # Temporal filter of channel
  
  if ( use_channel_dates == FALSE) 
    {
    seq_date_channel=date=seq(as.Date(start_date),as.Date(end_date),by=1) 
    channel_obj=channel_obj[which((seq_date_channel%in% channel_obj$date)==T),]  
   }
  
  #####################################################################################
  # Create data.frames for other count statistics.
   
   if (only_original_tweet==TRUE) { channel_obj=channel_obj[which(channel_obj$isRetweet==0),];
                                   }
  
  
  mat_retweet_df=data.frame(date=NA,message=NA,authors=NA,retweeted_users=NA)
  
  #######################################################################################
  # Create data.frame date,retweeted_authors and authors.
  
  if ( (length(channel_obj$isRetweet[which(channel_obj$isRetweet==1)]) > 0) && (only_original_tweet==FALSE))  { 
    
    id_retweet=which(channel_obj$isRetweet==1)
    retweeter_authors=tolower(gsub("^@","",channel_obj$screenName[id_retweet]))
    retweeted_users=tolower(gsub("^@","",unlist(lapply(channel_obj$mentions[id_retweet],function(x) unlist(strsplit(x," "))[1]))))
    mat_retweet_df=na.omit(data.frame(date=channel_obj$date[id_retweet],message=channel_obj$text[id_retweet],authors=retweeter_authors,retweeted_users=retweeted_users))
    
    }
 
  ######################################################################################################################################################################################
  # mentions
  
  id=which(channel_obj$mentions!="")
  mat_mentions=na.omit(data.frame(date=channel_obj$date[id],whomentions=channel_obj$screenName[id],mentioned=gsub("@","",channel_obj$mentions[id]),stringsAsFactors = F))
  
  if (nrow(mat_mentions)>0) {                                             
  mat_mentions_df=do.call("rbind",apply(mat_mentions,1,FUN=function(x) data.frame(expand.grid(x[1],x[2],as.character(unlist(strsplit(x[3]," ")),stringsAsFactors = F)))))
  names(mat_mentions_df)=c("date","whomention","whomentioned")
  }
  id=which(channel_obj$hashtagsOnTwitter!="")
  mat_hashtag=na.omit(data.frame(date=channel_obj$date[id],whohashtag=channel_obj$screenName[id],hashtags=channel_obj$hashtagsOnTwitter[id],stringsAsFactors = F))
  if (nrow(mat_hashtag)>0) {                                             
  
  mat_hashtag_df=do.call("rbind",apply(mat_hashtag,1,FUN=function(x) data.frame(expand.grid(x[1],x[2],as.character(unlist(strsplit(x[3]," ")),stringsAsFactors = F)))))
  names(mat_hashtag_df)=c("date","whohashtag","hashtag")
  }
  
  id=which(channel_obj$links!="")
  mat_links=na.omit(data.frame(date=channel_obj$date[id],wholinks=channel_obj$screenName[id],links=channel_obj$links[id]),stringsAsFactors = F)
   if (nrow(mat_links)>0) {                                             
   mat_links_df=do.call("rbind",apply(mat_links,1,FUN=function(x) data.frame(expand.grid(x[1],x[2],as.character(unlist(strsplit(x[3]," ")),stringsAsFactors = F)))))
  names(mat_links_df)=c("date","wholinks","links")
  }
                                      
  message("Text message are processed!\n") 
  
  
 
   
  #########################################################################
  # Create daily channel stats
  
  channel_stats=fastChannelstat(channel_obj)
  
  message("Channel stats calculated!\n")
  
  daily_stat=daily_channel_stat(channel_obj)
  
  message("Daily stats calculated!\n")
  
  
  #######################################################################################
  # Create favorite data.frame date,message and authors.
  
  id=which(channel_obj$favoriteCount>0)
  N_favorited=length(id)
  
  if (  N_favorited > 0) {
  ls_favorite_df=data.frame(date=channel_obj$date[id],
                            message=channel_obj$text[id],
                            authors=channel_obj$screenName[id],
                            favoriteCount=channel_obj$favoriteCount[id],
                            is.retweet=channel_obj$isRetweet[id])[order(-as.numeric(channel_obj$favoriteCount[id])),]
                            } else {
                            ls_favorite_df=data.frame(date=NA,
                            message=NA,
                            authors=NA,
                            favoriteCount=0,
                            is.retweet=NA)                    
         }
    
  
  
  ls_message_df=data.frame(data=channel_obj$date,
                           message=channel_obj$text,
                           authors=channel_obj$screenName,
                           retweetCount=channel_obj$retweetCount,
                           is.retweet=channel_obj$isRetweet)[order(-as.numeric(channel_obj$retweetCount)),]
  

  
   
  
  ########################################################################################################################
  
  rank_message_retweeted=data.frame(message=NA,SumretweetCount=NA)
  rank_authors_retweeted=data.frame(message=NA,SumretweetCount=NA)
  table_retweeter=data.frame(author_retweeter=NA,freq=NA)
  rank_authors_favorited=data.frame(authors=NA,SumfavoriteCount=0)
  if (  N_favorited > 0) {
 
  rank_authors_favorited=aggregate(as.numeric(ls_favorite_df$favoriteCount),list(ls_favorite_df$authors),sum)
  rank_authors_favorited=rank_authors_favorited[order(-rank_authors_favorited[,2]),]
   names(rank_authors_favorited)<-c("authors","SumfavoriteCount")
     
  }                                             
                                                      
 
  
 
 ################################################################################
  # Frequency analisys
  
  if ( (length(channel_obj$isRetweet[which(channel_obj$isRetweet==1)]) > 0) && (only_original_tweet==FALSE)) {
    ls_message_df$message=tolower(ls_message_df$message);
    ls_message_df$authors=tolower(ls_message_df$authors);
    rank_message_retweeted=aggregate(as.numeric(ls_message_df$retweetCount),list(ls_message_df$message),sum)
    rank_message_retweeted=rank_message_retweeted[order(-rank_message_retweeted[,2]),]
    names(rank_message_retweeted)<-c("message","SumretweetCount")
    
    rank_authors_retweeted=aggregate(as.numeric(ls_message_df$retweetCount),list(ls_message_df$authors),sum)
    rank_authors_retweeted=rank_authors_retweeted[order(-rank_authors_retweeted[,2]),]
    names(rank_authors_retweeted)<-c("authors","SumretweetCount")
    
    table_retweeter=as.data.frame.array(sort(table(tolower(mat_retweet_df$authors)),decreasing=T))
    table_retweeter=data.frame(authors=rownames(table_retweeter),
                             Freq=as.vector(table_retweeter))
    names(table_retweeter)<-c("author_retweeter","freq")
    rownames(table_retweeter)<-NULL
    table_retweeter=na.omit(table_retweeter);
      
  }
  
  ##########################################################################
  

  table_message=as.data.frame.array(sort(table(tolower(channel_obj$text)),decreasing=T))
  table_message=data.frame(message=rownames(table_message),
                           Freq=as.vector(table_message))
  
  names(table_message)<-c("message","freq")
  
  rownames(table_message)<-NULL

  message("Table_message stats calculated!\n")
  
  
  ##########################################################################
 table_authors=NULL
  if (length(channel_obj$screenName) >1) {
      table_authors=as.data.frame.array(sort(table(tolower(channel_obj$screenName)),decreasing=T))
      table_authors=data.frame(authors=rownames(table_authors),
                           Freq=as.vector(table_authors))
      names(table_authors)<-c("author","freq")
      rownames(table_authors)<-NULL
    table_authors=na.omit(table_authors);
  } 
    
  ##########################################################################
  table_hash=NULL
   if (length(mat_hashtag_df$hashtag) >1) {
  
  table_hash=as.data.frame.array(sort(table(tolower(mat_hashtag_df$hashtag)),decreasing=T))
  table_hash=data.frame(hashtag=rownames(table_hash),
                        Freq=as.vector(table_hash))
  names(table_hash)<-c("hashtag","freq")
  rownames(table_hash)<-NULL
  table_hash=na.omit(table_hash)   
  }
  ##########################################################################
 table_mentions=NULL 
 if (length(mat_mentions_df$whomentioned) >1) {
  
  table_mentions=as.data.frame.array(sort(table(tolower(mat_mentions_df$whomentioned)),decreasing=T))
  table_mentions=data.frame(users=rownames(table_mentions),
                            Freq=as.vector(table_mentions))
  names(table_mentions)<-c("mention","freq")
  rownames(table_mentions)<-NULL
  table_mentions=na.omit(table_mentions)   
   
    } 
  
  ##########################################################################
  
  table_links=NULL                                    
   if (length(mat_links_df$links) >1) {
   
    
  table_links=as.data.frame.array(sort(table(mat_links_df$links),decreasing=T))
  table_links=data.frame(users=rownames(table_links),
                            Freq=as.vector(table_links))
  names(table_links)<-c("links","freq")
  rowable_links=na.omit(table_links)   
   names(table_links)<-NULL
  table_links=na.omit(table_links)   
   }
      
 
  ##########################################################################
  # Graph analitics
 
  rt_graph=NULL
  men_graph=NULL
  
  if ( graph_analisys==TRUE) {
 
  if ((length(which(!is.na(mat_mentions_df$whomention))))>0)  {
  ############################################################################################################
  # Create a mention graph
  mat_mentions_df=na.omit(mat_mentions_df)
  mat_men_graph=data.frame(whomention=as.character(mat_mentions_df$whomention),whomentioned=as.character(mat_mentions_df$whomentioned))
  men_graph = graph.edgelist(as.matrix(mat_men_graph))
  E(men_graph )$weight <- 1
  men_graph <- simplify(men_graph,edge.attr.comb = list(weight = "sum", function(x)length(x)))
 
  }
  
   
  ############################################################################################################
  # Create a retweet graph
  
  
  if (naming!="account_statistics") {
   
           if ((only_original_tweet==FALSE) & (length(which(!is.na(mat_retweet_df$authors)))>0)) 
                                       {
                                      mat_retweet_df=na.omit(mat_retweet_df)
                                      rt_graph=data.frame(whoretweet=as.character(mat_retweet_df$authors),whomentioned=as.character(mat_retweet_df$retweeted_users))
                                      rt_graph = graph.edgelist(as.matrix(rt_graph))
                                      E(rt_graph )$weight <- 1
                                      rt_graph <- simplify(rt_graph,edge.attr.comb = list(weight = "sum", function(x)length(x)))
                                      }
                                     }

                  
  
                  message("Graphs of channel are done!\n")
  }
  ############################################################################################################
  # Get corpus and termdocfrequency matrix as qdap object
  
  corpus=NULL
  word_freq_matr=NULL
  
  if ( (corpus_analisys==TRUE)) 
                               {
                               stopwords="";
                               if ( stopword ==T) {stopwords=tm::stopwords(kind = corpus_lang)}
                               corpus=getCorpus(channel_obj$text,hashtag=corpus_hashtag,stopwords)
                               word_freq_matr<- as.data.frame(as.wfm(TermDocumentMatrix(corpus, control = list(wordLengths = c(2, Inf),stopword=T))))
                               word_freq_matr=data.frame(Words=row.names(word_freq_matr),Freq=word_freq_matr$content)
                               word_freq_matr=word_freq_matr[rev(order(word_freq_matr$Freq)),]
                               message("Corpus analisys of channel are done!\n")
                               }
  
  
  
  ########################################################################################
  
  res=list(channel_stat=channel_stats,
           daily_stat=daily_stat,
           table_hash=table_hash,
           table_message=data.frame(table_message,stringsAsFactors = F),
           table_mentions=data.frame(table_mentions,stringsAsFactors = F),
           table_links=data.frame(table_links,stringsAsFactors = F),
           table_authors=data.frame(table_authors,stringsAsFactors = F),
           table_authors_retweeter=data.frame(table_retweeter,stringsAsFactors = F),
           retweeted_authors=data.frame(rank_authors_retweeted,stringsAsFactors = F),
           favorited_authors=data.frame(rank_authors_favorited,stringsAsFactors = F),
           favorite_message=data.frame(ls_favorite_df,stringsAsFactors = F),
           rank_message_retweeted=data.frame(rank_message_retweeted,stringsAsFactors = F),
           top_message=table_message[1:Ntop,],
           top_authors=table_authors[1:Ntop,],
           top_hash=table_hash[1:Ntop,],
           top_mentions=table_mentions[1:Ntop,],
           top_authors_retweeter=table_retweeter[1:Ntop,],
           topfull_authors_retweeted=rank_authors_retweeted[1:Ntop,],
           topfull_message_retweeted=rank_message_retweeted[1:Ntop,],
           graph_retweet=rt_graph,
           graph_mentions=men_graph,
           channel_data=channel_obj,
           account_stats=NULL,
           channel_corpus=corpus,
           word_freq_matr=word_freq_matr,
           users_data=NULL)
  
  if (naming=="rtweet")     { res$users_data=attr(res$channel_data,"users")
                              res$channel_data=data.frame(res$channel_data[,names(res$channel_data)]);
                              }
                                                                                          
  if (naming=="account_statistics") 
    
  { stats_activity=aggregate(channel_obj[,5:22], list(channel_obj$data), sum)
    names(stats_activity)[1]="data"
    rownames(stats_activity)=stats_activity$data
    res$account_stats=stats_activity 
  }
  
  return(res)
}
