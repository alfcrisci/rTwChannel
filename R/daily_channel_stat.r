#' daily_channel_stat
#'
#' @description Function to compute daily summary stats of a twitter channel.
#'
#' @param  x  Data.frame Dataset of tweets
#' @param  check_duplicates Logical Check if exist duplicated tweets. 
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  channel, stats, twitter channel.
#' @return daily_channel_final Data.frame of daily stats performed by \code{fastChannelstat}
#'
#'
#' @export
#'
#'
#'
daily_channel_stat=function(x,check_duplicates=T,...) 
  { res_d=list();
    datetemp=unique(x$date);

    for ( i in 1:length(datetemp)) {    
          temp=x[which(x$date == datetemp[i]),]
          res_d[[i]]=suppressWarnings(data.frame(date=datetemp[i],fastChannelstat(temp,check_duplicates=T,...)))
  
          }

    res_daily=do.call("rbind",res_d)
    date_full=data.frame(date=seq(range(res_daily$date)[1],range(res_daily$date)[2],by=1))
    daily_channel_final=merge(date_full,res_daily,all.x=T)
    daily_channel_final$ini_date_full=NULL
    daily_channel_final$end_date_full=NULL
    return(daily_channel_final)
}
