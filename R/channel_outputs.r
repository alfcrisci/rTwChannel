#' channel_outputs
#'
#' @description Export result from statistical of \code{channel_analytic} function outputs.
#'
#' @param  stat_obj  list  output obtained by channel_analytic
#' @param  param  character Name of statistics or object of channel_analytic object.
#' @param  suffix_file character Prefix for file names outputs
#' @param  na_string character String value for missing data.
#' @param  filecsv logical export csv files.
#' @param  html logical export html sjPlot files.
#' @param  excel logical export excel files.
#' @return Object extracted from the list of output given by \code{channel_analytic} object.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  annotation
#'
#'
#'
#' @export
#'
#'

channel_outputs=function(stat_obj, param="channel_stat", suffix_file="LIG", na_string="",filecsv=FALSE, html=FALSE,excel=TRUE){
   options(java.parameters = "-Xmx4g" )
  param_list=list(channel_stat=1,
                  table_message=2,
                  table_hash=3,
                  table_mentions=4,
                  table_authors=5,
                  table_replies=6,
                  table_authors_retweeted=7,
                  table_authors_retweeter=8,
                  rank_authors_retweet=9,
                  rank_message_retweet=10,
                  top_message=11,
                  top_authors=12,
                  top_hash=13,
                  top_mentions=14,
                  top_replies=15,
                  top_authors_retweeted=16,
                  top_authors_retweeter=17,
                  topfull_authors_retweeted=18,
                  topfull_message_retweeted=19,
                  daily_stat=20,
                  authors_date=21,
                  links_date=22,
                  hash_date=23,
                  tag_date=24,
                  unique_message=25,
                  unique_authors=26,
                  unique_hash=27,
                  unique_mentions=28,
                  unique_authors_retweeted=29,
                  unique_authors_retweeter=30,
                  uniquefull_authors_retweeted=31,
                  uniquefull_message_retweeted=32,
                  graph_retweet_df=33,
                  graph_hash_df=34,
                  graph_mentions_df=35,
                  authors_favorite=36,
                  favorite_message_top=37,
                  channel_data=38,
                  account_stats=39
  )
  
  res=stat_obj[[as.numeric(param_list[param])]]
  
  if (param=="daily_stat") { res[,6:8]=round(res[,6:8],1) }
  
  if ( filecsv == TRUE) {
    write.csv(res,paste0(suffix_file,"_",param,".csv"),na=na_string,row.names = F)
    
  }
  
  if ( html == TRUE) {
    
    sjPlot::sjt.df(res,stringVariable = param,describe=FALSE,alternateRowColors = TRUE,file=paste0(param,"_",suffix_file,".html"))
    
  }
  
  if ( excel == TRUE) {
    
    XLConnect::writeWorksheetToFile(paste0(suffix_file,"_",param,".xls"), res, sheet=paste0(suffix_file))
    
  }
  
  return(res)
  
}
