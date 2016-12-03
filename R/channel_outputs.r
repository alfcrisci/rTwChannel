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
#' @importFrom XLConnect writeWorksheetToFile
#' @importFrom sjPlot sjt.df
#' @export
#'
#'

channel_outputs=function(stat_obj, param="channel_stat", suffix_file="LIG", na_string="",filecsv=FALSE, html=FALSE,excel=TRUE)
  {
                
   options(java.parameters = "-Xmx4g" )
   param_list=list(channel_stat=1,
                  daily_stat=2,
                  table_hash=3,
                  table_message=4,
                  table_mentions=5,
                  table_links=6,
                  table_authors=7,
                  table_authors_retweeter=8,
                  retweeted_authors=9,
                  favorited_authors=10,
                  favorite_message=11,
                  rank_message_retweeted=12,
                  top_message=13,
                  top_authors=14,
                  top_hash=15,
                  top_mentions=16,
                  top_authors_retweeter=17,
                  topfull_authors_retweeted=18,
                  topfull_message_retweeted=19,
                  graph_retweet=20,
                  graph_mentions=21,
                  channel_data=22,
                  account_stats=23,
                  channel_corpus=24,
                  word_freq_matr=25,
                  users_channel=26 
                  )
  
  
  res=stat_obj[[as.numeric(param_list[param])]]
  
  if ( filecsv == TRUE) {
    write.csv(res,paste0(suffix_file,"_",param,".csv"),na=na_string,row.names = F)
    
  }
  
  if ( html == TRUE) {
    
    sjt.df(res,stringVariable = param,describe=FALSE,alternateRowColors = TRUE,file=paste0(param,"_",suffix_file,".html"))
    
  }
  
  if ( excel == TRUE) {
    
    writeWorksheetToFile(paste0(suffix_file,"_",param,".xls"), res, sheet=paste0(suffix_file))
    
  }
  
  return(res)
  
}
