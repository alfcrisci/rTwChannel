#' notes_outputs
#'
#' @description Export result from statistical notes object. Require daily stat object from channel_analytic function.
#'
#' @param  daily_stat  data.frame Daily statistics obtained by channel_analytic
#' @param  stat_notes  list Statistical object obtained from stat_notes function.
#' @param  prefix_file character Prefix for file names outputs
#' @param  na_string character String value for missing data.
#' @param  filecsv logical export csv files.
#' @param  html logical export html sjPlot files.
#' @param  excel logical export excel files.
#' @param  daily_unique logical export daily stats of unique.
#' @return A list of daily statistics  data, the unique one and full aggregate stats.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  annotation
#'
#'
#'
#' @export
#'
#'

notes_outputs=function(daily_stat,stat_notes,prefix_file="notes",na_string="",filecsv=TRUE,html=TRUE,excel=TRUE,daily_unique=FALSE){
  options(java.parameters = "-Xmx4g")
  
  
  daily_stat_df = data.frame(date = as.Date(daily_stat$date))
  daily_stat_df_unique = data.frame(date = as.Date(daily_stat$date))
  stat_full = data.frame(category = NA,Nfreq = NA,N_unique = NA)
  
  for ( i in seq_along(stat_notes)) {
    
    stat_notes[[i]]$df$date=as.Date(stat_notes[[i]]$df$date)
    stat_notes[[i]]$df_unique$date=as.Date(stat_notes[[i]]$df_unique$date)
    
    daily_stat_df=merge(daily_stat_df,stat_notes[[i]]$df,all.x=T)
    
    daily_stat_df_unique=merge(daily_stat_df_unique,stat_notes[[i]]$df_unique,all.x=T)
    namecat=names(stat_notes[[i]]$df)
    stat_full[i,]=c(names(stat_notes[[i]]$df)[2],stat_notes[[i]]$N,stat_notes[[i]]$N_sumuniquedate)
  }
  res=list();
  
  res$daily_stat_df=daily_stat_df
  res$daily_stat_df_unique=daily_stat_df_unique
  res$stat_full=stat_full[order(as.numeric(stat_full$Nfreq),decreasing = T),]
  
  if ( filecsv == TRUE) {
    write.csv(res$daily_stat_df,paste0(prefix_file,"_daily_stat.csv"),na=na_string,row.names = F)
    if ( daily_unique == TRUE) {
    write.csv(res$daily_stat_df_unique,paste0(prefix_file,"_daily_stat_unique.csv"),na=na_string,row.names = F)
    }
    write.csv(res$stat_full,paste0(prefix_file,"_stat_full.csv"),na=na_string,row.names = F)
  }
  if ( html == TRUE) {
    sjPlot::sjt.df(res$daily_stat_df,stringVariable = "Daily_stat",describe=FALSE,alternateRowColors = TRUE,file=paste0(prefix_file,"_daily_stat.html"))
    if ( daily_unique == TRUE) {
      sjPlot::sjt.df(res$daily_stat_df_unique,stringVariable = "Daily_stat_unique",describe=FALSE,alternateRowColors = TRUE,file=paste0(prefix_file,"_daily_stat_unique.html"))
    }
    sjPlot::sjt.df(res$stat_full,stringVariable =  "Full_stat",describe=FALSE,alternateRowColors = TRUE,file=paste0(prefix_file,"_stat_full.html"))
    
  }
  
  if ( excel == TRUE) {
    
    XLConnect::writeWorksheetToFile(paste0(prefix_file,"_daily_stat.xls"), res$daily_stat_df, sheet=paste0("Daily_stat"))
    if ( daily_unique == TRUE) {
      XLConnect::writeWorksheetToFile(paste0(prefix_file,"_daily_stat_unique.xls"), res$daily_stat_df_unique, sheet=paste0("Daily_stat_unique"))
    }
    XLConnect::writeWorksheetToFile(paste0(prefix_file,"_stat_full.xls"), res$stat_full, sheet=paste0("Full_stats"))
    
  }
  
  return(res)
  
}
