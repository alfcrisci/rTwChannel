#' channel_annotation_names
#'
#' @description Retrieve the names of categorical annotations from a github public repository related to annotation regarding the channel.
#'
#' @param  channel  character  Name of channel in repository
#' @param  target  character  Target of annotation ( user,hashtag, geonames or other)
#' @param  repo  character  Address of repository where annotation key file are hosted.
#' @return x    data.frame   A data.frame with key and category
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  names, annotations
#'
#'
#'
#' @export
#'
#'

channel_annotation_names=function(channel="allertameteoLIG",
                                  target="hashtag",
                                  repo="https://raw.github.com/alfcrisci/twchannels_notations/master/")
{
  web_point=paste0(repo,channel,"/",channel,"_notation_names.csv")
  x <- try(read.csv(curl::curl(web_point)))
  if ( class(x) != "data.frame") { stop("Verify category and target and web connection!")}
  id=as.character(x[which(x$target==target),1]) 
  return(id)
  
}





