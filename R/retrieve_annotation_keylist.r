#' retrieve_annotation_keylist
#'
#' @description Retrieve a keylist qdap object with  annotations from a github public repository related to a channel.
#'
#' @param  channel  character  Name of channel in repository
#' @param  category  character  Annotation category
#' @param  target  character  Target of annotation ( user,hashtag, geonames or other)
#' @param  repo  character  Address of repository where annotation key file are hosted.
#' @return hash_temp  data.frame   A qdap keylist object with key and category
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  match
#'
#'
#'
#' @export
#'
#'

retrieve_annotation_keylist=function(channel="allertameteoLIG",
                                     category="emergency",
                                     target="hashtag",
                                     repo="https://raw.github.com/alfcrisci/twchannels_notations/master/")
{
  web_point=paste0(repo,channel,"/",channel,"_hash_",category,".csv")
  if (target=="authors") {web_point=gsub("_hash","",web_point)}
  if (target=="geonomi") {web_point=gsub("_geonomi","",web_point)}
  x <- try(read.csv(curl::curl(web_point)))
  if ( class(x) != "data.frame") { stop("Verify category and target and web connection!")}
  if (target=="authors") { x$authors_full=tolower(x$authors_full)} else {x$keywords=tolower(x$keywords)};
  hash_temp=qdapTools::hash(x)
  return(hash_temp)
  
}
