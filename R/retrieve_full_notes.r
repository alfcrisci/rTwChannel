#' retrieve_full_notes
#'
#' @description retrieve keylist to perform a build of qdap object from repository.
#'
#' @param  channel  list  Name of channel in repository
#' @param  label_notes  character  Vector of annotation category
#' @param  category  character  Target of annotation ( user, hashtag, geonames or other)
#' @param  save  logical  Save the files in rds format
#' @param  filerds  character  Name of file of rds
#' @return Return a qdap hash keylist  with key and category
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  annotation
#'
#'
#'
#' @export
#'
#'

retrieve_full_notes=function(channel, label_notes,category,save=TRUE,filerds="notes.rds" ) {
   res_notes=list()
  for ( i in 1:length(label_notes)) {
    res_notes[[i]]=retrieve_annotation_keylist(channel=channel,category=label_notes[i],target=target)
  }
  if( save==TRUE) {saveRDS(res_notes,filerds)}
  return(res_notes)
}

