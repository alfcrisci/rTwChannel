#' create_full_notes
#'
#' @description Merge keylist  objects with  annotations into a comprehensive one. Need a vector of label note's names.
#'
#' @param  obj_notes  character Name of channel in repository
#' @param  label  logical  Annotation category
#' @return A qdap keylist object with key and category
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  match
#'
#'
#'
#' @export
#'
#'

create_full_notes=function(obj_notes,label=FALSE) {
  notes=do.call('rbind',obj_notes)
  notes=unique(notes)
  if (label==TRUE) {
    notes$y="Annotated"
  }
  return(notes)
}
