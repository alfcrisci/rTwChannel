#' stat_notes
#'
#' @description Create a list statistical object by using qdap keylist
#'
#' @param  x  character  Vector of word to compute statistics of notes
#' @param  notes  qdap hash object  Vector of annotation category
#' @return A list of statistics for each category of annotation present in notes.
#' @author  Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@@ibe.cnr.it}
#' @keywords  annotation
#'
#'
#'
#' @export
#'
#'

stat_notes=function (x, notes) 
{
    options(warn = -1)
    x[,2] = tolower(x[,2])
    x[,2] = gsub("^@", "", x[, 2])
    label_notes = notes$y[1]
    x_unique = unique(x[,1:2])
    terms=x[,2]
    terms_unique=x_unique[,2]
    mapnotes=data.frame(data=x[,1],note_target=terms,authors=x$authors,noteterms=hash_look(terms,notes))
    mapnotes_unique =data.frame(data=x_unique[,1],note_target=terms_unique,noteterms=hash_look(terms_unique,notes))
    mapnotes = na.omit(mapnotes)
    mapnotes_unique = na.omit(mapnotes_unique)
    freq_day = as.data.frame(as.matrix(table(mapnotes$data)))
    names(freq_day) = c("N")
    freq_day$data = rownames(freq_day)
    freq_day_unique = as.data.frame(as.matrix(table(mapnotes_unique$data)))
    names(freq_day_unique) = c("N")
    freq_day_unique$data = rownames(freq_day_unique)
    res = list()
    res$df = data.frame(data=freq_day$data,N= freq_day$N)
    res$df_unique = data.frame(data=freq_day_unique$data,N=freq_day_unique$N)
    names(res$df) <- c("date", label_notes)
    names(res$df_unique) <- c("date", label_notes)
    res$N = nrow(mapnotes)
    res$N_sumuniquedate = nrow(mapnotes_unique)
    res$mapnotes= mapnotes
    res$mapnotes_unique= mapnotes_unique
    return(res)
    options(warn = 0)
}
