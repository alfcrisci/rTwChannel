% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_full_notes.r
\name{retrieve_full_notes}
\alias{retrieve_full_notes}
\title{retrieve_full_notes}
\usage{
retrieve_full_notes(
  channel,
  label_notes,
  category,
  save = TRUE,
  filerds = "notes.rds"
)
}
\arguments{
\item{channel}{list  Name of channel in repository}

\item{label_notes}{character  Vector of annotation category}

\item{category}{character  Target of annotation ( user, hashtag, geonames or other)}

\item{save}{logical  Save the files in rds format}

\item{filerds}{character  Name of file of rds}
}
\value{
Return a qdap hash keylist  with key and category
}
\description{
retrieve keylist to perform a build of qdap object from repository.
}
\author{
Istituto per la Bioeconomia Firenze Italy  Alfonso Crisci \email{alfonso.crisci@ibe.cnr.it}
}
\keyword{annotation}
