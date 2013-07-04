\name{get.taxon.info}
\alias{get.taxon.info}
\title{
Retrieve information of a list of botanical names.
}
\description{
Retrieve taxonomical information on flowering plants that occur in Brazil from the Flora do Brasil website.
}
\usage{
get.taxon.info(taxa, verbose = FALSE, pbar = TRUE)
}
\arguments{
  \item{taxa}{
A character string containing one or more botanical names.
}
  \item{verbose}{
A logical value indicating whether R detailed information on the data requests to Flora do Brasil should be shown.
}
  \item{pbar}{
A logical value indicating if a progress bar should be shown.
}
}
\details{
This function gets the status, family, author, and synonyms of a botanical name. It can also suggest a valid name if the one used as input is misspelled. You must have a working connection to the internet for it to work properly. If either your connection or Flora do Brasil crash whilst processing a list of botanical names, \code{get.taxon.info} will try to return an object with your unfinished list. You can use this object later as input in \code{get.taxon.info} to resume the search. This process can be repeated as many times as needed. Since \code{get.taxon.info} relies heavily on data scrapped from a website, changes upstream can lead to broken functions in \code{flora}. Always check if you have the latest version of the package. 
}
\value{
  An invisible list with the following fields:
  \item{tax.rank}{A character string of the taxonomical rank of the botanical name or a \code{flora} object with unprocessed names. It can contain, families, genera, species, varieties (var.), subspecies (subsp.), and hybrids.}
  \item{id}{A characther string of the identification of the botanical name on Flora do Brasil}
  \item{accepted}{A logical value indicating the status of the botanical name}
  \item{accepted.name}{A character string of the accepted name}
  \item{family}{A character string of the family}
  \item{name}{A character string of the searched botanical name}
  \item{author}{A character string of the author}
  \item{lower.taxa}{A character string of all accepted taxa of a lower rank}
  \item{synonyms}{A character string containing all the synonyms of a botanical name}
  \item{found}{A logical value indicating whether the name was found or not}
  \item{suggestion}{A character string of a name suggestion}
  \item{citation}{A character string indicating how to cite Flora do Brasil}
  \item{processed}{A logical value indicating whether the botanical name was processed}
}
\note{
Please note that the purpose of these functions is to make it easier to gather information in bulk. It is still highly recommended that you always verify the results on Flora do Brasil. Please cite them accordingly.
}
\references{
http://floradobrasil.jbrj.gov.br/
}
\author{
Gustavo Carvalho (gustavo.bio@gmail.com)
}
\examples{
\dontrun{
get.taxon.info(c("Myrcia lingua", "Myrcia bella", "Miconia", "Rubiaceae"))
}
}
\keyword{taxa}
\keyword{taxon}
