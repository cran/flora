\name{flora-package}
\alias{flora-package}
\alias{flora}
\docType{package}
\title{
Taxonomic information from the Flora do Brasil website
}
\description{
Functions to retrieve taxonomical information on flowering plants from the Flora do Brasil website and suggest valid botanical names in place of misspeled ones.
}
\details{
\tabular{ll}{
Package: \tab flora\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-07-03\cr
License: \tab GPL\cr
}
}
\author{
Gustavo Carvalho <gustavo.bio@gmail.com>
}
\references{
http://floradobrasil.jbrj.gov.br/
}
\keyword{flora}
\examples{
\dontrun{
taxon.author("Coffea arabica")
taxon.citation("Coffea")
taxon.family("Coffea arabica")
taxon.synonyms("Myrcia guianensis")
suggest.names("Fabaceaea")
}
}