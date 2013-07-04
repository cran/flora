\name{fmt.taxa}
\alias{fmt.taxa}
\title{
Present taxonomical information in a more readable way
}
\description{
This function converts a flora object to a data frame
}
\usage{
fmt.taxa(taxa)
}
\arguments{
  \item{taxa}{
A taxa object as returned by \code{\link{get.taxon.info}}.
}
}
\value{
A data frame.
}
\references{
http://floradobrasil.jbrj.gov.br/
}
\author{
Gustavo Carvalho (gustavo.bio@gmail.com)
}
\seealso{
\code{\link{get.taxon.info}}
}
\examples{
\dontrun{
species <- get.taxon.info(c("Myrcia bella", "Myrcia lingua", "Myconia"))
fmt.taxa(species)
}
}

\keyword{format}
