\name{Botanical-info}
\alias{taxon.author}
\alias{taxon.citation}
\alias{taxon.family}
\alias{is.accepted}
\alias{lower.taxa}
\alias{taxon.synonyms}
\title{
Retrieve the taxonomical information of a botanical name.
}
\description{
Retrieve the taxonomical information of a botanical name from the Flora do Brasil website.
}
\usage{
taxon.author(spp)
taxon.citation(spp)
taxon.family(spp)
lower.taxa(spp)
taxon.synonyms(spp)
}
\arguments{
  \item{spp}{
A character string containing a botanical name to be looked up on Flora do Brasil.
}
}
\value{
A character string or logical value.
}
\note{
Please note that the purpose of these functions is to make it easier to gather taxonomical information in bulk. It's highly recommended that you always verify the results on Flora do Brasil.
}
\references{
http://floradobrasil.jbrj.gov.br/jabot/
}
\author{
Gustavo Carvalho (gustavo.bio@gmail.com)
}
\seealso{
\code{\link{is.accepted}}
\code{\link{suggest.names}}
\code{\link{get.taxon.info}}
}
\examples{
\dontrun{
taxon.author("Miconia albicans")
taxon.author("Myrcia")
taxon.citation("Myrcia")
taxon.family("Coffea arabica")
taxon.family("Anadenanthera")
lower.taxa("Melastomataceae")
lower.taxa("Ouratea")
taxon.synonyms("Myrcia guianensis")
}
}
\keyword{author}
\keyword{autor}
\keyword{family}
\keyword{familia}
\keyword{lower.taxa}
\keyword{synonyms}
\keyword{synonymy}
\keyword{sinonimo}
\keyword{sinonimia}
\keyword{citation}
\keyword{cite}
\keyword{citar}