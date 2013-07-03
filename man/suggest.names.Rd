\name{suggest.names}
\alias{suggest.names}
\title{
Suggest valid botanical names
}
\description{
This function suggest valid botanical names using data from the Flora do Brasil website.
}
\usage{
suggest.names(spp)
}
\arguments{
  \item{spp}{
A character string containing botanical name.
}
}
\value{
Either a character string of a valid name suggestion or \code{NA}.
}
\references{
http://floradobrasil.jbrj.gov.br/
}
\author{
Gustavo Carvalho
}
\note{
The code for this function was borrowed from the \code{RecordLinkage} package.
}
\examples{
suggest.names("Mircia lingua")
}
\keyword{suggestion}
