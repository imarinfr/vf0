\name{ghpostd}
\alias{ghpostd}
\title{general height estimated as the rankRef-th ranked TD location}
\description{
gets the general height estimated as the rankRef-th ranked TD location
}
\usage{ghpostd( td, correction = FALSE, rankRef = c( "default" ) )}
\arguments{
  \item{td}{vf-object with total-deviation values}
  \item{correction}{obtain general height as difference from mean normal "85th" percentile TD value? Default is \code{FALSE}}
  \item{rankRef}{the rank position to take as reference for general height. Default is "default". For "default" it looks at the type of visual field, \code{p24d2} or \code{p30d2}, \code{fullt} or \code{sitas}, to get which is the position that corresponds, roughly with the 85th percentile}
}
\details{calculates the general height estimated as the pos-th ranked TD location}

\value{returns the subject's estimated general height}

\seealso{\code{\link{pdval}}}

\examples{
td <- tdval( vf91016right[15,] )
ghseventh <- ghpostd( td )
}

\author{Ivan Marin-Franch}