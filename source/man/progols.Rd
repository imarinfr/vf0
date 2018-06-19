\name{progols}
\alias{progols}
\title{progression by linear ordinary least squares}
\description{makes a linear regression with projection in the future by projyears years}
\usage{progols( tdate, index, projyears = 5,
         xlab = "years from first visit", ylab = "md",
         txtfont = "sans", pointsize = 10, cex = 1 )}
\arguments{
  \item{tdate}{visit date}
  \item{index}{index measured at the corresponding age}
  \item{xlab}{label for x-axis. Default is \code{age}}
  \item{ylab}{label for y-axis. Default is \code{md}}
  \item{projyears}{Years for projection. Default is \code{5}}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{sans}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{10}}
  \item{cex}{a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default. Default is \code{1}}
}

\examples{
  vfi <- vfindex( vf91016right )
  progols( vfi$tdate, vfi$mvfi, ylab = "vfi" )
}

\author{Ivan Marin-Franch}