\name{vflayout2}
\alias{vflayout2}
\title{printout with results for the visual field}
\description{creates a printout with results for the visual field. This includes sensitivity threshold values with a gray scale plot, and TD and PD values with color coding for the probability map}
\usage{vflayout2( vf, pwidth = 8.27,
                      pheight = 11.69, margin = 0.25,
                      filename = NULL,
                      ffamily = "serif", sizetxt = 12,
                      sizetxtSmall = 8, ffamilyvf = "serif",
                      pointsize = 5,
                      outerSymbol = "circle", outerInch = 0.13,
                      innerSymbol = "circle", innerInch = outerInch / 1.9,
                      lengthLines = 0, thicknessLines = 0 )}
\arguments{
  \item{vf}{\code{\link{vf}}-object with sensitivity thresholds}
  \item{pwidth}{width of the page (in inches). Default is \code{8.27}}
  \item{pheight}{height of the page (in inches). Default is \code{11.69}}
  \item{margin}{margins of the page (in inches). Default is \code{0.25}}
  \item{filename}{file name to save the printout as pdf. If it is saved to pdf, it won't be displayed in the screen. Default is \code{NULL}.}
  \item{ffamily}{Family font for the text in the from. Default is \code{"Helvetica"}}
  \item{sizetxt}{Text size for main text in the form. Default is \code{12}}
  \item{sizetxtSmall}{Text size for details in the form. Default is \code{8}}
  \item{ffamilyvf}{Family font for the number in the vfpots (TD, PD, etc). Default is \code{"Times"}}
  \item{pointsize}{Point size for numbers in the vfplots (TD, PD, etc). Default is \code{5}}
  \item{outerSymbol}{Type of outer symbol. Default is \code{"circle"}}
  \item{outerInch}{Inches of outer symbol. Default is \code{0.13}}
  \item{innerSymbol}{Type of inner symbol. Default is \code{"circle"}}
  \item{innerInch}{Inches of inner symbol. Default is \code{outerInch / 1.9}}
  \item{lengthLines}{Length of lines. Default is \code{0}}
  \item{thicknessLines}{Line thickness. Default is \code{0}}
}

\seealso{\code{\link{vflayout2ghr}}, \code{\link{vfplot2}}, \code{\link{vfplotloc2}}}

\examples{
  \donttest{
    vflayout2( vf91016right[15,] )
  }
}

\author{Ivan Marin-Franch}