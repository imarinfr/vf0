\name{vflayout2_legoplot}
\alias{vflayout2_legoplot}
\title{legoplot visualization for change in sensitivities over time of visual fields}
\description{
displays a legoplot visualization for change in sensitivities over time of visual fields
}
\usage{vflayout2_legoplot( vf, grp = 3, pwidth = 8.27, pheight = 11.69,
                               margin = 0.25, filename = NULL,
                               ffamily = "serif", sizetxt = 12,
                               sizetxtSmall = 8,
                               ffamilyvf = "serif", pointsize = 6,
                               txtcolorlego = "red", pointsizelego = 16,
                               outerSymbol = "circle", outerInch = 0.12,
                               innerSymbol = "circle", innerInch = outerInch / 1.9,
                               inch2axisunits = 12.528,
                               lengthLines = 0, thicknessLines = 0,
                               outerSymbollego = "square", outerInchlego = 0.36,
                               innerSymbollego = "circle", innerInchlego = 0.16 )}
\arguments{
  \item{vf}{\code{\link{vf}}-object with sensitivity thresholds}
  \item{grp}{how many visual fields to group. Default is \code{3}}
  \item{pwidth}{width of the page (in inches). Default is \code{8.27}}
  \item{pheight}{height of the page (in inches). Default is \code{11.69}}
  \item{margin}{margins of the page (in inches). Default is \code{0.25}}
  \item{filename}{file name to save the printout as pdf. If it is saved to pdf, it won't be displayed in the screen. Default is \code{NULL}.}
  \item{ffamily}{Family font for the text in the from. Default is \code{"Helvetica"}}
  \item{sizetxt}{Text size for main text in the form. Default is \code{12}}
  \item{sizetxtSmall}{Text size for details in the form. Default is \code{8}}
  \item{ffamilyvf}{Font family for text in lego plots. Default is \code{"Times"}}
  \item{pointsize}{Point size for text in lego plots. Default is \code{6}}
  \item{txtcolorlego}{Text color for lego plots. Default is \code{"red"}}
  \item{pointsizelego}{Point size for text of lego plots. Default is \code{16}}
  \item{outerSymbol}{Type of outer symbol. Default is \code{"circle"}}
  \item{outerInch}{Inches of outer symbol. Default is \code{0.12}}
  \item{innerSymbol}{Type of inner symbol. Default is \code{"circle"}}
  \item{innerInch}{Inches of inner symbol. Default is \code{outerInch / 1.9}}
  \item{inch2axisunits}{Conversion factor inches to axis units. Default is \code{12.528}}
  \item{lengthLines}{Length of lines. Default is \code{0}}
  \item{thicknessLines}{Line thickness. Default is \code{0}}
  \item{outerSymbollego}{Type of outer lego symbol. Default is \code{"square"}}
  \item{outerInchlego}{Inches of outer lego symbol. Default is \code{0.36}}
  \item{innerSymbollego}{Type of inner lego symbol. Default is \code{"circle"}}
  \item{innerInchlego}{Inches of inner lego symbol. Default is \code{0.16}}
}

\seealso{\code{\link{vflayout2}}, \code{\link{vflayout2_poplr}}}

\examples{
  \donttest{
    vflayout2_legoplot( vf91016right )
  }
}

\author{Ivan Marin-Franch}