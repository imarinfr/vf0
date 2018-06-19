\name{vfplot2}
\alias{vfplot2}
\title{visual field plot}
\description{plots location-specific sensitivity thresholds, or TD or PD values at each location of the visual field with the corresponding color code for the probability map}
\usage{vfplot2( vf, plotType, notSeenAsBlack = TRUE, newWindow = FALSE,
        txtfont = "serif", pointsize = 7, width = 6,
        xminmax = 29, yminmax = 29,
        outerSymbol = "circles", innerSymbol = "circles",
        outerSize = 1, innerSize = 1,
        outerInch = 0.14, innerInch = 0.08,
        lengthLines = 4.25, thicknessLines = 2 )}
\arguments{
  \item{vf}{\code{\link{vf}}-object with sensitivity thresholds, or TD or PD values}
  \item{plotType}{Type of plot to show. It can be visual sensitivities (\code{vf}), total-deviation values (\code{td}), or pattern-deviation values (\code{pd})}
  \item{notSeenAsBlack}{whether to plot non-seen as black. Default is \code{TRUE}}
  \item{newWindow}{whether to generate a new window for the plot. This becomes important when working with RStudio. Default value is \code{TRUE}}
  \item{txtfont}{font face of the text with visual-sensitivity values. Default is \code{Helvetica}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{width}{width of the window. height is calculated using the width, xminmax, and yminmax. Default is \code{6}}
  \item{xminmax}{minimum and maximum limits on the x axis, in degrees of visual angle. Default is \code{29}}
  \item{yminmax}{minimum and maximum limits on the y axis, in degrees of visual angle. Default is \code{29}}
  \item{outerSymbol}{The outer symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{innerSymbol}{The inner symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{outerSize}{Relative size of the outer symbol. Default is \code{1}}
  \item{innerSize}{Relative size of the inner symbol. Default is \code{1}}
  \item{outerInch}{Maximum size of the outer symbol in inches. Default is \code{0.14}}
  \item{innerInch}{Maximum size of the inner symbol in inches. Default is \code{0.08}}
  \item{lengthLines}{length of the lines denoting the orientation of the RNFL bundle. Default is \code{4.25}}
  \item{thicknessLines}{thickness of the lines denoting the orientation of the RNFL bundle. Default is \code{2}}
}
\details{This function 
The vfplot function operates on a single row of visual fileds (vf). Depending on the plot type it will generate a plot representing the sensitivity at each location. The  color scheme elucidates the degree of sensitivity at that location. Locations with zero sensitivity are shown in black. Blind spots have been excluded fronm the plot}

\seealso{\code{\link{vfplotloc2}}}

\examples{
  vfplot2( vf91016right[15,], plotType = "td" )
}

\author{Chaitanya Khadilkar, Ivan Marin-Franch}