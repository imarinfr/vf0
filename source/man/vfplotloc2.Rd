\name{vfplotloc2}
\alias{vfplotloc2}
\title{visual field data plot}
\description{plots location-specific data at each location of the visual field. It is the function called by \code{\link{vfplot}}}
\usage{vfplotloc2( vals, eye, patternMap, outerColor, innerColor = NULL, bs = NULL,
           axesCol = "black", txtfont = "mono", pointsize = 7,
           txtcolor = NULL, xminmax = 29, yminmax = 29, outerSymbol = "circles",
           innerSymbol = "circles", outerSize = 1, innerSize = 1,
           outerInch = 0.2, innerInch = 0.1, lengthLines = 0,
           thicknessLines = 0, outerBorderColor = NULL, innerBorderColor = NULL,
           outerBorderThickness = 2, innerBorderThickness = 2 )}
\arguments{
  \item{vals}{sensitivity threshold, TD or PD values, or other location-specific values}
  \item{eye}{eye being tested}
  \item{patternMap}{locations where to plot symbols, the TD and PD values}
  \item{outerColor}{colors to use for the outer symbols in the graph}
  \item{innerColor}{colors to use for the outer symbols in the graph. Default is \code{NULL}, meaning white}
  \item{bs}{location of the blind spot. Default is \code{NULL} meaning do not show blind spot}
  \item{axesCol}{color of the axes. Default is \code{black}}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{txtcolor}{color of text. Default is \code{black}}
  \item{xminmax}{minimum and maximum limits on the x axis}
  \item{yminmax}{minimum and maximum limits on the y axis}
  \item{outerSymbol}{The outer symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{innerSymbol}{The inner symbol at all locations. Can be any of circles, squares, rectangles, stars. Default is \code{circle}}
  \item{outerSize}{Relative size of the outer symbol. Default is \code{1}}
  \item{innerSize}{Relative size of the inner symbol. Default is \code{1}}
  \item{outerInch}{Maximum size of the outer symbol in inches. Default is \code{0.2}}
  \item{innerInch}{Maximum size of the inner symbol in inches. Default is \code{0.1}}
  \item{lengthLines}{length of the lines denoting the orientation of the RNF bundle}
  \item{thicknessLines}{thickness of the lines denoting the orientation of the RNF bundle}
  \item{outerBorderColor}{Border for outer symbol denoting statistical significance. Default is \code{NULL}}
  \item{innerBorderColor }{Color of the inner border that represents statistical significance. Default is \code{NULL}.}
  \item{outerBorderThickness}{Thickness of outer border for outer symbol denoting statistical significance. Default is \code{2}}
  \item{innerBorderThickness }{Thickness of the inner border that represents statistical significance. Default is \code{2}}	
}
\details{The vfplot function operates on a single row of visual fileds (vf). Depending on the plot type it will generate a plot representing the sensitivity at each location. The  color scheme elucidates the degree of sensitivity at that location. Locations with zero sensitivity are shown in black. Blind spots have been excluded fronm the plot}

\seealso{\code{\link{vfplot2}}}

\author{Chaitanya Khadilkar, Ivan Marin-Franch}