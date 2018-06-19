\name{vfplot2_poplr}
\alias{vfplot2_poplr}
\title{plot with the PoPLR analysis}
\description{plots the slope values and the corresponding probabiliby category for each location}
\usage{vfplot2_poplr( sl, pval, vfinfo, newWindow = FALSE, txtfont = "mono",
              pointsize = 7, width = 6,
              xminmax = 29, yminmax = 29,
              outerSymbol = "circles", innerSymbol = "circles",
              outerSize = 1, innerSize = 1,
              outerInch = 0.24, innerInch = 0.12,
              lengthLines = 0, thicknessLines = 0,
              colorMapType = "pval", colorScale = NULL,
              ringMapType  = NULL, ringScale  = NULL,
              imparedVision = 10, borderThickness = 2,
              idxNotSeen = NULL, rangeNormal = NULL,
              conormal = NULL )}
\arguments{
  \item{sl}{slopes}
  \item{pval}{pvalues calculated for the slope}
  \item{vfinfo}{information about the visual field}
  \item{newWindow}{boolean value- TRUE will generate a new window for the plot. Default value is TRUE}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{mono}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{7}}
  \item{width}{width of the window. height is calucated using the width, xminmax and yminmax}
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
  \item{colorMapType}{what does color map categorizes. It can be \code{pvals}, \code{slopes}, or years \code{blind}. Default is \code{pvals}}
  \item{colorScale}{Color mapping to use. Default is \code{NULL}. A different default is given depending on \code{colorMapType}}
  \item{ringMapType}{what concentric rings categorizes. It can be \code{pvals} or \code{slopes}, or years \code{blind}. Default is \code{NULL}, for which no rings are shown.}
  \item{ringScale}{Ring mapping to use. Default is \code{NULL}. A different default is given depending on \code{ringMapType}}
  \item{imparedVision}{sensitivity below which it is considered imparement. Default is \code{10}}
  \item{borderThickness}{Thickness of the borders if ringMapType is not \code{NULL}. Default is \code{5}}
  \item{idxNotSeen}{index of locations that are not seen. These locations will be displayed with a solid balck circle. Default is \code{NULL}.}
  \item{rangeNormal}{range to indicate which slopes are normal. All significance from data points within the range will be removed. Default is \code{NULL}.}
  \item{conormal}{if a \code{rangeNormal} is defined, then this specifies the within-normal category. Default is \code{NULL}. If \code{colorMapType = "pval"}, then it is 95, if \code{colorMapType = "slope"}, then it is 0.5}
}
\details{The vfplot function operates on a single row of visual fileds (vf). Depending on the plot type it will generate a plot representing the sensitivity at each location. The  color scheme elucidates the degree of sensitivity at that location. Locations with zero sensitivity are shown in black. Blind spots have been excluded fronm the plot}

\seealso{\code{\link{poplr}}, \code{\link{hist_poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vflayout2_poplr}}}

\references{
[1] N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in glaucoma: estimating the overall significance of deterioration with permutation analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology and Visual Science, 53, 2012
}

\examples{
  res <- poplr( vf91016right )
  vfplot2_poplr( res$sl, res$pval, res$vfdata )
}

\author{Ivan Marin-Franch, Chaitanya Khadilkar}