\name{ vflayout2_poplr }
\alias{ vflayout2_poplr }
\title{layout displaying the PoPLR analysis}
\description{the function performs the PoPLR analysis (see \code{\link{poplr}}) with default values and presents the results in form of a A4 printout}
\usage{vflayout2_poplr( vf, grp = 3, nperm = 5000,
                            plotType = "vf", truncVal = 1,
                            type = "slr", typecomb = "fisher",
                            pwidth = 8.27, pheight = 11.69,
                            margin = 0.25, filename = NULL,
                            colorMapType = "pval", colorScale = NULL,
                            ringMapType  = NULL,  ringScale  = NULL,
                            imparedVision = 10, rangeNormal = NULL,
                            ffamily = "serif", sizetxt = 12,
                            sizetxtSmall = 8,
                            ffamilyvf = "serif", pointsize = 7,
                            outerSymbol = "circle", outerInch = 0.12,
                            innerSymbol = "circle", innerInch = outerInch / 1.9,
                            inch2axisunits = 12.528,
                            lengthLines = 0, thicknessLines = 0,
                            outerInchpoplr = 0.185,
                            innerInchpoplr = outerInchpoplr / 1.9,
                            lengthLinespoplr = 0, borderThickness = 1.5 )}
\arguments{
  \item{vf}{\code{\link{vf}} object with threshold sensitivities, td, or pd values}
  \item{grp}{how many visual fields to group. Default is \code{3}}
  \item{nperm}{number of permutations. Default is \code{5000}}
  \item{plotType}{Type of plot to show. It can be visual sensitivities (\code{vf}), total-deviation values (\code{td}), or pattern-deviation values (\code{pd})}
  \item{truncVal}{p-value cut-off for truncation. Default is \code{1}}
  \item{type}{Type of regression statistic: \code{slr} for simple linear regression and \code{rank} for Spearman correlation coefficient. Default is \code{slr}}
  \item{typecomb}{Combination type for p-values. Default is \code{fisher}}
  \item{pwidth}{width of the page (in inches). Default is \code{8.27}}
  \item{pheight}{height of the page (in inches). Default is \code{11.69}}
  \item{margin}{margins of the page (in inches). Default is \code{0.25}}
  \item{filename}{file name to save the printout as pdf. If it is saved to pdf, it won't be displayed in the screen. Default is \code{NULL}.}
  \item{colorMapType}{what does color map categorizes. It can be \code{pvals}, \code{slopes}, or years \code{blind}. Default is \code{pvals}}
  \item{colorScale}{Color mapping to use. Default is \code{NULL}. A different default is given depending on \code{colorMapType}}
  \item{ringMapType}{what concentric rings categorizes. It can be \code{pvals} or \code{slopes}, or years \code{blind}. Default is \code{NULL}, for which no rings are shown.}
  \item{ringScale}{Ring mapping to use. Default is \code{NULL}. A different default is given depending on \code{ringMapType}}
  \item{rangeNormal}{range to indicate which slopes are normal. All significance from data points within the range will be removed. Default is \code{NULL}.}
  \item{imparedVision}{sensitivity below which it is considered imparement. Default is \code{10}}
  \item{ffamily}{Family font for the text in the from. Default is \code{"Helvetica"}}
  \item{sizetxt}{Text size for main text in the form. Default is \code{12}}
  \item{sizetxtSmall}{Text size for details in the form. Default is \code{8}}
  \item{ffamilyvf}{Font family for text in lego plots. Default is \code{"Times"}}
  \item{pointsize}{Point size for text in lego plots. Default is \code{7}}
  \item{outerSymbol}{Type of outer symbol. Default is \code{"circle"}}
  \item{outerInch}{Inches of outer symbol. Default is \code{0.12}}
  \item{innerSymbol}{Type of inner symbol. Default is \code{"circle"}}
  \item{innerInch}{Inches of inner symbol. Default is \code{outerInch / 1.9}}
  \item{inch2axisunits}{Conversion factor inches to axis units. Default is \code{12.528}}
  \item{lengthLines}{Line length. Default is \code{0}}
  \item{thicknessLines}{Line thickness. Default is \code{0}}
  \item{outerInchpoplr}{Size of outer symbols for PoPLR gragh. Default is \code{0.185}}
  \item{innerInchpoplr}{Size of inner symbols for PoPLR gragh. Default is \code{outerInchpoplr / 1.9}}
  \item{lengthLinespoplr}{length of lines. Default is \code{0}}
  \item{borderThickness}{Border thickness. Default is \code{1.5}}
}

\seealso{\code{\link{vflayout2}}, \code{\link{vflayout2_legoplot}}, \code{\link{poplr}}, \code{\link{hist_poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vfplot2_poplr}}}

\references{
[1] N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in glaucoma: estimating the overall significance of deterioration with permutation analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology and Visual Science, 53, 2012
}

\examples{
  \donttest{
    vflayout2_poplr( vf91016right )
  }
}

\author{Ivan Marin-Franch, Chaitanya Khadilkar}