\name{vflayout_poplr}
\alias{vflayout_poplr}
\title{layout displaying the PoPLR analysis}
\description{the function performs the PoPLR analysis (see \code{\link{poplr}}) with default values and presents the results in form of a A4 printout}
\usage{vflayout_poplr( vf, grp = 3, nperm = 5000,
                      plotType = "vf", truncVal = 1,
                      type = "slr", typecomb = "fisher",
                      pwidth = 8.27, pheight = 11.69,
                      margin = 0.25, filename = NULL,
                      colorMapType = "pval", colorScale = NULL )}
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
}

\seealso{\code{\link{vflayout}}, \code{\link{poplr}}, \code{\link{hist_poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vfplot_poplr}}}

\references{
[1] N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in glaucoma: estimating the overall significance of deterioration with permutation analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology and Visual Science, 53, 2012
}

\examples{
  \donttest{
    vflayout_poplr( vf91016right )
  }
}

\author{Ivan Marin-Franch, Chaitanya Khadilkar}