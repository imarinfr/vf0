\name{hist_poplr}
\alias{hist_poplr}
\title{histogram for PoPLR analysis}
\description{plots an histogram of Fisher S statistic combining all location p-values}
\usage{hist_poplr( scomb_obs, pcomb_obs, scomb, nLoc = 52, txtfont = "sans", pointsize = 10 )}
\arguments{
  \item{scomb_obs}{observed S-statistic for the observed visual fields}
  \item{pcomb_obs}{p-value associated with the observed S-statistic for the observed visual fields}
  \item{scomb}{S-statistics calculated for the permutation of visual fields}
  \item{nLoc}{number of locations in the visual field to be analyzed. For 24-2, it is 52 (54 minus the locations in the blind spot). Default is \code{52}.}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{sans}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{10}}
}

\seealso{\code{\link{poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vflayout_poplr}}, \code{\link{vfplot_poplr}}}

\references{
[1] N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in glaucoma: estimating the overall significance of deterioration with permutation analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology and Visual Science, 53, 2012
}

\author{Ivan Marin-Franch}