\name{hist_poplr}
\alias{hist_poplr}
\title{histogram for PoPLR analysis}
\description{plots an histogram of Fisher S statistic combining all location p-values}
\usage{hist_poplr( scomb_obs, pcomb_obs, scomb, txtfont = "sans", pointsize = 10 )}
\arguments{
  \item{scomb_obs}{observed S-statistic for the observed visual fields}
  \item{pcomb_obs}{p-value associated with the observed S-statistic for the observed visual fields}
  \item{scomb}{S-statistics calculated for the permutation of visual fields}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{sans}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{10}}
}

\seealso{\code{\link{poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vflayout2_poplr}}, \code{\link{vfplot2_poplr}}}

\references{
[1] N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in glaucoma: estimating the overall significance of deterioration with permutation analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology and Visual Science, 53, 2012
}

\examples{
#vflayout_poplr( vf91016right )
}

\author{Ivan Marin-Franch}