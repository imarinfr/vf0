\name{hist_poplr2}
\alias{hist_poplr2}
\title{histogram for PoPLR analysis}
\description{plots an histogram of Fisher S statistic combining all location p-values}
\usage{hist_poplr2( sl, pvall, spl, sr, pvalr, spr,
                    nLoc = 52, txtfont = "sans", pointsize = 10,
                    coltxtl  = rgb( red = 1.0, green = 0.0, blue = 0.0 ),
                    colhistl = rgb( red = 1.0, green = 0.0, blue = 0.0, alpha = 0.25 ),
                    coltxtr  = rgb( red = 0.0, green = 0.5, blue = 0.0 ),
                    colhistr = rgb( red = 0.0, green = 0.5, blue = 0.0, alpha = 0.25 ) )}
\arguments{
  \item{sl}{observed S-statistic for the pointwise significance tests that slopes are lower than a specified reference (typically zero)}
  \item{pvall}{p-value for the pointwise significance tests that slopes are lower than a specified reference (typically zero)}
  \item{spl}{S-statistics calculated for the permutation of visual fields}
  \item{sr}{same as \code{sl} but for the right tail}
  \item{pvalr}{same as \code{pvall} but for the right tail}
  \item{spr}{same as \code{spl} but for the right tail}
  \item{nLoc}{number of locations in the visual field to be analyzed. For 24-2, it is 52 (54 minus the locations in the blind spot). Default is \code{52}.}
  \item{txtfont}{font of the text with visual-sensitivity values. Default is \code{sans}}
  \item{pointsize}{size of the text with visual-sensitivity values. Default is \code{10}}
  \item{colhistl}{Histogram color for the significance test that slopes are lower than zero. Default is \code{rgb( red = 1.0, green = 0.0, blue = 0.0 )}}
  \item{coltxtl}{Text color for the significance test that slopes are lower than zero. Default is \code{rgb( red = 1.0, green = 0.0, blue = 0.0 )}}
  \item{colhistr}{same as \code{colhistl} but for the right tail. Default is \code{rgb( red = 0.0, green = 0.5, blue = 0.0 )}}
  \item{coltxtr}{same as \code{coltxtl} but for the right tail. Default is \code{rgb( red = 0.0, green = 0.5, blue = 0.0 )}}
}

\seealso{\code{\link{poplr}}, \code{\link{poplr_cstat}}, \code{\link{poplr_pstat}}, \code{\link{vflayout_poplr}}, \code{\link{vfplot_plr}}}

\references{
[1] N. O'Leary, B. C. Chauhan, and P. H. Artes. \emph{Visual field progression in glaucoma: estimating the overall significance of deterioration with permutation analyses of pointwise linear regression (PoPLR)}. Investigative Ophthalmology and Visual Science, 53, 2012
}

\author{Ivan Marin-Franch}