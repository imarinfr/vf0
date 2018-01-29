cart2jpolar <- function( xy, xy0 = c( 15, 2 ) ) {

  idx <- which( xy[,1] > 0 )
  xy[idx,2] = xy[idx,2] - xy0[2] * ( xy[idx,1] / xy0[1] )^2
  xy[,1] <- xy[,1] - xy0[1]

  r   <- sqrt( xy[,1]^2 + xy[,2]^2 )
  psi <- atan( abs( xy[,2] / xy[,1] ) )

  psi[is.nan( psi )]             <- 0
  psi[xy[,1] > 0 & xy[,2] < 0]   <- 2 * pi - psi[xy[,1] > 0 & xy[,2] < 0]
  psi[xy[,1] < 0 & xy[,2] > 0]   <- pi - psi[xy[,1] < 0 & xy[,2] > 0]
  psi[xy[,1] < 0 & xy[,2] < 0]   <- psi[xy[,1] < 0 & xy[,2] < 0] + pi
  psi[xy[,1] >= 0 & xy[,2] == 0] <- 0
  psi[xy[,1] == 0 & xy[,2] > 0]  <- pi / 2
  psi[xy[,1] == 0 & xy[,2] < 0]  <- -pi / 2
  psi[xy[,1] < 0 & xy[,2] == 0]  <- pi
  psi[psi < 0]                   <- 2 * pi + psi[psi < 0]

  return( data.frame( r = r, psi = 180 / pi * psi ) )
}