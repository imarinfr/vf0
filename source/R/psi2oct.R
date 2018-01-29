psi2oct <- function( psi, diam = 12, xy0 = c( 15, 2 ) ) {
  radius <- diam / 2
  r <- seq( 4, 10, 0.001 )
  oct <- NULL
  for( i in 1:length( psi ) ) {
    fb <- fiberpathpsi( psi[i] )
    xy <- jpolar2cart( data.frame( r = r, phi = fb( r ) ) ) - xy0
    xy <- xy[which.min( abs( xy$x^2 + xy$y^2 - radius^2 ) ),]
    oct2 <- atan( abs( xy[,2] / xy[,1] ) )
    
    oct2[is.nan( oct2 )]         <- 0
    oct2[xy$x > 0 & xy$y < 0]   <- 2 * pi - oct2[xy$x > 0 & xy$y < 0]
    oct2[xy$x < 0 & xy$y > 0]   <- pi - oct2[xy$x < 0 & xy$y > 0]
    oct2[xy$x < 0 & xy$y < 0]   <- oct2[xy$x < 0 & xy$y < 0] + pi
    oct2[xy$x >= 0 & xy$y == 0] <- 0
    oct2[xy$x == 0 & xy$y > 0]  <- pi / 2
    oct2[xy$x == 0 & xy$y < 0]  <- -pi / 2
    oct2[xy$x < 0 & xy$y == 0]  <- pi
    oct2[oct2 < 0]               <- 2 * pi + oct2[oct2 < 0]
    oct[i] <- oct2
  }
  return( 180 / pi * oct )
}