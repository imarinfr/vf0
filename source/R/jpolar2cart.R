jpolar2cart <- function( rpsi, xy0 = c( 15, 2 ) ) {

  r    <- rpsi[,1]
  psi  <- rpsi[,2]
  psip <- psi
  psip[psi > 90 & psi < 180]  <- 180 - psip[psi > 90 & psi < 180]
  psip[psi > 180 & psi < 270] <- psip[psi > 180 & psi < 270] - 180
  psip[psi > 270 & psi < 360] <- 360 - psip[psi > 270 & psi < 360]
  
  x <- sqrt( r^2 / ( 1 + ( tan( pi / 180 * psip ) )^2 ) )
  
  y <- abs( x * tan( pi / 180 * psip ) )
  x[psi > 90 & psi < 180]  <- - x[psi > 90 & psi < 180]
  x[psi > 180 & psi < 270] <- - x[psi > 180 & psi < 270]
  y[psi > 180 & psi < 270] <- - y[psi > 180 & psi < 270]
  y[psi > 270 & psi < 360] <- - y[psi > 270 & psi < 360]
  
  x[psi == 0]   <- r[psi == 0]
  y[psi == 0]   <- 0
  x[psi == 90]  <- 0
  y[psi == 90]  <- r[psi == 90]
  x[psi == 180] <- -r[psi == 180]
  y[psi == 180] <- 0
  x[psi == 270] <- 0
  y[psi == 270] <- -r[psi == 270]
  
  x        <- x + xy0[1]
  y[x > 0] <- y[x > 0] + xy0[2] * ( x[x > 0] / xy0[1] )^2

  return( data.frame( x = round( x, 6 ), y = round( y, 6 ) ) )
}