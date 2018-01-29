fiberpathpsi <- function( psi0, r0 = 4 ) {
  # Returns a function describing the expected fiber path given an angle of
  # incidence on the blind spot. Implemented after Jansonius_et_al_VR_2009, but see
  # erratum at Jansonius_etal_VR_2010_erratum. The coordinates are swapped in the
  # y-axis, as in visual field graphs
  # get c and b constants

  if( psi0 == 0 ) psi0 <- 360
  if( psi0 > 0 & psi0 < 60 ) {
    b <- 0.00083 * psi0^2 + 0.020 * psi0 - 2.65
    c <- 1.9 + 1.4 * tanh( ( psi0 - 121 ) / 14 )
  }
  if( psi0 >= 60 & psi0 <= 180 ) {
    b <- exp( -1.9 + 3.9 * tanh( - ( psi0 - 121 ) / 14 ) )
    c <- 1.9 + 1.4 * tanh( ( psi0 - 121 ) / 14 )
  }
  if( psi0 > 180 & psi0 <= 300 ) {
    b <- -exp( 0.7 + 1.5 * tanh( - ( 270 - psi0 ) / 25 ) )
    c <- 1.0 + 0.5 * tanh( ( 270 - psi0 ) / 25 )
  }
  if( psi0 > 300 ) {
    b <- 0.00083 * ( psi0 - 360 )^2 + 0.020 * ( psi0 - 360 ) - 2.65
    c <- 1.0 + 0.5 * tanh( ( 270 - psi0 ) / 25 )
  }
  fbfun <- function( r ) {
    return( psi0 + b * ( r - r0 )^c )
  }
  return( fbfun )
}