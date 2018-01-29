gcloc2psi <- function( xy, xy0 = c( 15, 2 ), r0 = 4 ) {
  # Returns the angle of incidence at the ONH of a vf locations
  psi0 <- rep( NA, nrow( xy ) )
  rpsi <- cart2jpolar( xy, xy0 )
  for( i in 1:nrow( xy ) ) {
    fbpathinv <- function( psi ) {
      if( psi < 0 ) psi <- psi + 360
      fb <- fiberpathpsi( psi, r0 )
      psiest <- fb( rpsi[i,1] )
      return( ( psiest - rpsi[i,2] )^2 )
    }
    psi00 <- c( 60, 180 )
    if( xy[i,2] < 0 ) psi00 <- c( -180, -60 )
    if( xy[i,1] > xy0[1] ) psi00 <- c( -60, 60 )
    
    psi0[i] <- optimize( fbpathinv, interval = psi00 )$minimum
    if( psi0[i] < 0 ) psi0[i] <- psi0[i] + 360
  }
  return( psi0 )
}