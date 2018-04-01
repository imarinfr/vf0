gcloc2psi <- function( xy, r0 = 4 ) {
  # Returns the angle of incidence at the ONH of a vf locations
  xy[,2] <- -xy[,2]
  psi0   <- rep( NA, nrow( xy ) )
  rpsi   <- cart2jpolar( xy )
  for( i in 1:nrow( xy ) ) {
    fbpathinv <- function( psi ) {
      fb <- fiberpathpsi( psi, r0 )
      psiest <- fb( rpsi[i,1] )
      return( ( psiest - rpsi[i,2] )^2 )
    }
    psi00 <- c( 60, 180 )
    if( xy[i,2] < 0 ) psi00 <- c( -180, -60 )
    if( xy[i,1] > 15 ) psi00 <- c( -60, 60 )
    
    psi0[i] <- optimize( fbpathinv, interval = psi00 )$minimum
  }
  return( psi0 )
}