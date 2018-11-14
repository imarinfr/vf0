hist_poplr <- function( scomb_obs, pcomb_obs, scomb, nLoc = 52, txtfont = "sans", pointsize = 10 ) {
  sep <- 0.1
  scomb_obs <- scomb_obs / nLoc
  scomb     <- scomb / nLoc
  scomb[scomb > 6] <- 6 # cap to a maximum S/n value of 6 (p-value with 6 decimal places)
  pcomb_txt <- paste0( "(p = ", round( pcomb_obs, 3 ), ")" )
  if( scomb_obs <= 6 ) {
    scomb_txt <- paste( "S / n = ", round( scomb_obs, 1 ) )
  } else {
    scomb_obs <- 6
    scomb_txt <- paste( "S / n > 6" )
  }
  adj <- 0.5
  if( scomb_obs < 1 ) adj <- 0
  if( scomb_obs > 5 ) adj <- 1
  breaks  <- seq( 0, 6, by = sep )
  ops     <- par()$ps
  ofamily <- par()$family
  oplt    <- par()$plt

  par( ps     = pointsize )
  par( family = txtfont )
  par( plt    = c( 0, 1, 0.3, 1 ) )

  hist( scomb, breaks = breaks, freq = FALSE, main = "", xlim = c( 0, 6 ), ylim = c( 0, 1.5 ), xlab = "", border = rgb( 0.7, 0.7, 0.7 ), col = rgb( 0.9, 0.9, 0.9 ), axes = FALSE )
  title( xlab = "S / number of locations n", mgp = c( 2, 1, 0 ) )
  lines( c( scomb_obs, scomb_obs ), c( 0, 1.1 ), col = "red" )
  points( c( scomb_obs ), 1.1, pch = 21, col = "red", bg = "red" )
  text( c( scomb_obs ), 1.2, labels = paste( scomb_txt, pcomb_txt ), adj = adj )

  par( ps     = ops )
  par( family = ofamily )
  par( plt    = oplt )
}