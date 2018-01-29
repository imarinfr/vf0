vfplotloc <- function( vals, patternMap, vftiles, vfhull, loccol,
                       xmin = -29, xmax = 29, ymin = -29, ymax = 29,
                       txtfont = "sans", pointsize = 10,
                       showaxis = FALSE, colaxis = "white" ) {

# init
  oplt    <- par()$plt
  ops     <- par()$ps
  ofamily <- par()$family
  par( plt = c( 0, 1, 0, 1 ) )
  par( ps = pointsize )
  par( family = txtfont )

  plot( patternMap$xod, patternMap$yod, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, xlim = c( xmin, xmax ), ylim = c( ymin, ymax ) )
  # plot VF values
  plot( vftiles, clipp = vfhull, add = TRUE, close = TRUE, showpoints = FALSE, border = "darkgray", fillcol = rgb( loccol ) )
  if( showaxis ) {
    axis( 1, pos = 0, labels = FALSE, lwd.ticks = 0, at = c( xmin, xmax ), col = colaxis )
    axis( 2, pos = 0, labels = FALSE, lwd.ticks = 0, at = c( ymin, ymax ), col = colaxis )
  }
  idx <- which( is.na( vals ) )
  if( length( idx ) > 0 ) {
    patternMap <- patternMap[-idx,]
    vals       <- vals[-idx]
  }
  coltxt <- rep( "black", length( patternMap$xod ) )
  coltxt[( loccol$red + loccol$green + loccol$blue ) / 3 < 0.25] <- "white"
  text( patternMap$xod, patternMap$yod, vals, col = coltxt )
  par( plt    = oplt )
  par( ps     = ops )
  par( family = ofamily )

}