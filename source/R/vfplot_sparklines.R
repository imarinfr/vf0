vfplot_sparklines <- function( vf,  patternMap, ... ) {

  texteval <- "vfsettings$locini"
  locini   <- eval( parse( text = texteval ) )

  oparfig <- par()$fig # save graphics state before setting to individual "pointwise" regions

  # plot regions in NDC coordinates NB: need to modify for non-24-2 patterns 
  locs <- cbind( grconvertX( patternMap$xod - 3, to = "ndc" ), grconvertX( patternMap$xod + 3, to = "ndc" ),
                 grconvertY( patternMap$yod - 3, to = "ndc" ), grconvertY( patternMap$yod + 3, to = "ndc" ) )

  #  set a new plot region for each of the sparklines
  for( col in 1:nrow( locs ) )  {
    par( fig = locs[col,], mar = c( 0, 0, 0, 0 ), mgp = c( 0, 0, 0 ), new = TRUE )
    plot( vf$tdate, vf[,locini + col - 1], ylim = c( 0, 35 ), type = "l", axes = FALSE, ... )
  }
  par ( fig = oparfig ) # reset the graphics states 
}