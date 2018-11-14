progols <- function( tdate, index, projyears = 5,
                     xlab = "years from first visit", ylab = "md",
                     txtfont = "sans", pointsize = 10, cex = 1,
                     markfl = FALSE, prggrp = 3 ) {

  lt      <- as.POSIXlt( tdate )
  mon     <- lt$year * 12 + lt$mon
  yeardif <- ( mon - mon[1] ) / 12
  xreg    <- c( min( yeardif ), max( yeardif ) + projyears )
# get regression
  mdreg <- lm( index ~ yeardif )
  pval  <- summary( mdreg )$coefficients[2,4] / 2
  pval  <- round( pval, 3 )
  ylab  <- paste( ylab, ", p = ", as.character( pval ), sep = "" )
  yreg  <- mdreg$coefficients[1] + mdreg$coefficients[2] * xreg
  
  ops     <- par()$ps
  ofamily <- par()$family
  oplt    <- par()$plt
  obty    <- par()$bty
  omgp    <- par()$mgp
  par( ps = pointsize )
  par( family = txtfont )
  par( bty = "n" )
  par( plt = c( 0.3, 1.0, 0.3, 1 ) )
  par( mgp = c( 1.85, 0.5, 0 ) )

  # graph limits
  xlim <- c( xreg[1], xreg[2] + 1 )
  if( mdreg$coefficients[2] <= 0 ) {
    ylim <- c( max( index ) - 6, max( index ) + 1 )
  } else {
    ylim <- c( min( index ) - 1, min( index ) + 6 )
  }
  firstTick <- ceiling( ylim[1] )
  tickMarks <- c( firstTick, firstTick + 2, firstTick + 4, firstTick + 6 )

  plot( yeardif, index, type = "n", axes = FALSE, ann = FALSE, xlim = xlim, ylim = ylim )
  axis( 1, las = 1, tcl = -0.3, lwd = 0.5, lwd.ticks = 0.5 )
  axis( 2, las = 1, tcl = -0.3, lwd = 0.5, lwd.ticks = 0.5, at = tickMarks )
  points( yeardif, index, pch = 19, col = "gray50" )
  lines( xreg, yreg, lwd = 3 )
  if( markfl ) {
    points( yeardif[1:prggrp], index[1:prggrp], pch = 19, col = "white", cex = 0.1 )
    points( yeardif[( length( yeardif ) - prggrp + 1 ):length( yeardif )], index[( length( yeardif ) - prggrp + 1 ):length( yeardif )], pch = 19, col = "white", cex = 0.1 )
  }
  box()
  title( xlab = xlab )
  title( ylab = ylab )

  par( new    = FALSE )
  par( ps     = ops )
  par( family = ofamily )
  par( plt    = oplt )
  par( bty    = obty )
  par( mgp    = omgp )
}
