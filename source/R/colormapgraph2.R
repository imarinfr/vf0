colormapgraph2 <- function( ncol = 3, mapval = NULL, notSeenAsBlack = TRUE,
                           txtfont = "serif", pointsize = 8,
                           outerSymbol = "square", innerSymbol = "circles",
                           outerSize = 1, innerSize = 1,
                           outerInch = 0.35, innerInch = 0.11 ) {

  if( is.null( mapval ) ) {
    texteval <- "vfenv$nv$pmapsettings"
    mapval <- eval( parse( text = texteval ) )
  }
  
  mapval$cutoffs[ length( mapval$cutoffs ) ] <-
          paste( ">",  mapval$cutoffs[ length( mapval$cutoffs ) - 1 ], sep = "" )

  total <- nrow( mapval )
  if( notSeenAsBlack ) total <- total + 1
  nrow <- ceil( total / ncol )

# get coordinates to plot
  coords   <- NULL
  coords$x <- rep( 1:ncol, nrow )
  coords$y <- rep( 1:nrow, ncol )
  coords$y <- coords$y[order( coords$y, decreasing = TRUE )]
  coords   <- as.data.frame( coords )
  coords   <- coords[1:total,]

  xmin     <- min( coords$x ) - 1 / ncol
  xmax     <- max( coords$x ) + 1 / ncol
  ymin     <- min( coords$y ) - 1 / nrow
  ymax     <- max( coords$y ) + 1 / nrow
# get rgb and text to plot
  rgbval <- NULL
  txtval   <- NULL
  idx <- 0
  if( notSeenAsBlack ) {
    idx <- 1
    rgbval$red[idx]   <- 0
    rgbval$green[idx] <- 0
    rgbval$blue[idx]  <- 0
    txtval[idx]       <- "NS"
  }
  for( i in 1:nrow( mapval ) ) {
    rgbval$red[i+idx]   <- mapval$red[i]
    rgbval$green[i+idx] <- mapval$green[i]
    rgbval$blue[i+idx]  <- mapval$blue[i]
    txtval[i+idx]       <- as.character( mapval$cutoffs[i] )
  }
  rgbval <- rgb( as.data.frame( rgbval ) )

# opar <- par( no.readonly = TRUE )
  oplt    <- par()$plt
  ops     <- par()$ps
  ofamily <- par()$family
  par( plt = c( 0, 1, 0, 1 ) )
  par( ps = pointsize )
  par( family = txtfont )

# legend
  plot( coords$x, coords$y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c( xmin, xmax ), ylim = c( ymin, ymax ) )
  outerDimensions <- t( matrix( data = rep( outerSize, nrow( coords ) ),nrow = length( outerSize ), ncol = nrow( coords ) ) )
  evaltxt <- paste( "symbols( coords$x, coords$y, " , outerSymbol, " = outerDimensions, add = TRUE, inches = outerInch, bg = rgbval, fg = rgbval, lwd = 1 )", sep = "" )
  eval( parse( text = evaltxt ) )
  innerDimensions <- t( matrix( data = rep( innerSize,nrow( coords ) ),nrow = length( innerSize ), ncol = nrow( coords ) ) )
  evaltxt <- paste( "symbols( coords$x, coords$y, ", innerSymbol," = innerDimensions, add = TRUE, inches = innerInch, bg = 'white', lwd = 1, fg = 'white' )", sep = "" )
  eval( parse( text = evaltxt ) )

  text( coords$x, coords$y, labels = txtval, adj = 0.525 )

  par( plt = oplt )
  par( ps = ops )
  par( family = ofamily )
}
