hist_poplr1 <- function( s, pval, sp, nLoc = 52, txtfont = "sans", pointsize = 10,
                         coltxt  = rgb( red = 1.0, green = 0.0, blue = 0.0 ),
                         colhist = rgb( red = 1.0, green = 0.0, blue = 0.0, alpha = 0.5 ) ) {
  sep <- 6 / 100
  s <- s / nLoc
  sp     <- sp / nLoc
  sp[sp > 6] <- 6 # cap to a maximum S/n value of 6 (p-value with 6 decimal places)
  pval_txt <- paste0( "(p = ", round( pval, 3 ), ")" )
  if( s <= 6 ) {
    sp_txt <- round( s, 1 )
  } else {
    s <- 6
    sp_txt <- paste( "> 6" )
  }
  adj <- 0.5
  if( s < 2 ) adj <- 0
  if( s > 4 ) adj <- 1
  breaks  <- seq( 0, 6, by = sep )

  par( ps     = pointsize )
  par( family = txtfont )
  par( plt = c( 0, 1.0, 0.3, 1 ) )
  par( mgp = c( 1.85, 0.5, 0 ) )

  ymax <- max( hist( sp, breaks = breaks, plot = FALSE )$density )
  hist( sp, breaks = breaks, freq = FALSE, main = "",
        xlim = c( 0, 6 ), ylim = c( 0, ymax ),
        xlab = "", ylab = "", lty = 0,
        col = colhist,
        axes = FALSE, ann = FALSE )
  axis( 1, tcl = -0.3, lwd = 0.5, lwd.ticks = 0.5 )
  lines( c( s, s ), 0.8 * c( 0, ymax ), col = coltxt )
  points( s, 0.8 * ymax, pch = 21, col = coltxt, bg = coltxt )
  text( s, 0.9 * ymax, labels = paste( sp_txt, pval_txt ), adj = adj )
  title( xlab = "S / n" )
}