hist_poplr2 <- function( sl, pvall, spl, sr, pvalr, spr,
                         nLoc = 52, txtfont = "sans", pointsize = 10,
                         coltxtl  = rgb( red = 1.0, green = 0.0, blue = 0.0 ),
                         colhistl = rgb( red = 1.0, green = 0.0, blue = 0.0, alpha = 0.25 ),
                         coltxtr  = rgb( red = 0.0, green = 0.5, blue = 0.0 ),
                         colhistr = rgb( red = 0.0, green = 0.5, blue = 0.0, alpha = 0.25 ) ) {
  sep <- 2 * 6 / 100
  sl  <- sl / nLoc
  sr  <- sr / nLoc
  spl <- spl / nLoc
  spr <- spr / nLoc
  spl[spl > 6] <- 6 # cap to a maximum S/n value of 6 (p-value with 6 decimal places)
  spr[spr > 6] <- 6 # cap to a maximum S/n value of 6 (p-value with 6 decimal places)
  pvall_txt <- paste0( "(p = ", round( pvall, 3 ), ")" )
  pvalr_txt <- paste0( "(p = ", round( pvalr, 3 ), ")" )
  if( sl <= 6 ) {
    spl_txt <- round( sl, 1 )
  } else {
    sl <- 6
    spl_txt <- paste( "> 6" )
  }
  if( sr <= 6 ) {
    spr_txt <- round( sr, 1 )
  } else {
    sr <- 6
    spr_txt <- paste( "> 6" )
  }
  adjl <- 0.5
  adjr <- 0.5
  sr  <- 12 - sr
  spr <- 12 - spr
  if( sl < 2 ) adjl <- 0
  if( sl > 4 ) adjl <- 1
  if( sr < 8 ) adjr <- 0
  if( sr > 10 ) adjr <- 1
  breaks  <- seq( 0, 2 * 6, by = sep )

  par( ps     = pointsize )
  par( family = txtfont )
  par( plt = c( 0, 1.0, 0.3, 1 ) )
  par( mgp = c( 1.85, 0.5, 0 ) )

  ymax <- max( c( max( hist( spl, breaks = breaks, plot = FALSE )$density ),
                  max( hist( spr, breaks = breaks, plot = FALSE )$density ) ) )
  # left tail
  hist( spl, breaks = breaks, freq = FALSE, main = "",
        xlim = c( 0, 12 ), ylim = c( 0, ymax ),
        xlab = "", ylab = "", lty = 0,
        col = colhistl,
        axes = FALSE, ann = FALSE )
  lines( c( sl, sl ), 0.8 * c( 0, ymax ), col = coltxtl )
  points( sl, 0.8 * ymax, pch = 21, col = coltxtl, bg = coltxtl )
  text( sl, 0.9 * ymax, labels = paste( spl_txt, pvall_txt ), adj = adjl, cex = 0.7 )
  # right tail
  hist( spr, breaks = breaks, freq = FALSE, main = "",
        xlim = c( 0, 6 ), ylim = c( 0, ymax ),
        xlab = "", ylab = "", lty = 0,
        col = colhistr,
        axes = FALSE, ann = FALSE, add = TRUE )
  lines( c( sr, sr ), 0.6 * c( 0, ymax ), col = coltxtr )
  points( sr, 0.6 * ymax, pch = 21, col = coltxtr, bg = coltxtr )
  text( sr, 0.7 * ymax, labels = paste( spr_txt, pvalr_txt ), adj = adjr, cex = 0.7 )

  axis( 1, at = c( 0, 2, 4, 6, 8, 10, 12 ), labels = c( 0, 2, 4, 6, 4, 2, 0 ),
        tcl = -0.3, lwd = 0.5, lwd.ticks = 0.5 )
  lines( c( 6, 6 ), c( 0, ymax ), col = "black", lty = "dashed" )
  title( xlab = "S / n" )
}