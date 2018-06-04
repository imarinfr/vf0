vfplot <- function( vf, plotType,
                    xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
                    notSeenAsBlack = TRUE, newWindow = FALSE,
                    txtfont = "sans", pointsize = 10, width = 6,
                    showaxis = FALSE, colaxis = "white" ) {

  # check that vf has only 1 entry
  if( nrow( vf ) > 1 ) {
    stop("Error! vf cannot have more than 1 rows")
  }

  # special locations in the visual field: BS locations
  bsxy   <- NULL
  bsxy$x <- c( 15, 15)
  bsxy$y <- c( 3, -3 )
  bsxy   <- as.data.frame( bsxy )
  
  texteval <- "vfsettings$locini"
  locini   <- eval( parse( text = texteval ) )
  
  # construct the pattern string based on the pattern type
  evaltxt <- paste( "vfsettings$", vf$tpattern, "$locnum", sep = "" )
  loc_num <- eval( parse( text = evaltxt ) )

  # construct patternmap
  evaltxt    <- paste( vf$tperimetry, "locmap$", vf$tpattern, sep = "" )
  patternMap <- eval( parse( text=evaltxt ) )
  patternMap <- patternMap[,c( "xod", "yod" )]

  # if not imposed, calculate limits of plot
  # expand by 5% each axis
  xrange <- max( patternMap$xod ) - min( patternMap$xod )
  yrange <- max( patternMap$yod ) - min( patternMap$yod )
  if( is.null( xmin ) ) xmin <- min( patternMap$xod ) - 0.025 * xrange
  if( is.null( xmax ) ) xmax <- max( patternMap$xod ) + 0.025 * xrange
  if( is.null( ymin ) ) ymin <- min( patternMap$yod ) - 0.025 * yrange
  if( is.null( ymax ) ) ymax <- max( patternMap$yod ) + 0.025 * yrange

# read in the plotType and decide what to do
  if( plotType == "vf" ) {
    dev  <- vf
  }

# if plot type id 'td' then calculate total deviation and total deviation probability
  if( plotType == "td" ) {
    dev  <- tdval( vf )
    devp <- tdpmap( dev )
  }  
    
# if plot type id 'pd' then first calculate total deviation
# use the toal deviation to calculate probabilty deviation 
  if( plotType == "pd" ) {
    dev  <- pdval( tdval( vf ) )
    devp <- pdpmap( dev )
  }

# if plot type id "pdghr" then first calculate total deviation
# use the total deviation to calculate pattern deviation from global-sensitivity estimate
  if( plotType == "pdghr" ) {
    dev  <- pdvalghr( tdval( vf ) )
    devp <- pdpmapghr( dev )
  }

# getRGB will return a table with the red, green and blue intensity values 
# corresponding to pattern deviation at each location
  if( plotType == "vf" ) {
    plotColor <- vfgrayscale( dev[,locini:( locini + loc_num - 1 )], age = vf$sage, pattern = vf$tpattern, algorithm = vf$talgorithm )
    cloneDev  <- as.character( round( dev[,locini:( locini + loc_num - 1 )] ) )
    cloneDev[which( dev[,locini:( locini + loc_num - 1 )] < 0 )] = "<0"
  }  else {
    plotColor <- vfcolormap( as.numeric( devp[,locini:( locini + loc_num - 1 )] ) )
    cloneDev  <- as.numeric( dev[,locini:( locini + loc_num - 1 )] )
    cloneDev  <- as.character( round( cloneDev ) )
    if( notSeenAsBlack ) {
      idxblack <- which( vf[locini:( locini + loc_num - 1 )] <= 0)
      if( length( idxblack ) > 0 ) plotColor[idxblack,] <- 0
    }
  }

  # if NA then plot all in black
  plotColor[is.na( plotColor )] <- 0

  # add void points if not in the test of locations
  for( i in 1:nrow( bsxy ) ) {
    if( xmin < bsxy$x[i] & xmax > bsxy$x[i] & ymin < bsxy$y[i] & ymax > bsxy$y[i] ) {
      idx <- which( patternMap$xod == bsxy$x[i] & patternMap$yod == bsxy$y[i] )
      if( length( idx ) > 0 ) {
        if( plotType == "vf" ) {
          plotColor[idx,] <- c( 0, 0, 0 )
        }  else {
          plotColor[idx,] <- c( 0.5, 0.5, 0.5 )
        }
      } else {
        patternMap <- rbind( patternMap, c( bsxy$x[i], bsxy$y[i] ) )
        cloneDev   <- c( cloneDev,  NA )
        if( plotType == "vf" ) {
          plotColor <- rbind( plotColor, c( 0, 0, 0 ) )
        }  else {
          plotColor <- rbind( plotColor, c( 0.5, 0.5, 0.5 ) )
        }
      }
    }
  }

  if( vf$seye == "OS" ) {
    xmin2 <- xmin
    xmin  <- -xmax
    xmax  <- -xmin2
    patternMap$xod <- -patternMap$xod
  }
  # get the Voronoi tesselation tiles tiles
  vftess  <- vftessellation( patternMap, dist = 3 )
  vftiles <- tile.list( vftess[[1]] )
  vfhull  <- vftess[[2]]

# create a new window and plot data in it
# window rescale is set to fixed to ensure re-sizing window doesn't re-size the plot
  height <- width * ( ymax - ymin ) / ( xmax - xmin )
  if( newWindow ) {
    device <- options( "device" )
    if( .Platform$OS.type == "unix" ) {
      if( Sys.info()["sysname"] == "Darwin" ) {
        options( device = "quartz" )
        dev.new( width = width, height = height, dpi = 85 )
      } else {
        options( device = "x11" )
        dev.new( width = width, height = height )
      }
    } else{
      options( device = "windows" )
      dev.new( width = width, height = height, rescale = "fixed" )
    }
    options( device = device )
  }
  vfplotloc( cloneDev, patternMap = patternMap,
             vftiles = vftiles, vfhull = vfhull, loccol = plotColor,
             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             txtfont = txtfont, pointsize = pointsize,
             showaxis = showaxis, colaxis = colaxis )
}