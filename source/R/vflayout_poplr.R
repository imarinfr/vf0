vflayout_poplr <- function( vf, grp = 3, nperm = 5000,
                            plotType = "vf", summaryIndex1 = "md", summaryIndex2 = "gh",
                            truncVal = 1, type = "slr", typecomb = "fisher",
                            pwidth = 8.27, pheight = 11.69,
                            margin = 0.25, filename = NULL,
                            colorMapType = "pval", colorScale = NULL,
                            showaxis = FALSE, colaxis = "black" ) {

  txtfont   <- "sans"
  pointsize <- 10
  txtsize   <- 6

  ##############
  # input checks
  ##############
  # check that all rows in vf belong to the same subject, the same test, the same perimetry
  # testing and the same algorithm, and the same eye
  if( length( unique( vf$tperimetry ) ) > 1 |
      length( unique( vf$tpattern   ) ) > 1 |
      length( unique( vf$talgorithm ) ) > 1 |
      length( unique( vf$id ) ) > 1         |
      length( unique( vf$seye ) ) > 1 ) {
    stop( "all visual fields should belong to the same subject and eye tested with the same perimeter and algorithm on the same locations" )
  }
  if( nrow( vf ) < 2 * grp ) stop( "the number of visual fields needs to be at least twice the number of visual fields to group for the analysis" )
  if( nrow( vf ) < 7 )       warning( "permutation analysis may not be precise for fewer than 7 visual fields" )
  # types of color map and ring map
  if( is.null( colorMapType) ) stop( "colorMapType must be 'slope', 'pval', or 'blind'" )
  if( colorMapType != "pval" & colorMapType != "slope" & colorMapType  != "blind" ) stop( "wrong colorMapType. Must be 'slope', 'pval', or 'blind'" )
  if( summaryIndex1 != "ms" & summaryIndex1 != "md" & summaryIndex1 != "gh" & summaryIndex1 != "ghr" ) stop( "wrong summary index. Must be 'ms', 'md', 'gh', 'ghr'" )
  if( summaryIndex2 != "ms" & summaryIndex2 != "md" & summaryIndex2 != "gh" & summaryIndex2 != "ghr" ) stop( "wrong summary index. Must be 'ms', 'md', 'gh', 'ghr'" )
  # truncation must be between zero and one
  if( truncVal <= 0 | truncVal > 1 ) stop("truncation must be between 0 and 1")

  # special locations in the visual field: BS locations
  bsxy   <- NULL
  bsxy$x <- c( 15, 15)
  bsxy$y <- c( 3, -3 )
  bsxy   <- as.data.frame( bsxy )

  # get normative values
  texteval <- "vfenv$nv"
  nv       <- eval( parse( text = texteval ) )

  texteval <- "vfsettings$locini"
  locini   <- eval( parse( text = texteval ) )

  # get settings for the pattern of test locations
  texteval <- paste( "vfsettings$", vf$tpattern[1], sep = "" )
  settings <- eval( parse( text = texteval ) )
  # get x and y locations
  texteval <- paste( vf$tperimetry[1], "locmap$",  vf$tpattern[1], sep = "" )
  patternMap <- eval( parse( text = texteval ) )
  patternMap <- patternMap[,c( "xod", "yod" )]

  # get the conventional color scale
  if( colorMapType == "pval" & is.null( colorScale ) ) {
    colorScale  <- nv$pmapsettings
  }
  if( colorMapType == "slope" & is.null( colorScale ) ) {
    colorScale         <- NULL
    colorScale$cutoffs <- c( -1.5, -1.0, -0.5, 0.5, 1 )
    colorScale$red     <- c( 0.8914642, 0.9999847, 0.9999847, 0.9742432, 0.0000000 )
    colorScale$green   <- c( 0.0000000, 0.5706177, 0.9041748, 0.9355011, 0.9999847 )   
    colorScale$blue    <- c( 0.1622925, 0.1513214, 0.0000000, 0.9213409, 0.9999847 )
    colorScale         <- as.data.frame( colorScale )
  }
  if( colorMapType == "blind" & is.null( colorScale ) ) {
    colorScale         <- NULL
    colorScale$cutoffs <- c( 5, 10, 15, 20 )
    colorScale$red     <- c( 0.8914642, 0.9999847, 0.9999847, 0.9742432 )
    colorScale$green   <- c( 0.0000000, 0.5706177, 0.9041748, 0.9355011 )   
    colorScale$blue    <- c( 0.1622925, 0.1513214, 0.0000000, 0.9213409 )
    colorScale         <- as.data.frame( colorScale )
  }

  ######################
  # analysis
  ######################
  # td and pd vals
  td  <- tdval( vf )
  pd  <- pdval( td )
  # get global indices
  vfindices <- vfstats( vf )
  if( summaryIndex1 == "gh"  | summaryIndex2 == "gh"  ) gh  <- ghpostd( td, correction = TRUE )
  if( summaryIndex1 == "ghr" | summaryIndex2 == "ghr" ) {
    ghr <- rep( NA, nrow( td ) )
    for( i in 1:nrow( td ) ) {
      ghr[i] <- ghranktd( td[i,] )$gh
    }
  }

  # get poplr analysis
  if( plotType == "vf" )    vals <- vf
  if( plotType == "td" )    vals <- td
  if( plotType == "pd" )    vals <- pd
  if( plotType == "pdghr" ) vals <- pdvalghr( td )
  pres <- poplr( vals, nperm = nperm, type = type, truncVal = truncVal, typecomb = typecomb )
  # init
  vfinfo0 <- vf[1,1:( locini - 1 )]
  vfinfo1 <- vf[1,1:( locini - 1 )]
  # get indices for averages
  idx0 <- c( 1:grp )
  idx1 <- c( ( nrow( vf ) - grp + 1 ):nrow( vf ) )
  # Initialize to 1 
  nonSeenLocations <- NULL
  nonSeenLocations[1:( ncol( vf ) - locini + 1 )] <- 1
  # find locations where stimulus is not seen in all of the last n exams
  # mark these locations as 0 
  nonSeenLocations[which( vf[idx1,locini:ncol( vf )] > 0 )] <- 0
  # get all indices of locations which are not seen
  idxNotSeen <- which( nonSeenLocations == 1 )
  # get all x values of blind spot locations from the first n exams that are not zero
  # compute a mean on the set of locations obtained
  idx <- which( vf$sbsx[idx0] != 0 )
  if( length( idx ) > 0 ) vfinfo0$sbsx <- mean( vf$sbsx[idx0[idx]] )
  if( length( idx ) == 0 ) vfinfo0$sbsx <- 0

  # get all x values of blind spot locations from the first n exams that are not zero
  # compute a mean on the set of locations obtained
  idx <- which( vf$sbsy[idx0] != 0 )
  if( length( idx ) > 0 ) vfinfo0$sbsy <- mean( vf$sbsy[idx0[idx]] )
  if( length( idx ) == 0 ) vfinfo0$sbsy <- 0

  # get all x values of blind spot locations from the last n exams that are not zero
  # compute a mean on the set of locations obtained  
  idx <- which( vf$sbsx[idx1] != 0 )
  if( length( idx ) > 0 ) vfinfo1$sbsx <- mean( vf$sbsx[idx1[idx]] )
  if( length( idx ) == 0 ) vfinfo1$sbsx <- 0

  # get all x values of blind spot locations from the last n exams that are not zero
  # compute a mean on the set of locations obtained  
  idx <- which( vf$sbsy[idx1] != 0 )
  if( length( idx ) > 0 ) vfinfo1$sbsy <- mean( vf$sbsy[idx1[idx]] )
  if( length( idx ) == 0 ) vfinfo1$sbsy <- 0
  vfinfo0$sage <- mean( vf$sage[idx0] )
  vfinfo1$sage <- mean( vf$sage[idx1] )
  locvalsidx   <- locini:( locini + settings$locnum - 1 )
  vf0          <- colMeans( vf[idx0, locvalsidx] )
  vf1          <- colMeans( vf[idx1, locvalsidx] )

  # open window wiht A4 page
  if( is.null( filename ) ) {
    device <- options( "device" )
    if( .Platform$OS.type == "unix" ) {
      if( Sys.info()["sysname"] == "Darwin" ) {
        options( device = "quartz" )
        dev.new( width = pwidth, height = pheight, dpi = 85 )
      } else {
        options( device = "x11" )
        dev.new( width = pwidth, height = pheight )
      }
    } else{
      options( device = "windows" )
      dev.new( width = pwidth, height = pheight, rescale = "fixed" )
    }
    options( device = device )
  } else {
    pdf( width = pwidth, height = pheight, file = filename )
  }

  # define the margins
  mwidth  <- pwidth  - 2 * margin
  mheight <- pheight - 2 * margin
  
  # create the layout of the printout
  printout <- createviewport( "printout", left = margin, top = margin, height = mheight, width = mwidth )

  xrange <- max( patternMap$xod ) - min( patternMap$xod )
  yrange <- max( patternMap$yod ) - min( patternMap$yod )
  xmin   <- min( patternMap$xod ) - 0.025 * xrange
  xmax   <- max( patternMap$xod ) + 0.025 * xrange
  ymin   <- min( patternMap$yod ) - 0.025 * yrange
  ymax   <- max( patternMap$yod ) + 0.025 * yrange

  color0 <- vfgrayscale( vf0, vfinfo0$sage, pattern = vfinfo0$tpattern, algorithm = vfinfo0$talgorithm )
  color1 <- vfgrayscale( vf1, vfinfo1$sage, pattern = vfinfo0$tpattern, algorithm = vfinfo0$talgorithm )
  # add void points if not in the test of locations
  for( i in 1:nrow( bsxy ) ) {
    if( xmin < bsxy$x[i] & xmax > bsxy$x[i] & ymin < bsxy$y[i] & ymax > bsxy$y[i] ) {
      idx <- which( patternMap$xod == bsxy$x[i] & patternMap$yod == bsxy$y[i] )
      if( length( idx ) > 0 ) {
        color0[idx,] <- c( 0, 0, 0 )
        color1[idx,] <- c( 0, 0, 0 )
      } else {
        patternMap <- rbind( patternMap, c( bsxy$x[i], bsxy$y[i] ) )
        vf0    <- c( vf0,  NA )
        vf1    <- c( vf1,  NA )
        color0 <- rbind( color0, c( 0, 0, 0 ) )
        color1 <- rbind( color1, c( 0, 0, 0 ) )
      }
    }
  }

  if( vf$seye[1] == "OS" ) {
    xmin2 <- xmin
    xmin  <- -xmax
    xmax  <- -xmin2
    patternMap$xod <- -patternMap$xod
  }
  # get the Voronoi tesselation tiles tiles
  vftess  <- vftessellation( patternMap, dist = 3 )
  vftiles <- tile.list( vftess[[1]] )
  vfhull  <- vftess[[2]]

  ######################################################
  # first plot all graphs
  ######################################################
  # sensitivity plot first n visits
  opar <- par( no.readonly = TRUE )
  par( fig = c( 0.10, 0.45, 0.60, 0.90 ) )
  vftxt <- round( vf0 )
  vftxt[which( vftxt < 0 )] <- "<0"
  vfplotloc( vftxt, patternMap, vftiles = vftiles, vfhull = vfhull, loccol = color0,
             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pointsize = txtsize,
             showaxis = showaxis, colaxis = colaxis )
  # sensitivity plot last n visits
  par( new = TRUE )
  par( fig = c( 0.50, 0.85, 0.60, 0.90 ) )
  vftxt[which( vf1 < 0 )] <- "<0"
  vftxt[which( vftxt < 0 )] <- "<0"
  vfplotloc( vftxt, patternMap, vftiles = vftiles, vfhull = vfhull, loccol = color1,
             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pointsize = txtsize,
             showaxis = showaxis, colaxis = colaxis )
  # LEGO plot
  par( new = TRUE )
  par( fig = c( 0.10, 0.45, 0.30, 0.60 ) )
  vftxt <- round( vf1 - vf0, 1 ) # IMF: Paul, need to decide what to do with sensitivities <0. Let's talk
  vfplot_legoplot( vftxt, patternMap = patternMap, vftiles = vftiles, vfhull = vfhull,
                   loccolout = color0, loccolin = color1, radius = 2.25, # radius is the radius in same units as x-axis (degrees of visual angle) of the circle
                   xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pointsize = txtsize,
                   showaxis = showaxis, colaxis = colaxis )
  # PLR plot
  par( new = TRUE )
  par( fig = c( 0.50, 0.85, 0.30, 0.60 ) )
  vfplot_plr( pres$sl, pres$pval, pres$vfdata, colorMapType = colorMapType, colorScale = colorScale, pointsize = txtsize,
              showaxis = showaxis, colaxis = colaxis )
  vfplot_sparklines( vf, patternMap, col = "black" )
  # color-code map
  if( colorMapType == "slope" ) colorScale$cutoffs <- 10 * colorScale$cutoffs
  par( new = TRUE )
  par( fig = c( 0.86, 0.90, 0.37, 0.53 ) )
  colormapgraph( ncol = 1, mapval = colorScale, symbol = "square", inch = 0.3, pointsize = txtsize, notSeenAsBlack = FALSE )
  # plot permutation histogram
  par( new = TRUE )
  par( fig = c( 0.10, 0.35, 0.07, 0.23 ) )
  hist_poplr( pres$scomb_obs, pres$pcomb_obs, pres$scomb )
  # general index 1
  par( new = TRUE )
  par( fig = c( 0.375, 0.625, 0.07, 0.23 ) )
  if ( summaryIndex1 == "md" )  progols( vfindices$tdate, vfindices$mtdev, projyears = 0, ylab = "md, dB",  markfl = TRUE, prggrp = grp )  # regression analysis
  if ( summaryIndex1 == "ms" )  progols( vfindices$tdate, vfindices$msens, projyears = 0, ylab = "ms, dB",  markfl = TRUE, prggrp = grp )  # regression analysis
  if ( summaryIndex1 == "gh" )  progols( vfindices$tdate, gh,              projyears = 0, ylab = "gh, dB",  markfl = TRUE, prggrp = grp )  # regression analysis
  if ( summaryIndex1 == "ghr" ) progols( vfindices$tdate, ghr,             projyears = 0, ylab = "ghr, dB", markfl = TRUE, prggrp = grp ) # regression analysis
  # plot general height on age
  par( new = TRUE )
  par( fig = c( 0.65, 0.9, 0.07, 0.23 ) )
  if ( summaryIndex2 == "md" )  progols( vfindices$tdate, vfindices$mtdev, projyears = 0, ylab = "md, dB",  markfl = TRUE, prggrp = grp )  # regression analysis
  if ( summaryIndex2 == "ms" )  progols( vfindices$tdate, vfindices$msens, projyears = 0, ylab = "ms, dB",  markfl = TRUE, prggrp = grp )  # regression analysis
  if ( summaryIndex2 == "gh" )  progols( vfindices$tdate, gh,              projyears = 0, ylab = "gh, dB",  markfl = TRUE, prggrp = grp )  # regression analysis
  if ( summaryIndex2 == "ghr" ) progols( vfindices$tdate, ghr,             projyears = 0, ylab = "ghr, dB", markfl = TRUE, prggrp = grp ) # regression analysis
  par( opar )

  ######################################################
  # create the text elements in the printouts
  ######################################################
  # The two above are to delete once the graphs are generated!!!
  mainInfo         <- createviewport( "mainInfo",          left =  0.45, top =  0.00, width = 4.75, height = 0.40, pheight = mheight, pwidth = mwidth )
  infobox2         <- createviewport( "infobox2",          left =  5.40, top =  0.00, width = 1.40, height = 0.40, pheight = mheight, pwidth = mwidth )
  infobox3         <- createviewport( "infobox3",          left =  3.40, top = 10.90, width = 1.40, height = 0.30, pheight = mheight, pwidth = mwidth )
  textvisit0       <- createviewport( "textvisit0",        left =  1.50, top =  1.05, width = 1.40, height = 0.30, pheight = mheight, pwidth = mwidth )
  textvisit1       <- createviewport( "textvisit1",        left =  4.90, top =  1.05, width = 1.40, height = 0.30, pheight = mheight, pwidth = mwidth )
  textlego         <- createviewport( "textlego",          left =  1.50, top =  4.55, width = 1.40, height = 0.30, pheight = mheight, pwidth = mwidth )
  textpoplar       <- createviewport( "textpoplar",        left =  4.90, top =  4.55, width = 1.40, height = 0.30, pheight = mheight, pwidth = mwidth )
  texthistogram    <- createviewport( "texthistogram",     left =  1.25, top =  8.40, width = 1.00, height = 0.30, pheight = mheight, pwidth = mwidth )
  textglobalindex1 <- createviewport( "textglobalindex1",  left =  3.50, top =  8.40, width = 1.00, height = 0.30, pheight = mheight, pwidth = mwidth )
  textglobalindex2 <- createviewport( "textglobalindex2",  left =  5.75, top =  8.40, width = 1.00, height = 0.30, pheight = mheight, pwidth = mwidth )
  # create the list and then generate the tree and "push" it
  list <- vpList( mainInfo, infobox2, infobox3, textvisit0, textvisit1, textlego, textpoplar, texthistogram, textglobalindex1, textglobalindex2 )
  tree <- vpTree( printout, list )
  pushViewport( tree )

  ######################################################
  # perimetry information
  ######################################################
  seekViewport( "mainInfo" )
  text <- vfinfo0$tperimetry
  if( text == "sap" ) {
    text = "Static Automated Perimetry."
  } else if( text == "fdp" ) {
    text = "Frequency-doubling Perimetry."
  } else if( text == "csp" ) {
    text = "Contrast sensitivity Perimetry."
  }
  text <- paste( text, "PoPLR progression analysis", sep = " " )
  # ID
  text <- paste( text, "Subject ID: ", sep = "\n" )
  text <- paste( text, vfinfo0$id, ",", sep = "" )
  # age
  text <- paste( text, " age: from ", round( vfinfo0$sage ), " to ", round( vfinfo1$sage ), ",", sep = "" )
  # eye
  texteye <- paste( "eye:", vfinfo0$seye, sep = " " )
  if( vfinfo0$seye == "OD" ) {
    texteye <- paste( texteye, "(right)", sep = " " )
  } else if ( vfinfo0$seye == "OS" ) {
    texteye <- paste( texteye, "(left)", sep = " " )
  } else {
    texteye <- paste( texteye, "(which?)", sep = " " )
  }
  text <- paste( text, texteye, sep = " " )
  grid.text( text, x = 0.0, y = 1.0, just = c( "left", "top" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )
  
  ######################################################
  # Details about printouts
  ######################################################
  seekViewport( "infobox2" )
  if( vfinfo0$tpattern == "p24d2" ) {
    textpattern <- "Central 24-2"
  } else if( vfinfo0$tpattern == "p30d2" ) {
    textpattern <- "Central 30-2"
  } else if( vfinfo0$tpattern == "p10d2" ) {
    textpattern <- "Central 10-2"
  } else if( vfinfo0$tpattern == "rnfl" ) {
    textpattern <- "RNFL-57"
  } else {
    textpattern <- "Unknown"
  }
  # algorithm
  if( vfinfo0$talgorithm == "sitas" ) {
    textalgorithm <- "SITA standard"
  } else if( vfinfo0$talgorithm == "sitaf" ) {
    textalgorithm <- "SITA fast"
  } else if( vfinfo0$talgorithm == "fullt" ) {
    textalgorithm <- "Full threshold"
  } else {
    textalgorithm <- "Unknown"
  }
  text <- paste( textpattern, textalgorithm, sep = "\n" )
  grid.text( text, x = 1.00, y = 1.00, just = c( "right", "top" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize ) )
  
  ######################################################
  # Details about first and last visits
  ######################################################
  seekViewport( "textvisit0" )
  text <- paste( "first ", as.character( grp ), " exams", sep = "" )
  if( grp == 1 ) text <- "first exam"
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )

  seekViewport( "textvisit1" )
  text <- paste( "last ", as.character( grp ), " exams", sep = "" )
  if( grp == 1 ) text <- "last exam"
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )
  
  ######################################################
  # Text for Lego-plot and PoPLR plot
  ######################################################
  seekViewport( "textlego" )
  text <- "legoplot"
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )

  seekViewport( "textpoplar" )
  if( plotType == "vf" )    text <- "PLR slope (sensitivity)"
  if( plotType == "td" )    text <- "PLR slope (total deviation)"
  if( plotType == "pd" )    text <- "PLR slope (pattern deviation)"
  if( plotType == "pdghr" ) text <- "PLR slope (rank pattern deviation)"
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )

  ######################################################
  # Text for permutation histogram
  ######################################################
  seekViewport( "texthistogram" )
  text <- paste( "permutation histogram", sep = "" )
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )

  ######################################################
  # Texts for global index plots
  ######################################################
  seekViewport( "textglobalindex1" )
  if( summaryIndex1 == "ms")  text <- paste( "mean sensitivity", sep = "" )
  if( summaryIndex1 == "md")  text <- paste( "mean deviation", sep = "" )
  if( summaryIndex1 == "gh")  text <- paste( "general height", sep = "" )
  if( summaryIndex1 == "ghr") text <- paste( "general height rank", sep = "" )
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )

  seekViewport( "textglobalindex2" )
  if( summaryIndex2 == "ms")  text <- paste( "mean sensitivity", sep = "" )
  if( summaryIndex2 == "md")  text <- paste( "mean deviation", sep = "" )
  if( summaryIndex2 == "gh")  text <- paste( "general height", sep = "" )
  if( summaryIndex2 == "ghr") text <- paste( "general height rank", sep = "" )
  grid.text( text, x = 0.50, y = 0.50, just = c( "center", "center" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize, fontface = "bold" ) )

  ######################################################
  # Details about printouts
  ######################################################
  seekViewport( "infobox3" )
  
  text <- paste( "norm vals: ", nv$nvname, sep = "" )
  text <- paste( text, substr( packageDescription( "visualFields" )$Date, 1, 4 ), sep = "\n" )
  text <- paste( text, "visualFields", packageDescription( "visualFields" )$Version, sep = " " )
  grid.text( text, x = 0.50, y = 0.00, just = c( "center", "bottom" ), gp = gpar( fontfamily = txtfont, fontsize = pointsize ) )
  
  # only if in save mode, then set device to off
  if( !is.null( filename ) ) {
    dev.off()
  }
}