#' Import visual field data from Haag-Streit Eyesuite
#' 
#' \code{loadvfEyesuite} imports visual field data from the Eyesuite software by Haag-Streit. These data are converted into a vf object.
#' 
#' @param filename the filename of the csv-file
#' @param date_order the order of the dates used in the specific locale
#' @return a vf object

loadvfEyesuite <- function(filename, date_order = "dmy") {

  eyesuiteNames <- c(
    "id", "lastname", "firstname", "dateofbirth", "gender", "ethnicity", 
    "V7", "V8", "V9", "V10",
    "apparatus", "serial_number", "V13", "V14", "V15", "V16", "V17", 
    "eye", "pattern", "stimulus_size", "stimulus_duration", "stiumulus_luminance",
    "strategy", "tperimetry", "V25", "testduration", "testdate", 
    "test_starting_time", "reliability_factor", "locnum", "questions", 
    "repetitions", "positive_catch_trials", "false_positives", "negative_catch_trials", 
    "false_negatives", "notes", "sphere", "cylinder", "axis", "bcva"
  )
  
  # Read the csv-file exported by EyeSuite
  vFieldsRaw <- 
    data.table(
      read.csv2(filename, header = F, quote = "", stringsAsFactors = F, fill = T,
                col.names = paste("V", 1:2000, sep = ""))
    )
  
  names(vFieldsRaw)[seq(eyesuiteNames)] <- eyesuiteNames
  
  # use factors to replace values
  # the data.table function ":=" for replacing by reference is used to safe time
  vFieldsRaw[, eye := factor(eye, levels = c(0, 1, 3), labels = c("OD", "OS", "binocular"))]
  vFieldsRaw[, strategy := factor(strategy, levels = c(0, 1, 2, 3, 4, 6, 11), 
                             labels = c("normal", "dynamic", "2LT/normal", "low vision", "1LT", "TOP", "GATE"))]
  vFieldsRaw[, pattern := factor(pattern, levels = c("G", "BT"),
                                 labels = c("pG1", "BT"))]
  vFieldsRaw[, tperimetry := factor(tperimetry, levels = c(0, 1), labels = c("sap", "swap"))]
  
  # add numbers for each visual field
  vFieldsRaw[, i := .I]
  
  #extract the header information (id, tperimetry, ...)
  header <- vFieldsRaw[, .SD, .SDcols = id:bcva]
  header[, i := .I]
  
  # create a data table from the pattern
  patternMatrix <- data.table(saplocmap$pG1[, c("loc", "xod", "yod")])
  
  # exclude binocular visual fields
  if(any(vFieldsRaw$eye == "binocular")) {
    warning("Binocular visual fields are not supported! Binocular visual fields have been removed.")
    vFieldsRaw <- vFieldsRaw[eye != "binocular", ]
  }
  
  #function to extract sensitivities for the different loci
  extractLocations <- function(tLine) {
    
    locMatrix <- as.data.table(matrix(unlist(tLine[, 44:338]) / 10, 59, 5, byrow = T))
    names(locMatrix) <- c("xod", "yod", "sens1", "sens2", "norm")
    
    if(tLine[1, 18] == "OS") locMatrix[, xod := -xod]
    
    combinedTable <- locMatrix[patternMatrix, on = c("xod", "yod")]
    returnTable <- apply(combinedTable[, .(sens1, sens2)], 1, mean, na.rm = T)
    names(returnTable) <- paste("L", as.character(combinedTable$loc), sep = "")
    rValue <- as.data.table(t(as.matrix(returnTable, 1, 59)))
    return(rValue)
    
  }
  
  # apply the extractLocations function on each row
  vFieldsRaw <- vFieldsRaw[, extractLocations(.SD), by = i]
  
  # merge sensitivity data with header
  vFieldsRaw <- header[vFieldsRaw, on = "i"]
  vFieldsRaw[, i := NULL]
  
  # convert all text columns into the correct class
  vFieldsRaw[, id := as.character(id)]
  vFieldsRaw[, tperimetry := as.character(tperimetry)]
  vFieldsRaw[, talgorithm := as.character(strategy)]
  vFieldsRaw[, tpattern := as.character(pattern)]
  vFieldsRaw[, tdate := parse_date_time(as.character(testdate), date_order)]
  vFieldsRaw[, ttime := as.character(test_starting_time)]
  vFieldsRaw[, stype := as.character(notes)]
  vFieldsRaw[, sage := agecalc(parse_date_time(dateofbirth, date_order), parse_date_time(testdate, date_order))]
  vFieldsRaw[, seye := as.character(eye)]
  vFieldsRaw[, sbsx := 15]
  vFieldsRaw[, sbsy := -3]
  vFieldsRaw[, sfp := false_positives / positive_catch_trials]
  vFieldsRaw[, sfn := false_negatives / negative_catch_trials]
  vFieldsRaw[, sfl := NA]
  vFieldsRaw[, sduration := as.character(testduration)]
  vFieldsRaw[, spause := NA]
  
  #exract final table
  finalIndex <- c("id", "tperimetry", "talgorithm", "tpattern", "tdate", "ttime", "stype", "sage", "seye", "sbsx", "sbsy", "sfp",
                  "sfn", "sfl", "sduration", "spause")
  finalIndex <- c(finalIndex, paste("L", 1:59, sep = ""))
  
  # return vf-object
  return(vFieldsRaw[, ..finalIndex])
  
}



