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
      read.csv2(filename, header = F, quote = "", stringsAsFactors = F)
    )
  
  names(vFieldsRaw)[seq(eyesuiteNames)] <- eyesuiteNames
  
  # vFieldsRaw[locnum %in% c(59, 73), ]
  
  # use factors to replace values
  # the data.table function for replacing by reference is used to safe time
  vFieldsRaw[, eye := factor(eye, levels = c(0, 1), labels = c("OD", "OS"))]
  vFieldsRaw[, strategy := factor(strategy, levels = c(0, 1, 2, 3, 4, 6, 11), 
                             labels = c("normal", "dynamic", "2LT/normal", "low vision", "1LT", "TOP", "GATE"))]
  vFieldsRaw[, pattern := factor(pattern, levels = c("G", "BT"),
                                 labels = c("pG1", "BT"))]
  vFieldsRaw[, tperimetry := factor(tperimetry, levels = c(0, 1), labels = c("sap", "swap"))]
  
  locnames <- paste("L", 1:59, sep = "")
  header <- vFieldsRaw[, 1:43]
  header[, i := .I]
  
  ind1 <- c(rep(FALSE, 43), rep(c(FALSE, FALSE, FALSE, TRUE, FALSE), 59))
  ind2 <- c(rep(FALSE, 43), rep(c(FALSE, FALSE, TRUE, FALSE, FALSE), 59))
  phase1 <- vFieldsRaw[, ..ind1] / 10
  setnames(phase1, locnames)
  phase1[, i := .I]
  phase2 <- vFieldsRaw[, ..ind2] / 10
  setnames(phase2, locnames)
  phase2[, i := .I]
  l = list(phase1, phase2)
  vFieldsRaw <- rbindlist(l, use.names = T)
  vFieldsRaw <- vFieldsRaw[, lapply(.SD, mean, na.rm = T), by = i]
  
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
  vFieldsRaw[, fsl := NA]
  vFieldsRaw[, sduration := as.character(testduration)]
  vFieldsRaw[, spause := NA]
  
  finalIndex <- c("id", "tperimetry", "talgorithm", "tpattern", "tdate", "ttime", "stype", "sage", "seye", "sbsx", "sbsy", "sfp",
                  "sduration", "spause")
  finalIndex <- c(finalIndex, paste("L", 1:59, sep = ""))
  
  # return vf-object
  return(vFieldsRaw[, ..finalIndex])
  
}



