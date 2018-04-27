.onLoad <- function(libname = find.package("visualFields"), pkgname = "visualFields"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      c("." , "..finalIndex", ".I", ".SD", "bcva", "dateofbirth", "eye", "false_negatives",
        "false_positives", "i", "id", "negative_catch_trials", "notes", "pattern",
        "positive_catch_trials", "questions", "read.csv2", "repetitions", "sage", "saplocmap",
        "sbsx", "sbsy", "sduration", "sens1", "sens2", "seye", "sfl", "sfn", "sfp", "spause", "strategy",
        "stype", "talgorithm", "tdate", "test_starting_time", "testdate", "testduration",
        "tpattern", "tperimetry", "ttime", "xod"
      )
    )
  invisible()
}