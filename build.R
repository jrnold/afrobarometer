#' ---
#' title: Build the Merged Afrobarometer data-set
#' ---
#'
#' This will run all scripts necessesary to create the merged data.
#'

files <- c("bin/download.R", "bin/clean-afrobarometer.R")
for (file in files) {
  # source each script in a new environment to ensure that they are self-contained.
  sys.source(file, envir = rlang::env())
}
