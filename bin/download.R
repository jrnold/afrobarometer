#!/usr/bin/env Rscript
#' ---
#' title: Download Afrobarometer Data
#' ---
#'
#' Download raw data from Afrobarometer and WALS datasets
library("tidyverse")
library("yaml")
library("httr")

#'
#' ## Afrobarometer
#'
#' Now use merged afrobarometer
#'

AFROBAROMETER_DIR <- here::here("data-raw", "external", "afrobarometer")

AFROBAROMETER_URLS <- list(
  "r1" = "https://afrobarometer.org/sites/default/files/data/round-1/merged_r1_data.sav",
  "r2" = "https://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav",
  "r3" = "https://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav",
  "r4" = "https://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav",
  "r5" = "https://afrobarometer.org/sites/default/files/data/round-5/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
  "r6" = "https://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav"
)

CODEBOOK_URLS <- list(
  "r1" = "http://afrobarometer.org/sites/default/files/data/round-1/merged_r1_codebook2.pdf",
  "r2" =  "http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_codebook2.pdf",
  "r3" = "http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_codebook2_0.pdf",
  "r4" = "http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_codebook3.pdf",
  "r5" = "http://afrobarometer.org/sites/default/files/data/round-5/merged_r5_codebook_2.pdf",
  "r6" = "http://afrobarometer.org/sites/default/files/data/round-6/merged_round_6_codebook_20161.pdf"
)


download_afrobarometer_file <- function(src, dst, authentication) {
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(dst, basename(src))
  if (!file.exists(outfile)) {
    message("Downloading ", src, "\n")
    resp <- GET(src, add_headers("user-agent" = "Mozilla/5.0"),
                write_disk(outfile, overwrite = TRUE),
                authentication)
    status <- status_code(resp)
    message("Status code: ", status, "\n")
    if (status == 200) {
      message("Wrote to ", outfile, "\n")
    } else {
      stop("Failed to download \n", call. = FALSE)
    }
    outfile
  } else {
    message(sprintf("%s already exists; not downloading it\n", dst))
  }
}

#' Create authentication object from a user's name and password.
#'
#' ```yaml
#' user: username
#' password: password
#' type: "basic"
#' ```
#'
authentication <- yaml::yaml.load_file(here::here("afrobarometer.yml")) %>%
  invoke(authenticate, .)

#' Download all afrobarometer datasets
AFROBAROMETER_URLS %>%
  walk(download_afrobarometer_file, dst = AFROBAROMETER_DIR,
       authentication)

#' Download all afrobarometer Codebooks
CODEBOOK_URLS %>%
  walk(download_afrobarometer_file, dst = AFROBAROMETER_DIR,
       authentication)

