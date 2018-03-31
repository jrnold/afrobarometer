#' ---
#' title: Clean and Merge Afrobarometer Surveys
#' ---
#'
#' Cleans and concatenates multiple rounds of Afrobarometer data and
#' outputs to an `.rds` file.
#'
suppressPackageStartupMessages({
  library("tidyverse")
  library("glue")
  library("stringr")
})


OUTDIR <- here::here("data")
OUTFILE <- file.path(OUTDIR, "afrobarometer.rds")
AFROBAROMETER_ROUNDS <- 3:6

cat(glue("Merging rounds {str_c(AFROBAROMETER_ROUNDS, collapse = ',')}\n\n"))

truthy <- function(x) {
  !is.null(x) && length(x) && as.logical(x)
}

# Questions has basic mappings from
VARIABLE_MAP <-
  yaml::yaml.load_file("data-raw/afrobarometer-variables.yml")

AFROBAROMETER_FILENAMES <-
  list("merged_r1_data.sav",
      "merged_r2_data.sav",
      "merged_r3_data.sav",
      "merged_r4_data.sav",
      "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
      "merged_r6_data_2016_36countries2.sav") %>%
  map_chr(~ here::here("data-raw", "external", "afrobarometer", .x))

#' Wrapper to read and return the Afrobarometer data for a round
#'
read_afrobarometer <- function(r = 6L) {
  haven::read_sav(AFROBAROMETER_FILENAMES[r]) # nolint
}


# Additional transformations
cleaners <- list()

#' Regularize all whitespace
regularize_whitespace <- function(x) str_replace_all(x, "\\s+", " ")

#' Remove spaces around forward slashes
regularize_slashes <- function(x) str_replace_all(x, "\\s*/\\s*", "/")

#' Regularize quotes
regularize_quotes <- function(x) {
  str_replace_all(x, c("[`’]" = "'", "[“”]" = '"'))
}

#' Remove accents etc
to_ascii <- function(x) {
  stringi::stri_trans_general(x, "Any-latin; Latin-ascii")
}

NA_VALUES <- list(
  "Don't know",
  "Missing",
  "Refused",
  "Not applicable",
  "Not asked in country"
)

NA_PATTERNS <- list(
  "^don't know$" = "Don't know",
  "^refused( to answer)?$" = "Refused",
  "^don[']t know ?/ ?haven[']t heard enough( to say)?$" = "Don't know",
  "^don[']t know ?/ ?did not understand the question$" = "Don't know",
  "^no further (answer|reply)$" = "No further answer",
  "^(not applicable|NA |N / A).*$" = "Not applicable",
  "^not asked in .*$" = "Not asked in country",
  "^missing( data)?$" = "Missing")

clean_na <- function(x) {
  for (i in seq_along(NA_PATTERNS)) {
    x <- str_replace_all(x, regex(names(NA_PATTERNS)[[i]], ignore_case = TRUE),
                         NA_PATTERNS[[i]])
  }
  x
}

clean_variable <- function(x, config) {
  if (haven::is.labelled(x)) {
    x <- as.character(haven::as_factor(x))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  # process transformations
  pp <- config[["preprocess"]]
  if (!is.null(pp)) {
    if (is.character(x)) {
      x <- regularize_whitespace(x)
      x <- regularize_quotes(x)
      x <- regularize_slashes(x)
      x <- to_ascii(x)
      x <- clean_na(x)
      # Replace any patterns
      if (!is.null(pp[["patterns"]])) {
        for (pattern in names(pp[["patterns"]])) {
          x <- str_replace_all(x, pattern, pp[["patterns"]][[pattern]])
        }
      }
      # Replace any exact matches
      if (!is.null(pp[["mappings"]])) {
        x <- dplyr::recode(x, !!!pp[["mappings"]])
      }

      if (!is.null(config[["case"]])) {
        if (config[["case"]] == "lower") {
          x <- str_to_lower(x)
        } else if (case == "upper") {
          x <- str_to_upper(x)
        } else if (case == "title") {
          x <- str_to_title(x)
        } else {
          stop("Invalid case: ", config[["case"]], "\n", .call = FALSE)
        }
      }
    }
  }
  if (config[["type"]] == "number") {
    x <- as.numeric(x)
  } else if (config[["type"]] == "integer") {
    x <- as.integer(x)
  } else if (config[["type"]] == "date") {
    x <- as.Date(x)
  }

  x
}

lpi <- function(x) {
  mutate(out, lpi = (as.numeric(lpi_food) +
                       as.numeric(lpi_water) +
                       as.numeric(lpi_medical) +
                       as.numeric(lpi_fuel) +
                       as.numeric(lpi_cash)) / 5 - 1)
}

clean_afrobarometer <- function(.round) {
  cat(glue("Processing round {.round}."), '\n')
  # generate the list of variable names to select
  variables <-
    discard(VARIABLE_MAP, ~ is.null(.x[["variables"]][[str_c("r", .round)]]))

  # extract the subset of variables that are needed/wanted
  out <- haven::read_sav(AFROBAROMETER_FILENAMES[.round]) %>%
    rename_all(str_to_lower)

  # apply cleaning functions to each variable
  for (i in names(variables)) {
    varinfo <- VARIABLE_MAP[[i]]
    varname <- str_to_lower(varinfo[["variables"]][[str_c("r", .round)]])
    cat("\t", glue("cleaning variable {i}"), "\n")
    out[[i]] <- clean_variable(out[[varname]], varinfo)
  }

  # Any other cleanup code goes HERE

  out %>%
    select(one_of(names(variables))) %>%
    mutate(round = UQ(.round))
}

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

afrob <- map_df(AFROBAROMETER_ROUNDS, clean_afrobarometer) %>%
  select(round, respno, country, everything())

write_rds(afrob, OUTFILE)

