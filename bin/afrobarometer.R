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


NA_VALUES <- list(
  "Don't know",
  "Missing",
  "Refused",
  "Not applicable",
  "Not asked in country",
  "Don't know / did not understand the question",
  "Don't know / haven't heard enough to say"
)

default_clean <- function(x) {
  if (haven::is.labelled(x)) {
    x <- as.character(haven::as_factor(x))
  }
  if (is.character(x)) {
    # Common patterns to replace in all cases
    x <- str_replace_all(x, c("\\s+" = " ",
                              "[`’]" = "'",
                              " */ *" = " / "))
    # Converting to ASCII since accents and UTF is used inconsistently
    x <- stringi::stri_trans_general(x, "Any-latin; Latin-ascii")
    # Make missing values consistent
    x <- str_replace(x, regex("^don't know$", ignore_case = TRUE),
                     "Don't know") %>%
       str_replace(regex("^refused( to answer)?$", ignore_case = TRUE),
                         "Refused") %>%
       str_replace(regex("^don[']t know ?/ ?haven[']t heard enough( to say)?$",
                         ignore_case = TRUE),
                   "Don't know") %>%
       str_replace(regex("^don[']t know ?/ ?did not understand the question$",
                         ignore_case = TRUE),
                   "Don't know") %>%
       str_replace(regex("^no further (answer|reply)$", ignore_case = TRUE),
                   "No further answer") %>%
       str_replace(regex("^(not applicable|NA |N / A).*$", ignore_case = TRUE),
                   "Not applicable") %>%
       str_replace(regex("^not asked in .*$", ignore_case = TRUE),
                   "Not asked in country") %>%
       str_replace(regex("^missing( data)?$", ignore_case = TRUE),
                   "Missing")

    # trim extra whitespace
    x <- str_trim(x)
  }
  x
}

clean_afrobarometer <- function(.round) {
  cat(glue("Processing round {.round}."), '\n')
  # generate the list of variable names to select
  variables <-
    map(VARIABLE_MAP, c("variables", str_c("r", .round))) %>%
    discard(is.null) %>%
    map(str_to_lower) %>%
    map(rlang::sym)

  # extract the subset of variables that are needed/wanted
  out <- haven::read_sav(AFROBAROMETER_FILENAMES[.round]) %>%
    rename_all(str_to_lower) %>%
    select(!!!variables)

  # apply cleaning functions to each variable
  for (i in names(out)) {
    varinfo <- VARIABLE_MAP[[i]]
    cat("\t", glue("cleaning variable {i}"), "\n")
    out[[i]] <- default_clean(out[[i]])

    # Replace any patterns
    if (!is.null(varinfo[["patterns"]])) {
      for (pattern in names(varinfo[["patterns"]])) {
        out[[i]] <- str_replace_all(out[[i]], pattern,
                                    varinfo[["patterns"]][[pattern]])
      }
    }
    # Replace any mappings
    if (!is.null(varinfo[["mappings"]])) {
      out[[i]] <- recode(out[[i]], !!!varinfo[["mappings"]])
    }
    # create variable with missing value types
    # match in a case-insenitive manner
    is_missing <- str_to_lower(out[[i]]) %in% str_to_lower(NA_VALUES)
    if (any(is_missing)) {
      out[[str_c(i, "_NA")]] <-
        if_else(is_missing,
                recode(str_to_lower(out[[i]]),
                       !!!as.list(set_names(NA_VALUES, str_to_lower(NA_VALUES)))
                       ),
                NA_character_)
      # Ensure that missing values are in
      out[[i]] <- if_else(is_missing, NA_character_, out[[i]])
    }
    # Lower case strings
    if (truthy(varinfo[["lower"]])) {
      out[[i]] <- str_to_lower(out[[i]])
    }
    # If it's a string / factor type
    if (varinfo[["type"]] %in% c("string")) {
      # enum constraint = factor
      if (!is.null(varinfo[["constraints"]][["enum"]])) {
        lvls <- varinfo[["constraints"]][["enum"]]
        badlvls <- base::setdiff(unique(na.omit(out[[i]])), lvls)
        if (length(badlvls)) {
          stop("Invalid levels of ", i, " found: ",
               str_c(sQuote(badlvls), collapse = ", "), "\n", call. = FALSE)
        }
        # ordered means what it says
        if (!is.null(varinfo[["ordered"]]) && varinfo[["ordered"]]) {
          out[[i]] <- ordered(out[[i]], levels = lvls)
        } else {
          out[[i]] <- factor(out[[i]], levels = lvls)
        }
      }
    } else if (varinfo[["type"]] == "number") {
      out[[i]] <- as.numeric(out[[i]])
    } else if (varinfo[["type"]] == "integer") {
      out[[i]] <- as.integer(out[[i]])
    } else if (varinfo[["type"]] == "date") {
      out[[i]] <- as.Date(out[[i]])
    }
  }

  # Calculate LPI. I think it is just the mean of the components.
  out <- mutate(out, lpi = (as.numeric(lpi_food) +
                             as.numeric(lpi_water) +
                             as.numeric(lpi_medical) +
                             as.numeric(lpi_fuel) +
                             as.numeric(lpi_cash)) / 5 - 1)

  # Any other cleanup code
  out %>%
    mutate(round = UQ(.round))
}

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

afrob <- map_df(AFROBAROMETER_ROUNDS, clean_afrobarometer) %>%
  select(round, respno, country, everything())

write_rds(afrob, OUTFILE)

