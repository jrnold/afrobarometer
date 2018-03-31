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

NA_FACTOR <- "Missing"
NA_PATTERNS <- list(
  "Don't know" = c(
    "^don[']?t\\s+know.*$"
  ),
  "Missing" = c(
    "^missing( data)?$"
  ),
  "Refused" = c(
    "^refused( to answer)?$"
  ),
  "No further answer" = c(
    "^no further (answer|reply)$"
  ),
  "Not applicable" = c(
    "^not applicable.*$",
    "^NA/No opposition$",
    "^N/A$"
  ),
  "Not asked in country" = c(
    "^not asked in .*$"
  )
)

clean_na <- function(x) {
  for (replacement in names(NA_PATTERNS)) {
    for (pattern in NA_PATTERNS[[replacement]]) {
        x <- str_replace_all(x, regex(pattern, ignore_case = TRUE), replacement)
    }
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
  if (is.character(x)) {
    x <- regularize_whitespace(x)
    x <- regularize_quotes(x)
    x <- regularize_slashes(x)
    x <- to_ascii(x)
    x <- clean_na(x)
  }
  pp <- config[["preprocess"]]
  if (!is.null(pp)) {
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
  if (config[["type"]] == "number") {
    x <- as.numeric(x)
  } else if (config[["type"]] == "integer") {
    x <- as.integer(x)
  } else if (config[["type"]] == "date") {
    x <- as.Date(x)
  } else if (config[["type"]] == "factor") {
    # Levels
    lvls <- config[["factor"]][["levels"]]
    x <- if_else(is.na(x), NA_FACTOR, x)
    # Levels associated with missing values
    missing_values <- unname(config[["factor"]][["missing"]] %||% character(0))
    all_lvls <- c(lvls, missing_values)
    is_ordered <- config[["factor"]][["ordered"]] %||% FALSE
    bad_values <- base::setdiff(unique(x), all_lvls)
    if (length(bad_values)) {
      warning("Bad values observed: ",
              str_c("'", bad_values, "'", collapse = ", "))
    }
    x <- factor(x, levels = all_lvls)
    if (length(missing_values)) {
      attr(x, "missing_values") <- missing_values
    }
    if (is_ordered) {
      attr(x, "ordered_levels") <- lvls
    }
  }
  x
}

# put LPI on 0-4 scale
numeric_lpi <- function(x) {
  if_else(x %in% attr(x, "missing_values"), NA_real_, as.numeric(x) - 1)
}

postprocess_funs <- list(
  lpi = function(x, r) {
    mutate(x, lpi = (numeric_lpi(lpi_food) +
                       numeric_lpi(lpi_water) +
                       numeric_lpi(lpi_medical) +
                       numeric_lpi(lpi_fuel) +
                       numeric_lpi(lpi_cash)) / 5)
  },
  age_int = function(x, r) {
    age <- x[["age"]]
    age <- if_else(age %in% attr(age, "missing_values"),
                   NA_integer_, as.integer(age))
    x[["age_int"]] <- age
    x
  },
  # Categories of Age as used by AGE_COND variable in r5
  age_cond = function(x, r) {
    mutate(x, age_cond = fct_collapse(age,
      "18-35" = as.character(18:35),
      "36-50" = as.character(36:50),
      "51 and above" = as.character(51:130)
    ))
  },
  # Categories of Education as used by EDUC_COND variable in r5
  educ_cond = function(x, r) {
    x <- mutate(x, educ_cond =
               fct_collapse(education,
                          "No formal education" =
                          c("Informal schooling only", "No formal schooling"),
                          "Primary" = c("Some primary schooling",
                                        "Primary school completed"),
                          "Secondary" =
                            c("Secondary school/high school completed",
                              "Some secondary school/high school"),
                          "Post-secondary" = c(
                            "Some university",
                            "University completed",
                            "Post-graduate",
                            "Post-secondary qualifications, other than university"
                          )))
    attr(x[["educ_cond"]], "missing_values") <- c("Don't know", "Missing", "Refused")
    attr(x[["educ_cond"]], "ordered_levels") <-
      c("No formal education", "Primary", "Secondary", "Post-secondary")
    x
  }
)

OTHERVARS <- c("lpi", "age_int", "age_cond")

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
  for (i in names(postprocess_funs)) {
    cat("Running ", i, "\n")
    fxn <- postprocess_funs[[i]]
    out <- fxn(out, .round)
  }

  out %>%
    select(one_of(names(variables)), one_of(OTHERVARS)) %>%
    mutate(round = UQ(.round))
}

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

afrob <- map_df(AFROBAROMETER_ROUNDS, clean_afrobarometer) %>%
  select(round, respno, country, everything())

write_rds(afrob, OUTFILE)

