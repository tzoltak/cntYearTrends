#' @title Preparing simulation conditions
#' @description
#' Returns names of parameters required to specify a simulation condition.
#' @param nonNA a logical value - if `TRUE` returns only names of parameters
#' which values cannot be set to missing values
#' @returns a character vector
#' @seealso [check_conditions_names], [check_conditions]
#' @export
get_required_conditions_names <- function(nonNA = FALSE) {
  stopifnot(is.logical(nonNA), length(nonNA) == 1, !anyNA(nonNA))
  reqNames <- c("nRespondents", "projectBiasesSD", "nItemsProbs",
                "respScaleLengthProbs",
                "arMeanStartLB", "arMeanStartUB", "arMeanChangeSD",
                "arMeanTrendLB", "arMeanTrendUB",
                "arVarStartLB", "arVarStartUB", "arVarChangeSD",
                "unstLoadingDefault",
                "difficultyMean", "difficultySD", "difficultyLB", "difficultyUB",
                "thresholdsIncrLB", "thresholdsIncrUB",
                "unstLoadingsCSD", "unstLoadingsYSD",
                "difficultyCSD", "difficultyYSD",
                "variant", "nCountriesPerGroup")
  if (nonNA) reqNames <- setdiff(reqNames, c("difficultyMean", "difficultySD"))
  return(reqNames)
}
#' @title Preparing simulation conditions
#' @description
#' Checks whether a given vector of names contains names of all the parameters
#' required to specify a simulation condition
#' @param parNames a character vector
#' @returns its argument (or throws an error)
#' @seealso [check_conditions], [prepare_conditions]
check_conditions_names <- function(parNames) {
  stopifnot(is.character(parNames),
            !anyNA(parNames))
  reqNames <- get_required_conditions_names()
  if (!all(reqNames %in% parNames)) {
    stop(paste0("Some required parameters has not been specified: '",
                paste(setdiff(reqNames, parNames), collapse = "', '"), "'."))
  }
  if (any(!(parNames %in% reqNames))) {
    message(paste0("Some additional parameters have been specified: '",
                   paste(setdiff(parNames, reqNames), collapse = "', '"), "'. ",
                   "These ones won't affect the simulation design."))
  }
  if (any(duplicated(parNames))) {
    stop(paste0("Each argument must be specified as a column in exactly one data frame (duplicated names: '",
                paste(parNames[duplicated(parNames)], collapse = "', '"), "')."))
  }
  return(parNames)
}
#' @title Preparing simulation conditions
#' @description
#' Checks whether columns in the data frame storing specifications of simulation
#' conditions have a correct set o values
#' @param conditions a data frame
#' @returns its argument (or throws an error)
#' @seealso [check_conditions_names], [prepare_conditions]
#' @export
check_conditions <- function(conditions) {
  stopifnot(is.data.frame(conditions))
  check_conditions_names(names(conditions))

  nItemsProbsProblems <- lapply(unique(conditions$nItemsProbs),
                                function(x) try(eval(str2expression(x))))
  nItemsProbsProblems <-
    unique(conditions$nItemsProbs)[!sapply(nItemsProbsProblems, is.numeric)]
  if (length(nItemsProbsProblems) > 0L) {
    stop(paste0("There are some problems in expressions specyfing distributions of probabilities of number of items across projects:\n  `",
                paste(nItemsProbsProblems, collapse = "`,\n  `"), "`.\n",
                "These expressions either don't evaluate to numeric vectors or don't evaluate at all."))
  }

  respScaleLengthProbsProblems <- lapply(unique(conditions$respScaleLengthProbs),
                                         function(x) try(eval(str2expression(x))))
  respScaleLengthProbsProblems <-
    unique(conditions$respScaleLengthProbs)[!sapply(respScaleLengthProbsProblems,
                                                    is.numeric)]
  if (length(respScaleLengthProbsProblems) > 0L) {
    stop(paste0("There are some problems in expressions specyfing distributions of probabilities of the response scale length across projects:\n  `",
                paste(respScaleLengthProbsProblems, collapse = "`,\n  `"), "`.\n",
                "These expressions either don't evaluate to numeric vectors or don't evaluate at all."))
  }

  stopifnot(!anyNA(conditions[, get_required_conditions_names(nonNA = TRUE)]),
            is.numeric(conditions$nRespondents),
            all(conditions$nRespondents > 0),
            all(is.finite(conditions$nRespondents)),
            as.integer(conditions$nRespondents) == conditions$nRespondents,
            is.numeric(conditions$projectBiasesSD),
            all(conditions$projectBiasesSD >= 0),
            all(is.finite(conditions$projectBiasesSD)),
            is.character(conditions$nItemsProbs),
            "All expresions given by 'nItemsProbs' must evaluate to vectors of numeric values summing up to 1." =
              all(sapply(conditions$nItemsProbs,
                         function(x) all(eval(str2expression(x)) > 0 &
                                           eval(str2expression(x)) < 1))),
            "All expresions given by 'nItemsProbs' must evaluate to vectors of numeric values summing up to 1." =
              all(sapply(conditions$nItemsProbs,
                         function(x) sum(eval(str2expression(x)))) == 1),
            is.character(conditions$respScaleLengthProbs),
            "All expresions given by 'nItemsProbs' must evaluate to vectors of numeric values summing up to 1." =
              all(sapply(conditions$respScaleLengthProbs,
                         function(x) all(eval(str2expression(x)) > 0 &
                                           eval(str2expression(x)) < 1))),
            "All expresions given by 'nItemsProbs' must evaluate to vectors of numeric values summing up to 1." =
              all(sapply(conditions$respScaleLengthProbs,
                         function(x) sum(eval(str2expression(x)))) == 1),
            is.numeric(conditions$arMeanStartLB),
            is.numeric(conditions$arMeanStartUB),
            "`arMeanStartUB` must be greater than `arMeanStartLB`" =
              all(conditions$arMeanStartUB > conditions$arMeanStartLB),
            is.numeric(conditions$arMeanChangeSD),
            all(conditions$arMeanChangeSD >= 0),
            all(is.finite(conditions$arMeanChangeSD)),
            is.numeric(conditions$arMeanTrendLB),
            is.numeric(conditions$arMeanTrendUB),
            "`arMeanTrendUB` must greater than `arMeanTrendLB`" =
              all(conditions$arMeanTrendUB > conditions$arMeanTrendLB),
            is.numeric(conditions$arVarStartLB),
            is.numeric(conditions$arVarStartUB),
            is.numeric(conditions$arVarChangeSD),
            all(conditions$arVarChangeSD >= 0),
            all(is.finite(conditions$arVarChangeSD)),
            is.numeric(conditions$unstLoadingDefault),
            all(is.finite(conditions$unstLoadingDefault)),
            is.numeric(conditions$difficultyMean),
            is.numeric(conditions$difficultySD),
            all((is.na(conditions$difficultyMean) &
                   is.na(conditions$difficultySD)) |
                  (!is.na(conditions$difficultyMean) &
                     !is.na(conditions$difficultySD))),
            all(is.finite(conditions$difficultyMean) |
                  is.na(conditions$difficultyMean)),
            all(is.finite(conditions$difficultySD) |
                  is.na(conditions$difficultySD)),
            all(conditions$difficultySD >= 0 | is.na(conditions$difficultySD)),
            is.numeric(conditions$difficultyLB),
            is.numeric(conditions$difficultyUB),
            "`difficultyUB` must greater than `difficultyLB`" =
              all(conditions$difficultyUB > conditions$difficultyLB),
            is.numeric(conditions$thresholdsIncrLB),
            is.numeric(conditions$thresholdsIncrUB),
            all(conditions$thresholdsIncrLB > 0),
            "`thresholdsIncrUB` must greater than `thresholdsIncrLB`" =
              all(conditions$thresholdsIncrUB > conditions$thresholdsIncrLB),
            is.numeric(conditions$unstLoadingsCSD),
            all(conditions$unstLoadingsCSD >= 0),
            all(is.finite(conditions$unstLoadingsCSD)),
            is.numeric(conditions$unstLoadingsYSD),
            all(conditions$unstLoadingsYSD >= 0),
            all(is.finite(conditions$unstLoadingsYSD)),
            is.numeric(conditions$difficultyCSD),
            all(conditions$difficultyCSD >= 0),
            all(is.finite(conditions$difficultyCSD)),
            is.numeric(conditions$difficultyYSD),
            all(conditions$difficultyYSD >= 0),
            all(is.finite(conditions$difficultyYSD)),
            is.character(conditions$variant),
            is.numeric(conditions$nCountriesPerGroup),
            all(conditions$nCountriesPerGroup > 0),
            all(is.finite(conditions$nCountriesPerGroup)),
            all(as.integer(conditions$nCountriesPerGroup) == conditions$nCountriesPerGroup))
  return(conditions)
}
