#' @title Simulation flow
#' @description
#' Runs the simulation.
#' @param conditions a data frame with simulation conditions, typically
#' constructed using [prepare_conditions]
#' @param coverageScheme a data frame with country-year-survey project coverage
#' scheme (see [check_coverage_scheme])
#' @param nIterPerCond a positive integer - number of iterations to be run for
#' each condition
#' @param suffix optionally a string - suffix that will be added to the name
#' of a file storing simulation results (that will be saved to the disk)
#' @param iter optionally a positive integer - number of MCMC iterations in
#' model estimation
#' @param max_redraws Optional non-negative integer. Maximum number of retries
#' to re-generate data if any Claassen aggregate (Country–Year–Item–Project
#' proportion of affirmative answers) equals 0 or 1. If 0, no retries are made
#' @param stanPars optionally a list with additional arguments that will be
#' passed to the Stan models' `sample` method - see [estimate_dcpo] and
#' [estimate_claassen]
#' @details
#' See [run_iteration]
#' @returns (invisibly) a list of four data frames:
#' \describe{
#'   \item{modelSummaries}{basic model summary statistics in a *long* format}
#'   \item{countryMeans}{generated country-year means (as returned by
#'                       [generate_data], i.e. not standardized) and their
#'                       estimates from the models (standardized within the
#'                       group of observed country-means)}
#'   \item{items}{item parameters (generated, not estimated)}
#'   \item{itemDistributions}{distributions (counts) of responses to items for
#'                            each project-country-year-item}
#' }
#' Moreover, after completing each simulation condition-iteration function will
#' save to the disk file "cntYearTrends_results\[suffix\].RData" storing the
#' data frames listed above.
#' @examples
#' \dontrun{
#' str(conditions)
#' str(coverageScheme)
#' set.seed(12345)
#' run_simulation(conditions[1:2, ], coverageScheme, nIterPerCond = 1L, iter = 100L, max_redraws = 10L)
#' }
#' @export
run_simulation <- function(conditions, coverageScheme, nIterPerCond,
                           suffix = "", iter = 1000L, max_redraws = 5L ,stanPars = list()) {
  check_conditions(conditions)
  check_coverage_scheme(coverageScheme, conditions)
  stopifnot(is.numeric(nIterPerCond), length(nIterPerCond) == 1,
            as.integer(nIterPerCond) == nIterPerCond, nIterPerCond > 0,
            is.character(suffix), length(suffix) == 1L, !anyNA(suffix),
            is.numeric(iter), length(iter) == 1L, iter > 0,
            as.integer(iter) == iter,
            is.numeric(max_redraws), length(max_redraws) == 1L, max_redraws >= 0L,
            as.integer(max_redraws) == max_redraws,
            is.list(stanPars))
  models <- prepare_stan_models()
  modelSummaries <- countryMeans <- items <- itemDistributions <- data.frame()
  for (i in seq_len(nIterPerCond)) {
    for (j in seq_len(nrow(conditions))) {
      cat("\n#########################################\n Simulation iteration ",
          i, " (out of ", nIterPerCond,"),\n condition number ", j, " (out of ",
          nrow(conditions),")\n#########################################\n\n",
          sep = "")
      resultsIter <- run_iteration(models, coverageScheme, conditions[j, ],
                                   iter, stanPars, max_redraws)
      modelSummaries <- dplyr::bind_rows(modelSummaries,
                                         cbind(i = i,
                                               cond = j,
                                               resultsIter$modelSummaries))
      countryMeans <- dplyr::bind_rows(countryMeans,
                                       cbind(i = i,
                                             cond = j,
                                             resultsIter$countryMeans))
      items <- dplyr::bind_rows(
        items,
        cbind(i = i,
              cond = j,
              resultsIter$items[, names(resultsIter$items) != "thresholds"],
              stats::setNames(as.data.frame(resultsIter$items$thresholds),
                              paste0("threshold",
                                     seq_len(ncol(resultsIter$items$thresholds))))))
      itemDistributions <- dplyr::bind_rows(itemDistributions,
                                            cbind(i = i,
                                                  j = j,
                                                  resultsIter$itemDistributions))
      save(conditions, modelSummaries, countryMeans, items, itemDistributions,
           file = paste0("cntYearTrends_results", suffix, ".RData"))
    }
  }
  invisible(list(conditions = conditions,
                 modelSummaries = modelSummaries,
                 countryMeans = countryMeans,
                 items = items,
                 itemDistributions = itemDistributions))
}
