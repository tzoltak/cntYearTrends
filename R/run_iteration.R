#' @title Simulation flow
#' @description
#' Performs a single iteration of the simulation.
#' @param models a list of *CmdStanModel* objects - typically returned by
#' [prepare_stan_models]
#' @inheritParams run_simulation
#' @param condition a one-row data frame with additional parameters specifying
#' data generation process that will be passed to [generate_data]
#' @details
#' At the moment function estimates three models:
#' \describe{
#'   \item{Solt's DCPO model}{some explanations and perhaps citation}
#'   \item{Claassen's model}{some explanations and perhaps citation}
#'   \item{Multinomial extension of the Claassen's model}{
#'         some explanations and perhaps citation}
#' }
#' @returns a list of three data frames:
#' \describe{
#'   \item{modelSummaries}{basic model summary statistics in a *long* format}
#'   \item{countryMeans}{generated country-year means (as returned by
#'                       [generate_data], i.e. not standardized) and their
#'                       estimates from the models (standardized within the
#'                       group of observed country-means)}
#'   \item{items}{item parameters (generated, not estimated)}
#' }
run_iteration <- function(models, coverageScheme, condition, iter, stanPars) {
  stopifnot(is.list(models), all(c("dcpo", "claassen") %in% names(models)),
            all(sapply(models, inherits, what = "CmdStanModel")),
            is.data.frame(coverageScheme),
            is.data.frame(condition), nrow(condition) == 1L,
            is.numeric(iter), length(iter) == 1L, iter > 0,
            as.integer(iter) == iter, is.list(stanPars))
  
  
  if (exists("responses")) {
    rm(responses)
  }
  
  condition <- as.list(condition)
  data <- do.call(generate_data, c(pCGY = list(coverageScheme), condition))
  countryMeans <- data$countryYears

  dcpo <- estimate_dcpo(data$responses, models$dcpo, iter = iter,
                        pars = stanPars)
  if (!is.null(dcpo$countryMeans)) {
    countryMeans <- merge(countryMeans, dcpo$countryMeans,
                          by = c("country", "year"), all.x = TRUE)
  }
  claassen <- estimate_claassen(data$responses, models$claassen,
                                variant = "dichotomous", iter = iter,
                                pars = stanPars)
  if (!is.null(claassen$countryMeans)) {
    countryMeans <- merge(countryMeans, claassen$countryMeans,
                          by = c("country", "year"), all.x = TRUE)
  }
  claassenMulti <- estimate_claassen(data$responses, models$claassen,
                                     variant = "multinomial", iter = iter,
                                     pars = stanPars)
  if (!is.null(claassenMulti$countryMeans)) {
    countryMeans <- merge(countryMeans, claassenMulti$countryMeans,
                          by = c("country", "year"), all.x = TRUE)
  }

  return(list(modelSummaries = rbind(dcpo$modelSummary,
                                     claassen$modelSummary,
                                     claassenMulti$modelSummary),
              countryMeans = countryMeans,
              items = data$items,
              itemDistributions = aggregate_data(data$responses,
                                                 variant = "distributions")))
}
