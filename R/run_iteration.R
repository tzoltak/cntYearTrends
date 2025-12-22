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
#'   \item{countryMeans}{generated country-year means (both as returned by
#'                       [generate_data], i.e. not standardized, and
#'                       standardized within the group of observed country-means
#'                       - the latter with suffix *Std* added to their names)
#'                       and their estimates from the models (standardized
#'                       within the group of observed country-means)}
#'   \item{items}{item parameters (generated, not estimated)}
#' }
run_iteration <- function(models, coverageScheme, condition, iter, stanPars, max_redraws) {
  stopifnot(is.list(models), all(c("dcpo", "claassen") %in% names(models)),
            all(sapply(models, inherits, what = "CmdStanModel")),
            is.data.frame(coverageScheme),
            is.data.frame(condition), nrow(condition) == 1L,
            is.numeric(iter), length(iter) == 1L, iter > 0,
            as.integer(iter) == iter,
            is.numeric(max_redraws), length(max_redraws) == 1L, max_redraws >= 0L,
            as.integer(max_redraws) == max_redraws,
            is.list(stanPars))
  condition <- as.list(condition)
  redraw <- 0L
  repeat {
    data <- do.call(generate_data, c(pCGY = list(coverageScheme), condition))

    agg_cls <- aggregate_data(data$responses, variant = "Claassen")
    prop <- agg_cls$RespN / agg_cls$Sample
    degenerate <- any(prop == 0 | prop == 1)

    if (!degenerate) break

    redraw <- redraw + 1L
    if (redraw > max_redraws) {
      warning("Max redraws reached; proceeding with degenerate Claassen aggregates.")
      break
    }
  }
  countryMeans <- data$countryYears
  countryMeans$meanStd <-
    (countryMeans$mean - mean(countryMeans$mean)) / stats::sd(countryMeans$mean)
  countryMeans$varStd <- countryMeans$var / stats::var(countryMeans$mean)

  dcpo <- estimate_dcpo(data$responses, models$dcpo, iter = iter,
                        pars = stanPars)
  if (!is.null(dcpo$countryMeans)) {
    countryMeans <- dplyr::left_join(countryMeans, dcpo$countryMeans,
                                     by = c("country", "year"))
    meanToStd <- mean(countryMeans$mean_dcpo, na.rm = TRUE)
    sDToStd <- stats::sd(countryMeans$mean_dcpo, na.rm = TRUE)
    countryMeans <-
      dplyr::mutate(countryMeans,
                    dplyr::across(c("mean_dcpo", "q05_dcpo", "q95_dcpo"),
                                  ~(. - meanToStd) / sDToStd),
                    sd_dcpo = .data$sd_dcpo / sDToStd)
  }
  claassen <- estimate_claassen(data$responses, models$claassen,
                                variant = "dichotomous", iter = iter,
                                pars = stanPars)
  if (!is.null(claassen$countryMeans)) {
    countryMeans <- dplyr::left_join(countryMeans, claassen$countryMeans,
                                     by = c("country", "year"))
    meanToStd <- mean(countryMeans$mean_claassen, na.rm = TRUE)
    sDToStd <- stats::sd(countryMeans$mean_claassen, na.rm = TRUE)
    countryMeans <-
      dplyr::mutate(countryMeans,
                    dplyr::across(c("mean_claassen",
                                    "q05_claassen", "q95_claassen"),
                                  ~(. - meanToStd) / sDToStd),
                    sd_claassen = .data$sd_claassen / sDToStd)
  }
  claassenMulti <- estimate_claassen(data$responses, models$claassen,
                                     variant = "multinomial", iter = iter,
                                     pars = stanPars)
  if (!is.null(claassenMulti$countryMeans)) {
    countryMeans <- dplyr::left_join(countryMeans, claassenMulti$countryMeans,
                                     by = c("country", "year"))
    meanToStd <- mean(countryMeans$mean_claassenMulti, na.rm = TRUE)
    sDToStd <- stats::sd(countryMeans$mean_claassenMulti, na.rm = TRUE)
    countryMeans <-
      dplyr::mutate(countryMeans,
                    dplyr::across(c("mean_claassenMulti",
                                    "q05_claassenMulti", "q95_claassenMulti"),
                                  ~(. - meanToStd) / sDToStd),
                    sd_claassen = .data$sd_claassen / sDToStd)
  }

  return(list(modelSummaries = rbind(dcpo$modelSummary,
                                     claassen$modelSummary,
                                     claassenMulti$modelSummary),
              countryMeans = countryMeans,
              items = data$items,
              itemDistributions = aggregate_data(data$responses,
                                                 variant = "distributions")))
}
