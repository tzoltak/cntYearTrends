#' @title Preparing simulation conditions
#' @description
#' Prepares a data frame storing simulation conditions. Its arguments should be
#' data frames giving combinations of simulation parameters.
#' @param constant a one-row data frame containing values of parameters that
#' should be hold constant across all the simulation conditions
#' @param ... data frames with combinations of values of the other parameters
#' @details
#' Each parameter in the list below should be given as a column in exactly one
#' of the data frames provided as function arguments:
#'
#' \strong{Projects' characteristics}
#' \describe{
#'   \item{nRespondents}{ (an integer) - number of respondents in each country}
#'   \item{projectBiasesSD}{ (a number) - std. dev. of the centered-normal distributions from which project biases will be drawn}
#'   \item{nItemsProbs}{ (a string with expression creating a named numeric vector summing up to 1, with names coercing to integers) - probability distribution of a project having a given number of items}
#'   \item{respScaleLengthProbs}{ (a string with expression creating a named numeric vector summing up to 1, with names coercing to integers) - probability distribution of a project having a given length of the response scale}
#' }
#' \strong{Country-means and SDs autoregressive process parameters}
#' \describe{
#'   \item{arMeanStartLB}{ (a number) - lower bound of a uniform distribution from which country means in the first year are drawn}
#'   \item{arMeanStartUB}{ (a number) - lower bound of a uniform distribution from which country means in the first year are drawn}
#'   \item{arMeanChangeSD}{ (a number) - std. dev. of change of country means relatively to the previous year}
#'   \item{arMeanTrendLB}{ (a number) - the lower bound for the mean trend}
#'   \item{arMeanTrendUB}{ (a number) - the upper bound for the mean trend}
#'   \item{arVarStartLB}{ (a number) - the lower bound of a uniform distrib. (of within-countries std. devs. in the first year)}
#'   \item{arVarStartUB}{ (a number) - the upper bound of a uniform distrib. (of within-countries std. devs. in the first year)}
#'   \item{arVarChangeSD}{ (a number) - std. dev. of a multiplicative change of within-country SD relatively to the previous year (this is parameter of a log-normal distrib.)}
#' }
#' \strong{Default item parameters}
#' \describe{
#'   \item{unstLoadingDefault}{ (a number) - unstandardized loading}
#'   \item{difficultyMean}{ (a number or NA) - if non-NA: expected value of the trunc.-normal distribution of the project-items difficulties; if set to `NA_real_` project-items difficulties will be computed given sampled values of their thresholds - see parameters below}
#'   \item{difficultySD}{ (a number or NA) - if non-NA: standard deviation of the trunc.-normal distribution of the project-items difficulties; if set to `NA_real_` project-items difficulties will be computed given sampled values of their thresholds - see parameters below}
#'   \item{difficultyLB}{ (a number) - lower bound of either 1) trunc.-normal distribution of the project-items difficulties or 2) uniform distribution of the project-items first threshold (if `difficultyMean` and `difficultySD` set to `NA_real_`)}
#'   \item{difficultyUB}{ (a number) - upper bound of either 1) trunc.-normal distribution of the project-items difficulties or 2) uniform distribution of the project-items first threshold (if `difficultyMean` and `difficultySD` set to `NA_real_`)}
#'   \item{thresholdsIncrLB}{ (a number) - lower bound of the uniform distribution of the project-items threshold increments}
#'   \item{thresholdsIncrUB}{ (a number) - upper bound of the uniform distribution of the project-items threshold increments}
#' }
#' \strong{Non-invariance parameters}
#' \describe{
#'   \item{unstLoadingsCSD}{ (a number) - between-countries non-invariance effect std. dev.}
#'   \item{unstLoadingsYSD}{ (a number) - temporal non-invariance effect std. dev. (at the year-project-item level)}
#'   \item{difficultyCSD}{ (a number) - between-countries non-invariance effect std. dev.}
#'   \item{difficultyYSD}{ (a number) - temporal non-invariance effect std. dev. (at the year-project-item level)}
#' }
#' \strong{Parameters describing the projects' coverage}
#' \describe{
#'   \item{variant}{ (a string) - the name of the coverage variant}
#'   \item{nCountriesPerGroup}{ (an integer) - the number of countries that will be created for each row of a given variant}
#' }
#' @returns a data frame
#' @seealso [check_conditions], [check_conditions_names]
#' @examples
#' constant <- data.frame(
#'   # projects' characteristics
#'   nRespondents = 1500L,
#'   projectBiasesSD = 0.5, # project biases SD
#'   nItemsProbs = "c(`1` = 0.3, `2` = 0.4, `3` = 0.3)", # number of items in a project
#'   respScaleLengthProbs = "c( `2` = 0.1, `4` = 0.2, `5` = 0.3, `7` = 0.3, `10` = 0.1)",
#'   # country-means and SDs autoregressive process parameters #####################
#'   arMeanStartLB = -1.0, # the lower bound for the initial means of each country
#'   arMeanStartUB = 0, # the upper bound for the initial means of each country
#'   arMeanChangeSD = 0.3, # SD of change of country means relatively to the previous year
#'   arMeanTrendLB = -1.5, # the lower bound for the mean trend.
#'   arMeanTrendUB = 0.5, # the upper bound for the mean trend.
#'   arVarStartLB = 0.6, # the lower bound of a uniform distrib.
#'                       # (of within-countries SDs in the first year)
#'   arVarStartUB = 1.4, # the upper bound of a uniform distrib.
#'                       # (of within-countries SDs in the first year)
#'   arVarChangeSD = 0, # SD of a multiplicative change of within-country SD relatively to
#'                      # the previous year (this is parameter of a log-normal distrib.)
#'   # default item parameters
#'   unstLoadingDefault = 0.75,
#'   difficultyDefault = 0, # mean of thresholds
#'   difficultyMean = NA_real_, # use uniform distrib. to draw project-items difficulties
#'   difficultySD = NA_real_, # use uniform distrib. to draw project-items difficulties
#'   difficultyLB = -1, # lower bound of the uniform distrib. of the project-items first threshold
#'   difficultyUB = 0, # upper bound of the uniform distrib. of the project-items first threshold
#'   thresholdsIncrLB = 0.3, # lower bound of the uniform distrib.
#'                           # of the project-items threshold increments
#'   thresholdsIncrUB = 1 # upper bound of the uniform distrib.
#'                        # of the project-items threshold increments
#' )
#' (invariance = rbind(
#'   "full metric + small thresholds var." = data.frame(
#'     unstLoadingsCSD = 0, # between-countries non-invariance effect SD
#'     unstLoadingsYSD = 0, # temporal non-invariance effect SD
#'     difficultyCSD = 0.05, # between-countries non-invariance effect SD
#'     difficultyYSD = 0.2), # temporal non-invariance effect SD
#'   "full metric + large thresholds var." = data.frame(
#'     unstLoadingsCSD = 0,
#'     unstLoadingsYSD = 0,
#'     difficultyCSD = 0.2,
#'     difficultyYSD = 0.2),
#'   "small loadings var. + small thresholds var." = data.frame(
#'     unstLoadingsCSD = 0.05,
#'     unstLoadingsYSD = 0,
#'     difficultyCSD = 0.05,
#'     difficultyYSD = 0.2),
#'   "small loadings var. + large thresholds var." = data.frame(
#'     unstLoadingsCSD = 0.15,
#'     unstLoadingsYSD = 0,
#'     difficultyCSD = 0.2,
#'     difficultyYSD = 0.2)
#' ))
#' (scheme <- rbind(
#'   data.frame(variant = "dense",
#'              nCountriesPerGroup = c(2L, 4L, 10L)), # there are 10 groups
#'   data.frame(variant = "sparse",
#'              nCountriesPerGroup = c(2L, 4L, 10L)) # there are 10 groups
#' ))
#' prepare_conditions(constant, invariance, scheme)
#' @export
prepare_conditions <- function(constant, ...) {
  stopifnot(is.data.frame(constant) | is.null(constant))
  if (!is.null(constant)) {
    stopifnot(nrow(constant) == 1L)
  }
  varying <- list(...)
  stopifnot("All arguments must be data frames" =
              all(sapply(varying, is.data.frame)))
  check_conditions_names(c(names(constant), unlist(lapply(varying, names))))

  conditions <- do.call(tidyr::expand_grid,
                        append(list(constant), varying))
  check_conditions(conditions)
  return(conditions)
}
