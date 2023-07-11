#' @title Model estimation
#' @description
#' Estimates Solt's model.
#' @param responses a data frame with responses - typically the `responses`
#' element of a list returned by [generate_data]
#' @param model an object of class *CmdStanModel* representing Solt's model
#' @param variant a string indicating whether to estimate the original
#' dichotomous Claassen's model or its multinomial extension
#' @param iter a positive integer - number of MCMC iterations
#' @param pars a list of additional parameters that are passed to the `model`'s
#' `sample` method
#' @return a list with two elements:
#' \describe{
#'   \item{modelSummary}{a data frame with model estimation time and *rhat*}
#'   \item{countryMeans}{a data frame with country-year means estimates}
#' }
#' @export
estimate_claassen <- function(responses, model,
                              variant = c("dichotomous", "multinomial"),
                              iter = 1000L,
                              pars = list(max_treedepth = 14,
                                          adapt_delta = 0.99,
                                          step_size = 0.005,
                                          seed = 3724,
                                          chains = 4,
                                          parallel_chains = 4,
                                          iter_warmup = iter / 2,
                                          iter_sampling = iter / 2,
                                          refresh = iter / 10,
                                          init = 0.1)) {
  variant <- match.arg(variant, several.ok = FALSE)
  stopifnot(
    (is.data.frame(responses) && 
       all(c("project", "country", "year", "item",
             "respScaleLength", "respondent",
             "response", "Item", "Item_Cnt") %in% names(responses))) || 
      (is.list(responses) && 
         all(sapply(responses, function(df) {
           is.data.frame(df) &&
             all(c("project", "country", "year", "item",
                   "respScaleLength", "respondent",
                   "response", "Item", "Item_Cnt") %in% names(df))
         }))),
    inherits(model, "CmdStanModel"),
    is.numeric(iter), length(iter) == 1L, iter > 0,
    as.integer(iter) == iter,
    is.list(pars)
  )
  defaultPars <- list(max_treedepth = 14,
                      adapt_delta = 0.99,
                      step_size = 0.005,
                      seed = 3724,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = iter / 2,
                      iter_sampling = iter / 2,
                      refresh = iter / 10,
                      init = 0.1)
  for (p in seq_along(pars)) defaultPars[[names(pars)[p]]] <- pars[[p]]
  pars <- defaultPars

  input <- format_claassen(responses, variant)
  output <- do.call(model$sample, c(data = list(input$dat.1), pars))
  modelSummary <- data.frame(model = ifelse(variant == "dichotomous",
                                            "Claassen",
                                            "ClaassenMulti"),
                             errors = "",
                             estimationTime = output$time()$total,
                             rhat = NA_real_)
  if (any(output$return_codes() != 0)) {
    modelSummary$errors <-
      paste0("Stan non-zero output code (for at least one chunk): ",
             paste(output$return_codes(), collapse = ", "))
    countryMeans <- NULL
  } else {
    modelSummary$rhat =  max(output$summary(NULL, "rhat")$rhat, na.rm = TRUE)
    countryMeans <- summarize_claassen_results(input, output)
    if (variant == "multinomial") {
      names(countryMeans) <- sub("claassen", "claassenMulti", names(countryMeans))
    }
  }
  return(list(modelSummary = modelSummary,
              countryMeans = countryMeans))
}
