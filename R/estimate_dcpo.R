#' @title Model estimation
#' @description
#' Estimates Solt's DCPO model.
#' @param responses a data frame with responses - typically the `responses`
#' element of a list returned by [generate_data]
#' @param model an object of class *CmdStanModel* representing Solt's model
#' @param iter a positive integer - number of MCMC iterations
#' @param pars a list of additional parameters that are passed to the `model`'s
#' `sample` method
#' @return a list with two elements:
#' \describe{
#'   \item{modelSummary}{a data frame with model estimation time and *rhat*}
#'   \item{countryMeans}{a data frame with country-year means estimates}
#' }
#' @export
estimate_dcpo <- function(responses, model, iter = 1000L,
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

  modelSummary <- data.frame(model = "dcpo",
                             errors = "",
                             estimationTime = NA_real_,
                             rhat = NA_real_)
  countryMeans <- NULL

  input <- try(format_dcpo(responses))
  if (inherits(input, "try-error")) {
    modelSummary$errors = paste("Format DCPO failed:",
                                attributes(input)$condition$message)
  } else {
    output <- do.call(model$sample, c(data = list(input[1L:13L]), pars))
    modelSummary$estimationTime = output$time()$total
    if (any(output$return_codes() != 0)) {
      modelSummary$errors <-
        paste0("Stan non-zero output code (for at least one chunk): ",
               paste(output$return_codes(), collapse = ", "))
    } else {
      modelSummary$rhat =  max(output$summary(NULL, "rhat")$rhat, na.rm = TRUE)
      countryMeans <- summarize_dcpo_results(input, output)
    }
  }
  return(list(modelSummary = modelSummary,
              countryMeans = countryMeans))
}
