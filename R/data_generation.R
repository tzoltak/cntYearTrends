#' @title Data generation
#' @description
#' Prepares a data frame with year-project-countries
#' @inheritParams generate_data
#' @returns a data frame with columns `project`, `country` and `year`
#' @seealso [generate_data]
make_countries <- function(pCGY, nCountriesPerGroup) {
  pCGY$year <- as.integer(factor(pCGY$year))
  pCGY$project <- as.integer(factor(pCGY$project))
  pCGY$countryGroup <- as.integer(factor(pCGY$countryGroup))
  pCGY <- tidyr::expand_grid(pCGY,
                             country = 1L:nCountriesPerGroup)
  pCGY$country <- (pCGY$countryGroup - 1L)*nCountriesPerGroup + pCGY$country
  return(pCGY[, c("project", "country", "year")])
}
#' @title Data generation
#' @description
#' Generates project characteristics
#' @param nProjects an integer - the number of survey projects
#' (compare [check_coverage_scheme])
#' @inheritParams generate_data
#' @param nItemsProbs a named numeric vector -  compare [prepare_conditions]
#' @param respScaleLengthProbs a named numeric vector -
#' compare [prepare_conditions]
#' @returns a data frame with columns `project`, `bias`, `nItems`
#' and `respScaleLength`
#' @seealso [generate_data], [generate_latent]
generate_projects <- function(nProjects, projectBiasesSD,
                              nItemsProbs,
                              respScaleLengthProbs) {
  biases <- stats::rnorm(nProjects, mean = 0, sd = projectBiasesSD)
  nItems <- sample(as.integer(names(nItemsProbs)), nProjects, replace = TRUE,
                   prob = nItemsProbs)
  respScaleLength <- sample(as.integer(names(respScaleLengthProbs)),
                            nProjects, replace = TRUE,
                            prob = respScaleLengthProbs)
  return(data.frame(project = 1L:nProjects,
                    bias = biases,
                    nItems = nItems,
                    respScaleLength = respScaleLength))
}
#' @title Data generation
#' @description
#' Generates country-year parameters (mean and variance)
#' @param nCountries an integer - the number of countries
#' (compare [check_coverage_scheme])
#' @param nYears an integer - the number of years
#' (compare [check_coverage_scheme])
#' @inheritParams generate_data
#' @returns a data frame with columns `country`, `year`, `mean`
#' and `var`
#' @seealso [generate_data], [generate_latent]
generate_country_year_parameters <- function(nCountries, nYears,
                                             arMeanStartLB, arMeanStartUB,
                                             arMeanChangeSD,
                                             arVarStartLB, arVarStartUB,
                                             arVarChangeSD,
                                             arMeanTrendLB, arMeanTrendUB) {
  sdLog <- log(arVarChangeSD^2 + 1)^0.5
  meanLog <- sdLog^2/2
  means <- vars <- matrix(NA_real_, nrow = nCountries, ncol = nYears,
                          dimnames = list(1L:nCountries,
                                          1L:nYears))
  means[, "1"] <- stats::runif(nCountries, min = arMeanStartLB, max = arMeanStartUB)
  vars[, "1"] <- stats::runif(nCountries,
                              min = arVarStartLB, max = arVarStartUB)
  for (i in 2L:nYears) {
    
    means[, i] <- means[, i - 1L] + stats::rnorm(nCountries, mean = 0,
                                                 sd = arMeanChangeSD)
    means[, i] <- ifelse(means[, i] < arMeanTrendLB,
                         arMeanTrendLB + (arMeanTrendLB - means[, i]),
                         means[, i])
    means[, i] <- ifelse(means[, i] > arMeanTrendUB,
                         arMeanTrendUB - (means[, i] - arMeanTrendUB),
                         means[, i])
    vars[, i] <- vars[, i - 1L] * stats::rlnorm(nCountries,
                                                meanlog = meanLog, sdlog = sdLog)
  }
  return(data.frame(country = as.integer(rep(rownames(means), nYears)),
                    year = as.integer(rep(colnames(means), each = nCountries)),
                    mean = as.vector(means),
                    var = as.vector(vars)))
}
#' @title Data generation
#' @description
#' Generates latent trait values with project bias already incorporated
#' @param pCY a data frame with survey project-country-year parameters - see
#' [make_countries], [generate_projects] and [generate_country_year_parameters]
#' @inheritParams generate_data
#' @returns a data frame with columns `project`, `country`, `year`, `respondent`
#' and `latent`
#' @seealso [generate_data], [generate_responses]
generate_latent <- function(pCY, nRespondents) {
  pCY <- tidyr::expand_grid(pCY,
                            respondent = 1L:nRespondents)
  pCY$latent = pCY$mean + pCY$bias + pCY$var^0.5*stats::rnorm(nrow(pCY),
                                                              mean = 0, sd = 1)
  return(pCY[, c("project", "country", "year", "respondent", "latent")])
}
#' @title Data generation
#' @description
#' Generates item parameters
#' @inheritParams generate_data
#' @inheritParams generate_latent
#' @param unstLoadingsCL a numeric - +- these will be set as limits of trunc.-norm. distribs.
#' @param unstLoadingsYL a numeric - +- these will be set as limits of trunc.-norm. distribs.
#' @param difficultyCL a numeric - +- these will be set as limits of trunc.-norm. distribs.
#' @param difficultyYL a numeric - +- these will be set as limits of trunc.-norm. distribs.
#' @returns a data frame with columns `project` `country`, `year`, `item`,
#' `respScaleLength`, `unstLoading` and `thresholds`
#' @seealso [generate_data], [generate_responses]
generate_items <- function(pCY,
                           unstLoadingDefault, difficultyDefault,
                           # standard deviations of trunc.-norm. distribs.
                           unstLoadingsCSD, unstLoadingsYSD,
                           difficultyCSD, difficultyYSD,
                           # +- this will be set as limits of the uniform distrib.
                           relThresholdsL,
                           # +- these will be set as limits of trunc.-norm. distribs.
                           unstLoadingsCL = 2*unstLoadingsCSD,
                           unstLoadingsYL = 2*unstLoadingsYSD,
                           difficultyCL = 2*difficultyCSD,
                           difficultyYL = 2*difficultyYSD) {
  # making rmtruncnorm not to fail in corner cases
  if (unstLoadingsCSD == 0) unstLoadingsCL <- 0.1
  if (unstLoadingsYSD == 0) unstLoadingsYL <- 0.1
  if (difficultyCSD == 0) difficultyCL <- 0.1
  if (difficultyYSD == 0) difficultyYL <- 0.1
  
  projects <- unique(pCY[, c("project", "respScaleLength", "nItems")])
  items <- tidyr::expand_grid(projects,
                              item = 1L:max(projects$nItems))
  items <- items[items$item <= items$nItems,
                 c("project", "item", "respScaleLength")]
  items$unstLoading <- unstLoadingDefault
  items$difficulty <- difficultyDefault
  items$relThresholds <-
    t(sapply(items$respScaleLength,
             function(nCat, relThresholdsL, maxNCat) {
               relThresholds <- sort(stats::runif(nCat - 1L,
                                                  -relThresholdsL,
                                                  relThresholdsL))
               relThresholds <- relThresholds - mean(relThresholds)
               relThresholds <- c(relThresholds,
                                  rep(NA_real_, maxNCat - nCat))
               return(relThresholds)
             },
             relThresholdsL = relThresholdsL,
             maxNCat = max(items$respScaleLength)))
  
  countries <- unique(pCY[, c("country"), drop = FALSE])
  countries$unstLoadingCBias <-
    mnormt::rmtruncnorm(nrow(countries),
                        0, unstLoadingsCSD,
                        -unstLoadingsCL, unstLoadingsCL)
  countries$difficultyCBias <-
    mnormt::rmtruncnorm(nrow(countries),
                        0, difficultyCSD,
                        -difficultyCL, difficultyCL)
  
  itemYears <- tidyr::expand_grid(items,
                                  year = unique(pCY$year))
  itemYears$unstLoadingYBias <-
    mnormt::rmtruncnorm(nrow(itemYears),
                        0, unstLoadingsYSD,
                        -unstLoadingsYL, unstLoadingsYL)
  itemYears$unstLoadingYBias[itemYears$year == 1] = 0
  itemYears$difficultyYBias <-
    mnormt::rmtruncnorm(nrow(itemYears),
                        0, difficultyYSD,
                        -difficultyYL, difficultyYL)
  itemYears$difficultyYBias[itemYears$year == 1] = 0
  
  pCY <- merge(pCY, countries, by = "country")
  pCY <- merge(pCY, itemYears, by = c("project", "year", "respScaleLength"))
  
  pCY$unstLoading <- pCY$unstLoading + pCY$unstLoadingCBias + pCY$unstLoadingYBias
  pCY$difficulty <- pCY$difficulty + pCY$difficultyCBias + pCY$difficultyYBias
  pCY$thresholds <- pCY$difficulty + pCY$relThresholds
  return(pCY[, c("project", "country", "year", "item", "respScaleLength",
                 "unstLoading", "thresholds")])
}
#' @title Data generation
#' @description
#' Generates responses
#' @param latent a data frame returned by [generate_latent]
#' @param items a data frame returned by [generate_items]
#' @returns a data frame with columns `project`, `country`, `year`, `item`,
#' `respScaleLength`, `respondent` and `response`
#' @seealso [generate_data]
generate_responses <- function(latent, items) {
  respItems <- dplyr::left_join(latent, items, 
                                by = c("project", "country", "year"), 
                                relationship =
                                  "many-to-many")
  respItems$latentResponse <- respItems$latent * respItems$unstLoading +
    stats::rnorm(nrow(respItems), mean = 0, sd = 1)
  respItems$response <-
    rowSums((respItems$latentResponse - respItems$thresholds) > 0, na.rm = TRUE)
  return(respItems[, c("project", "country", "year", "item", "respScaleLength",
                       "respondent", "response")])
}
#' @title Data generation
#' @description
#' Generates data by calling more specific data-generating functions
#' @param pCGY a data frame with country-year-survey project coverage scheme
#' (see [check_coverage_scheme])
#' @param nRespondents an integer - the number of respondents within each
#' country(-year-survey project)
#' @param nCountriesPerGroup an integer - the number of countries to be
#' generated for each group of countries (compare [check_coverage_scheme])

#' @param nCountriesPerGroup an integer - the number of countries to be
#' generated for each group of countries (compare [check_coverage_scheme])
#' @param variant a string - see [check_coverage_scheme]
#' @param projectBiasesSD a numeric - see [prepare_conditions]
#' @param nItemsProbs a string - see [prepare_conditions]
#' @param respScaleLengthProbs a string - see [prepare_conditions]
#' @param arMeanStartLB a numeric - see [prepare_conditions]
#' @param arMeanStartUB a numeric - see [prepare_conditions]
#' @param arMeanChangeSD a numeric - see [prepare_conditions]
#' @param arVarStartLB a numeric - see [prepare_conditions]
#' @param arVarStartUB a numeric - see [prepare_conditions]
#' @param arVarChangeSD a numeric - see [prepare_conditions]
#' @param arMeanTrendLB a numeric - see [prepare_conditions]
#' @param arMeanTrendUB a numeric - see [prepare_conditions]
#' @param unstLoadingDefault a numeric - see [prepare_conditions]
#' @param difficultyDefault a numeric - see [prepare_conditions]
#' @param unstLoadingsCSD a numeric - see [prepare_conditions]
#' @param unstLoadingsYSD a numeric - see [prepare_conditions]
#' @param difficultyCSD a numeric - see [prepare_conditions]
#' @param difficultyYSD a numeric - see [prepare_conditions]
#' @param relThresholdsL a numeric - see [prepare_conditions]
#' @returns a list of four data frames:
#' \describe{
#'   \item{countryYears}{,}
#'   \item{projectCountryYears}{,}
#'   \item{items}{,}
#'   \item{responses}{.}
#' }
#' @seealso [make_countries], [generate_projects],
#' [generate_country_year_parameters], [generate_latent], [generate_items]
#' and [generate_responses]
#' @export
generate_data <- function(pCGY,
                          # projects characteristics
                          nRespondents, nCountriesPerGroup, variant,
                          projectBiasesSD, nItemsProbs, respScaleLengthProbs,
                          # country-means and SDs autoregressive process parameters
                          arMeanStartLB, arMeanStartUB, arMeanChangeSD,
                          arMeanTrendLB, arMeanTrendUB,
                          arVarStartLB, arVarStartUB, arVarChangeSD,
                          # item parameters
                          unstLoadingDefault, difficultyDefault,
                          unstLoadingsCSD, unstLoadingsYSD,
                          difficultyCSD, difficultyYSD,
                          relThresholdsL) {
  nItemsProbs <- eval(str2expression(as.character(nItemsProbs)))
  respScaleLengthProbs <- eval(str2expression(as.character(respScaleLengthProbs)))
  
  pCGY <- pCGY[pCGY$variant == variant, ]
  
  projectCountryYears <- make_countries(pCGY = pCGY,
                                        nCountriesPerGroup = nCountriesPerGroup)
  
  nProjects <- length(unique(projectCountryYears$project))
  nCountries <- length(unique(projectCountryYears$country))
  
  projects <- generate_projects(nProjects = nProjects,
                                projectBiasesSD = projectBiasesSD,
                                nItemsProbs = nItemsProbs,
                                respScaleLengthProbs = respScaleLengthProbs)
  countryYears <-
    generate_country_year_parameters(nCountries = nCountries,
                                     nYears = max(pCGY$year) - min(pCGY$year) + 1L,
                                     arMeanStartLB = arMeanStartLB,
                                     arMeanStartUB = arMeanStartUB,
                                     arMeanChangeSD = arMeanChangeSD,
                                     arMeanTrendLB = arMeanTrendLB,
                                     arMeanTrendUB = arMeanTrendUB,
                                     arVarStartLB = arVarStartLB,
                                     arVarStartUB = arVarStartUB,
                                     arVarChangeSD = arVarChangeSD)
  
  projectCountryYears <- merge(projectCountryYears,
                               projects,
                               by = "project")
  projectCountryYears <- merge(projectCountryYears,
                               countryYears,
                               by = c("country", "year"))
  latent <- generate_latent(pCY = projectCountryYears,
                            nRespondents = nRespondents)
  items <- generate_items(pCY = projectCountryYears,
                          unstLoadingDefault = unstLoadingDefault,
                          difficultyDefault = difficultyDefault,
                          relThresholdsL = relThresholdsL,
                          # standard deviations of trunc.-norm. distribs.
                          unstLoadingsCSD = unstLoadingsCSD,
                          unstLoadingsYSD = unstLoadingsYSD,
                          difficultyCSD = difficultyCSD,
                          difficultyYSD = difficultyYSD)
  
  # Generate responses in chunks to save memory
  unique_countries <- unique(latent$country)
  country_chunks <- split(unique_countries, ceiling(seq_along(unique_countries)/5))
  
  responses_list <- vector("list", length(country_chunks))
  
  # Generate responses for each chunk of countries and store them in the list
  for(i in seq_along(country_chunks)) {
    chunk <- country_chunks[[i]]
    latent_chunk <- latent[latent$country %in% chunk, ]
    responses_chunk <- generate_responses(latent = latent_chunk, items = items)
    responses_chunk$Item = paste0("i", responses_chunk$item, "p", responses_chunk$project)
    responses_chunk$Item_Cnt = paste0(responses_chunk$Item, "c", responses_chunk$country)
    responses_list[[i]] <- responses_chunk  # store the chunk data frame in the list
    
    # Free up memory used by temporary variables
    rm(latent_chunk, responses_chunk)
    gc()
  }
  
  
  items$Item = paste0("i", items$item, "p", items$project)
  items$Item_Cnt = paste0(items$Item, "c", items$country)

  return(list(countryYears = countryYears,
              projectCountryYears = projectCountryYears,
              items = items,
              responses = responses_list))
}
