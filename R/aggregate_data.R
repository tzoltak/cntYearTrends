#' @title Data generation
#' @description
#' Aggregates data so it fits the structure needed for the estimating routines
#' @param responses a data frame - `responses` element of the list returned by
#' [generate_data]
#' @param variant a string defining the structure of the returned data
#' @returns a data frame
#' @export
aggregate_data <- function(responses,
                           variant = c("distributions", "distributionsClaassen",
                                       "DCPO", "Claassen", "ClaassenMulti")) {
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
         })))
  
  if (variant %in% c("distributions", "distributionsClaassen")) {
    if (variant == "distributionsClaassen") {
      responses$response <-
        as.integer(responses$response >= (responses$respScaleLength / 2))
    }
    responses$n <- 1L
    responses <- stats::aggregate(responses[, "n", drop = FALSE],
                                  responses[, c("project", "country", "year",
                                                "item", "respScaleLength",
                                                "response")],
                                  sum)
    return(stats::reshape(responses, direction = "wide",
                          idvar = c("project", "country", "year", "item",
                                    "respScaleLength"),
                          timevar = "response", ids = "n"))
  } else if (variant == "DCPO") {
    responses$n <- 1L
    names(responses)[names(responses) == "project"] <- "survey"
    names(responses)[names(responses) == "response"] <- "r"
    responses <- stats::aggregate(responses[, "n", drop = FALSE],
                                  responses[, c("year", "country", "survey",
                                                "Item", "Item_Cnt", "r")],
                           sum)
    countries <- stats::aggregate(data.frame(cc_rank = rep(1L, nrow(responses))),
                                  responses[, "country", drop = FALSE],
                                  sum)
    responses <- merge(responses, countries, by = "country")
    responses <- responses[order(responses$country, responses$year,
                                 responses$survey, responses$Item,
                                 responses$r),
                           c("country", "year", "survey", "Item", "Item_Cnt",
                             "r", "n", "cc_rank")]
    names(responses) <- tolower(names(responses))
    responses$item_cnt <- NULL
    rownames(responses) <- 1L:nrow(responses)
    return(responses)
  } else if (variant == "Claassen") {
    responses$RespN <- responses$response >= (responses$respScaleLength / 2)
    responses$Sample <- 1L
    names(responses)[names(responses) == "year"] <- "Year"
    names(responses)[names(responses) == "country"] <- "Country"
    names(responses)[names(responses) == "project"] <- "Project"
    responses <- stats::aggregate(x = responses[, c("Sample", "RespN")],
                                  responses[, c("Country", "Year", "Project",
                                                "Item", "Item_Cnt")],
                                  sum)
    responses <- responses[order(responses$Country, responses$Year,
                                 responses$Item, responses$Project),
                           c("Country", "Year", "Item", "Sample", "RespN",
                             "Project", "Item_Cnt")]
    rownames(responses) <- 1L:nrow(responses)
    return(responses)
  } else if (variant == "ClaassenMulti") {
    responses$Sample <- 1L
    names(responses)[names(responses) == "year"] <- "Year"
    names(responses)[names(responses) == "country"] <- "Country"
    names(responses)[names(responses) == "project"] <- "Project"
    names(responses)[names(responses) == "response"] <- "RespN"
    responses <- stats::aggregate(x = responses[, c("Sample", "RespN")],
                                  responses[, c("Country", "Year", "Project",
                                                "Item", "Item_Cnt",
                                                "respScaleLength")],
                                  sum)
    responses$Sample <- responses$Sample * responses$respScaleLength
    responses <- responses[order(responses$Country, responses$Year,
                                 responses$Item, responses$Project),
                           c("Country", "Year", "Item", "Sample", "RespN",
                             "Project", "Item_Cnt")]
    rownames(responses) <- 1L:nrow(responses)
    return(responses)
  }
}
