#' @title Model estimation
#' @description
#' Transforms data on responses to the format fitting Claassen's model.
#' @param responses a data frame with responses - typically the `responses`
#' element of a list returned by [generate_data]
#' @param variant a string indicating whether the returned data should fit
#' the original dichotomous Claassen's model or its multinomial extension
#' @details
#' Adaptation of the original Claassen's code that is available at
#' @return a list
#' @export
format_claassen <- function(responses,
                            variant = c("dichotomous", "multinomial")) {
  variant <- match.arg(variant, several.ok = FALSE)
  
  if (is.list(responses)) {
    
    responses <- lapply(responses, function(df) {
      stopifnot(is.data.frame(df),
                all(c("project", "country", "year", "item",
                      "respScaleLength", "respondent",
                      "response", "Item", "Item_Cnt") %in% names(df)))
      
      df <- aggregate_data(df, ifelse(variant == "dichotomous",
                                      "Claassen",
                                      "ClaassenMulti"))
      return(df)
    })
    responses <- do.call(rbind, responses)
  } else {
    stopifnot(is.data.frame(responses),
              all(c("project", "country", "year", "item",
                    "respScaleLength", "respondent",
                    "response", "Item", "Item_Cnt") %in% names(responses)))
    
    responses <- aggregate_data(responses, ifelse(variant == "dichotomous",
                                                  "Claassen",
                                                  "ClaassenMulti"))
  }
  responses <- responses[order(responses$Country, responses$Year), ]
  # equivalent to logit(mean(responses$Response))
  mean.resp.prop <- mean(responses$RespN / responses$Sample)
  mean.resp.log <- log(mean.resp.prop / (1 - mean.resp.prop))
  # create item-country length indicator for items (number of countries for each item)
  item.ind.len <- tapply(responses$Country, responses$Item,
                         function(x) length(unique(x)), simplify = TRUE)
  # length of each estimated mood series for ragged array models
  len_theta_ts <- tapply(responses$Year, responses$Country,
                         function(x) 1 + max(x) - min(x), simplify = TRUE)
  # create r-length cntry and year vectors
  cntrys.r <- rep(as.numeric(names(len_theta_ts)), len_theta_ts)
  year.r <- unlist(tapply(responses$Year, responses$Country,
                          function(x) seq(min(x), max(x)), simplify = TRUE))
  # create indicator vector for estimate start positions
  est_pos <- 1 + c(0, cumsum(len_theta_ts[-length(len_theta_ts)]))
  # create R-vector mapping R support estimates to N opinions
  n.map <- data.frame(Obs = seq_len(nrow(responses)), Cntry = responses$Country,
                      Yr = responses$Year)
  r.map <- data.frame(Est = seq_len(sum(len_theta_ts)), Cntry = cntrys.r,
                      Yr = year.r)
  n.r.merg <- merge(n.map, r.map, by = c("Cntry", "Yr"), all.x = TRUE)
  n.r.merg <- n.r.merg[order(n.r.merg$Obs), ]
  
  return(list(dat.1 = list(N = nrow(responses),
                           K = length(unique(responses$Item)),
                           J = length(unique(responses$Country)),
                           P = length(unique(responses$Item_Cnt)),
                           R = sum(len_theta_ts),
                           jj = responses$Country,
                           pp = as.numeric(factor(responses$Item_Cnt)),
                           kk = as.numeric(factor(responses$Item)),
                           rr = n.r.merg$Est,
                           it_len = item.ind.len,
                           est_pos = est_pos,
                           len_theta_ts = len_theta_ts,
                           x = responses$RespN,
                           samp = responses$Sample,
                           mn_resp_log = mean.resp.log),
              cnt.names = unique(responses$Country),
              r.map = r.map))
}