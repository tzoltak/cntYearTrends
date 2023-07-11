#' @title Model estimation
#' @description
#' Transforms data on responses to the format fitting DCPO (Solt's) model.
#' @param responses a data frame with responses - typically the `responses`
#' element of a list returned by [generate_data]
#' @details
#' Adaptation of the original Solt's function of the same name included in the
#' DCPOtools package - compare https://github.com/fsolt/DCPOtools.
#' @return a list
#' @importFrom dplyr .data %>%
#' @export
format_dcpo <- function(responses) {
   scale_q = "i1p5"
   scale_cp = 1
  
   if (is.list(responses)) {
     
     responses <- lapply(responses, function(df) {
       stopifnot(is.data.frame(df),
                 all(c("project", "country", "year", "item",
                       "respScaleLength", "respondent",
                       "response", "Item", "Item_Cnt") %in% names(df)))
       
       df <- aggregate_data(df, variant = "DCPO")
       return(df)
     })
     responses <- do.call(rbind, responses)
   } else {
     stopifnot(is.data.frame(responses),
               all(c("project", "country", "year", "item",
                     "respScaleLength", "respondent",
                     "response", "Item", "Item_Cnt") %in% names(responses)))
     
     responses <- aggregate_data(responses, variant = "DCPO")
   }
   
   
  # generate cumulative number of respondents with answers above each cutpoint
  responses <- responses %>%
    dplyr::mutate(kk = factor(.data$country, levels = unique(.data$country)),
                  tt = .data$year - min(.data$year) + 1,
                  qq = factor(.data$item, levels = unique(.data$item)),
                  rr = .data$r,
                  r = .data$r + 1,
                  question = .data$item,
                  item = paste(.data$question, .data$r, "or higher")) %>%
    dplyr::group_by(.data$country, .data$year, .data$question) %>%
    dplyr::arrange(dplyr::desc(.data$r), .by_group = TRUE) %>%
    dplyr::mutate(y_r = round(cumsum(.data$n)),
                  n_r = round(sum(.data$n))) %>%
    dplyr::arrange(.data$r, .by_group = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$kk, .data$tt) %>%
    dplyr::filter(.data$y_r > 0, .data$rr > 0)

  useDelta <- responses %>%
    dplyr::group_by(.data$qq, .data$kk) %>%
    dplyr::summarize(years = dplyr::n_distinct(.data$year),
                     .groups = "drop") %>%
    dplyr::arrange(.data$kk) %>%
    tidyr::pivot_wider(names_from = "kk", values_from = "years",
                       values_fill = 0) %>%
    dplyr::mutate(countries =
                    rowSums(dplyr::pick(dplyr::everything())[, -1] > 1)) %>%
    dplyr::mutate(dplyr::across(-c("qq", "countries"),
                                ~ifelse(. > 1 &
                                          .data$countries > 2 &
                                          .data$qq != scale_q,
                                        1, 0))) %>%
    dplyr::select(-"qq", -"countries")

  scaleItemMatrix <- responses %>%
    dplyr::group_by(.data$qq, .data$rr) %>%
    dplyr::summarize(n = sum(.data$n_r),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "rr", names_prefix = "x",
                       values_from = "n", values_fill = 0) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^x\\d+$"),
                                ~ifelse(. > 0, 10, 0)),
                  dplyr::across(dplyr::matches(paste0("^x", scale_cp, "$")),
                                ~ifelse(.data$qq == scale_q, . + 1, 0)),
                  dplyr::across(dplyr::matches("^x\\d+$"),
                                ~ifelse(. > 0, . - 10, 0))) %>%
    dplyr::select(-"qq") %>%
    as.matrix()
  stopifnot(sum(scaleItemMatrix) == 1)

  return(list(K = max(as.numeric(responses$kk)),
              `T` = max(responses$tt),
              Q = max(as.numeric(responses$qq)),
              R = max(responses$rr),
              N = nrow(responses),
              kk = as.numeric(responses$kk),
              tt = as.numeric(responses$tt),
              qq = as.numeric(responses$qq),
              rr = responses$rr,
              y_r = responses$y_r,
              n_r = responses$n_r,
              fixed_cutp = scaleItemMatrix,
              use_delta = useDelta,
              data = responses,
              data_args = list(scale_q = scale_q, scale_cp = scale_cp,
                               delta = TRUE)))
}
