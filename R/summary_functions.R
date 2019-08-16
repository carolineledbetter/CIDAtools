#' Summary Functions
#'
#' These are functions that provide count, mean and sd
#' and median and IQR summaries for tibbles.
#' They are wrappers for summarise functions that are designed to used
#' with `purrr::map` and `purrr::map_dfr` to create summaries of many
#' variables in long table form.
#'
#' @param .data A tbl.
#' @param count_var A variable for which you want counts. If more than one
#' variable is needed use `purrr::map`.
#' @param mean_sd_var A variable for which you want a mean and standard
#' deviation. If more than one
#' variable is needed use `purrr::map`.
#' @param median_iqr_var A variable for which you want a median, and 25^th^ and
#' 75^th^
#' percentile. If more than one
#' variable is needed use `purrr::map`.
#'
# get frequencies for categorical variables
count_fxn <- function(.data, count_var){
  count_var <- rlang::enquo(count_var)
  tmp <- dplyr::count(.data,
                      value = !!count_var,
                      .drop = F)
  dplyr::mutate(tmp, value = as.character(value))
}

# get mean and sd
mean_sd_fxn <- function(.data, mean_sd_var){
  mean_sd_var <- enquo(mean_sd_var)
  dplyr::summarise(.data,
                   mean = mean(!!mean_sd_var),
                   sd = sd(!!mean_sd_var))
}

# get median and iqr
median_iqr_fxn <- function(.data, median_var){
  median_var <- enquo(median_var)
  dplyr::summarise(.data,
                   median = median(!!median_var),
                   Q25 = quantile(!!median_var, probs = 0.25),
                   Q75 = quantile(!!median_var, probs = 0.75))
}
