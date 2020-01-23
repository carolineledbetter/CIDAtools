#' Summary Functions
#'
#' These are functions that provide count, mean and sd
#' and median and IQR summaries for tibbles.
#' They are wrappers for summarise functions that are designed to used
#' with `purrr::map` and `purrr::map_dfr` to create summaries of many
#' variables in long table form.
#'
#' @param data A tbl.
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
#' @importFrom stats median sd quantile
#'
#' @name summ_fxns
NULL
#> NULL

#' @rdname summ_fxns
#' @export
# get frequencies for categorical variables
count_fxn <- function(data, count_var){
  if (!requireNamespace('dplyr', quietly = T)) {
    stop('dplyr is required')
  }
  if (!requireNamespace('rlang', quietly = T)) {
    stop('rlang is required')
  }
  count_var <- rlang::enquo(count_var)
  tmp <- dplyr::count(data,
                      value = !!count_var,
                      .drop = F)
  tmp <- dplyr::arrange(tmp,
                        (value == 'Missing'),
                        .by_group = TRUE)
  tmp <- dplyr::mutate(tmp,
                       value = as.character(value),
                       pct = n/sum(n)*100)
  tidyr::pivot_longer(tmp,
                      cols = !!count_var,
                      names_to = 'variable',
                      values_to = value)
}

#' @rdname summ_fxns
#' @export
# get mean and sd
mean_sd_fxn <- function(data, mean_sd_var){
  if (!requireNamespace('dplyr', quietly = T)) {
    stop('dplyr is required')
  }
  if (!requireNamespace('rlang', quietly = T)) {
    stop('rlang is required')
  }
  mean_sd_var <- rlang::enquo(mean_sd_var)
  dplyr::summarise(data,
                   mean = mean(!!mean_sd_var, na.rm = T),
                   sd = sd(!!mean_sd_var, na.rm = T))
}

#' @rdname summ_fxns
#' @export
# get median and iqr
median_iqr_fxn <- function(data, median_var){
  if (!requireNamespace('dplyr', quietly = T)) {
    stop('dplyr is required')
  }
  if (!requireNamespace('rlang', quietly = T)) {
    stop('rlang is required')
  }
  median_var <- rlang::enquo(median_var)
  dplyr::summarise(data,
                   median = median(!!median_var, na.rm = T),
                   Q25 = quantile(!!median_var, probs = 0.25, na.rm = T),
                   Q75 = quantile(!!median_var, probs = 0.75, na.rm = T))
}


