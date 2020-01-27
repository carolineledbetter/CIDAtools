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
#' @examples
#' count_fxn(mtcars, cyl)
#'
#' # the functions will preserve existing groupings
#' by_cyl <- dplyr::group_by(mtcars, cyl)
#' count_fxn(by_cyl, am)
#'
#' # the functions can be combined with purrr::map and rlang::quos
#' # to create long tables of summaries
#' purrr::map(rlang::quos(am), count_fxn, data = by_cyl)
#' purrr::map(rlang::quos(vs, am), count_fxn, data = by_cyl)
#' purrr::map_dfr(rlang::quos(vs, am), count_fxn, data = by_cyl)
#' purrr::map_dfr(rlang::quos(disp, hp), mean_sd_fxn, data = by_cyl)


# get frequencies for categorical variables

count_fxn <- function(data, count_var){
  if (!requireNamespace('dplyr', quietly = T)) {
    stop('dplyr is required')
  }
  if (!requireNamespace('rlang', quietly = T)) {
    stop('rlang is required')
  }

  # allow variable postions as well as names
  if(is.numeric(rlang::enexpr(count_var))){
    count_var <- rlang::sym(names(dplyr::select(dplyr::ungroup(data),
                                         !!count_var)))
  }
  count_var <- rlang::enquo(count_var)


  name <- names(dplyr::select(dplyr::ungroup(data), !!count_var))

  tmp <- dplyr::count(data,
                      value = !!count_var,
                      .drop = F)
  tmp <- dplyr::arrange(tmp,
                        (value == 'Missing'),
                        .by_group = TRUE)
  tmp <- dplyr::mutate(tmp,
                       variable = name,
                       value = as.character(value),
                       pct = n/sum(n)*100)

  # want variable, value, n, pct to be in order,
  # grouping variable will automatically be added to the
  # front but don't want message
  suppressMessages(
  dplyr::select(tmp, variable, value, n, pct)
  )
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

  # allow variable postions as well as names
  if(is.numeric(rlang::enexpr(mean_sd_var))){
    mean_sd_var <- rlang::sym(names(dplyr::select(dplyr::ungroup(data),
                                                !!mean_sd_var)))
  }

  mean_sd_var <- rlang::enquo(mean_sd_var)

  name <- names(dplyr::select(dplyr::ungroup(data), !!mean_sd_var))

  dplyr::summarise(data,
                   variable = name,
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

  # allow variable postions as well as names
  if(is.numeric(rlang::enexpr(median_var))){
    median_var <- rlang::sym(names(dplyr::select(dplyr::ungroup(data),
                                                  !!mmedian_var)))
  }

  median_var <- rlang::enquo(median_var)

  name <- names(dplyr::select(dplyr::ungroup(data), !!median_var))

  dplyr::summarise(data,
                   variable = name,
                   median = median(!!median_var, na.rm = T),
                   Q25 = quantile(!!median_var, probs = 0.25, na.rm = T),
                   Q75 = quantile(!!median_var, probs = 0.75, na.rm = T))
}

by_cyl <- dplyr::group_by(mtcars, cyl)
purrr::map(rlang::quos(am), count_fxn, data = by_cyl)
purrr::map(rlang::quos(vs, am), count_fxn, data = by_cyl)
purrr::map_dfr(rlang::quos(vs, am), count_fxn, data = by_cyl)
purrr::map_dfr(rlang::quos(disp, hp), mean_sd_fxn, data = by_cyl)
purrr::map_dfr(c(1,3:7), mean_sd_fxn, data = by_cyl)
