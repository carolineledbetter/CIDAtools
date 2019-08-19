#' Create a descriptive table (table one)
#'
#'
#' This is a function created to provide characteristics of a study group with
#' an option to stratify by some variable (usually an exposure) The output of
#' this function is designed to be used with pander or kable in rmarkdown,
#' but all row
#' name formatting can be removed with the option: emphasis = 'n'.
#'
#' @param data the data frame or design object of the data you which to
#' characterize
#' @param rowvars A vector of row positions or names.
#' @param colvar The position or name of the variable to stratify by, set
#' to \code{NULL} for no stratification.(p values cannot be returned if
#' \code{NULL})
#' @param sigfig The number of significant digits to use for mean, sd, median,
#' and IQR.
#' @param rowvar_names An optional vector of row names to be used for
#' variables. Must be the same length and in the same order as rowvars.
#' @param incl_missing Set to \code{TRUE} to include missing values (default)
#' @param incl_pvalues Set to \code{TRUE} to include p values (p values are only
#' calculated on non missing observations) (default = \code{FALSE})
#' @param emphasis Set to \code{'s'} for to indent categories for categorical values,
#' \code{'b'} to bold just variable names, and \code{'n'} for no emphasis.
#' @param MedIQR optional vector of continuous variables to return median and
#' IQR instead of mean and SD.
#' @param asTable should a table (\code{TRUE}) or a matrix be returned
#' @param lineBreaks should the N be printed on a seperate line from the
#' categories. default is \code{TRUE} (see details)
#' @param tight if \code{TRUE} (default) no spaces between numbers and
#' parenthesis
#' @param verbose should both levels of binary variables be printed.
#' @param ... Arguments passed through methods to table 1.
#' @return  a table/matrix with N and percentages for categorical variables, mean
#' and sd for continuous ones, and median and 25th and 75th percentile for integers.
#' If variables are passed via the MedIQR argument,
#' median and 25th and 75th percentile is calculated instead for those variable.
#' @details  Only
#' the 1st factor of binary categorical variables is displayed. All
#' determinations of categorical,
#' binary, or continuous are performed automatically.
#' Character variables are converted to factors. Variables are displayed in
#' the following order: binary, non-binary categorical, continuous, and integers
#' + continuous variables with median and IQR.
#' If no stratification variable is provided, summary statistics on the
#' entire sample are provided. No p-values can be provided in this case.
#' If a design object is passed in lieu of a data frame, weighted numbers
#' using the survey package are provided.
#' (The survey package must be installed in this case.)
#'
#' To use the lineBreaks use \code{panderOptions('keep.line.breaks', TRUE)}
#' @keywords table1 tableone characteristic descriptive
#' @export
#'


desc_table <- function(data, ....){
  UseMethod('desc_table')
}

#' @describeIn Table1 unweighted table 1
#' @export

desc_table.tbl_df <- function(data,
                              rowvars,
                              colvar = NULL,
                              freq = NULL,
                              mean_sd = NULL,
                              median_iqr = NULL,
                              rowvar_names = NULL,
                              incl_missing = TRUE,
                              incl_pvalues = FALSE,
                              unlist = TRUE,
                              sort = TRUE,
                              tight = TRUE,
                              verbose = FALSE,
                              allow_large_categories = FALSE
                              ) {
  ### check for necessary tidyverse packages
  ### otherwise run as data frame
  for (pkg in c('dplyr', 'rlang', 'purrr')) {
    if (!requireNamespace(pkg, quietly = T)) {
    data <- as.data.frame(data)
    return(desc_table.data.frame(data))
    }
  }; rm('pkg')


  # column classes
  rowvars <- rlang::enquo(rowvars)
  col_class <- purrr::map_chr(dplyr::select(data,
                                            !!rowvars), class)

  na_to_missing <- function(x){
    if(is.factor(x)) levels(x) <- c(levels(x), 'Missing')
    x <- replace(x, is.na(x), 'Missing')
    if(is.factor(x)) x <- droplevels(x)
    return(x)
  }

  # frequency variables
  if (is.null(rlang::enexpr(freq))) {
    freq <- rlang::syms(names(col_class)[col_class %in%
                                             c("factor",
                                               "character",
                                               "logical")])
  } else {
    freq <- rlang::enquo(freq)
    freq <- rlang::syms(names(dplyr::select(data, !!freq)))
  }
  names(freq) <- lapply(freq, rlang::as_string)

  if (incl_missing) {
    data <- dplyr::mutate_at(data,
                             names(freq), na_to_missing)
  }
  # mean variables
  if (is.null(rlang::enexpr(mean_sd))) {
    mean_sd <- rlang::syms(names(col_class)[col_class %in%
                                           c("integer",
                                             "double",
                                             "numeric")])
  } else {
    mean_sd <- rlang::enquo(mean_sd)
    mean_sd <- rlang::syms(names(dplyr::select(data, !!mean_sd)))
  }
  names(mean_sd) <- lapply(mean_sd, rlang::as_string)

  # median IQR variables
  if (is.null(rlang::enexpr(median_iqr))) {
    median_iqr <- NULL
  } else {
    median_iqr <- rlang::enquo(median_iqr)
    median_iqr <- rlang::syms(names(dplyr::select(data, !!median_iqr)))
    names(median_iqr) <- lapply(median_iqr, rlang::as_string)
  }

  # group_by colvar
  if (!is.null(rlang::enexpr(colvar))) {
    if (is.character(rlang::enexpr(colvar))) colvar <- rlang::sym(colvar)
    colvar <- rlang::enquo(colvar)
    data <- dplyr::group_by(data, !!colvar)
  }

  suppressMessages(
    # select in this case will add missing grouping variables,
    # this is desired behaviour
  if(!allow_large_categories){
    n_categories <- purrr::map(dplyr::select(data, !!rowvars,),
                               dplyr::n_distinct)
    if(any(purrr::map(dplyr::select(data, !!rowvars),
                      dplyr::n_distinct) > 10)) {
      has_have = ifelse(sum(n_categories > 10), 'have', 'has')
      stop(paste(paste(names(n_categories)[n_categories > 10],
                 collapse = ', '),
                 has_have,
                 'more than 10 distinct values. To allow run with',
                 '`allow_large_categories` = TRUE.'))
    }
  }
  )

  # get counts
  suppressWarnings(
    # don't want warning for NAs
    # because variable has been changed to value,
    # it's not useful anyway
  count_rows <- purrr::map_dfr(data = data,
                               .x = freq,
                               .f = count_fxn,
                               .id = 'variable')
  )
  count_rows <- dplyr::mutate(count_rows,
                              output = purrr::pmap(list(n = n, pct = pct),
                                                   list))
  count_rows <- dplyr::select(count_rows,
                              -n, -pct)
  # get means
  mean_rows <- purrr::map_dfr(data = data,
                 .x = mean_sd,
                 .f = mean_sd_fxn,
                 .id = 'variable')
  mean_rows <- dplyr::mutate(mean_rows,
                             output = purrr::pmap(list(mean = mean, sd = sd),
                                                  list))
  mean_rows <- dplyr::select(mean_rows,
                             -mean, -sd)

  #get medians
  median_rows <- purrr::map_dfr(data = data,
                 .x = median_iqr,
                 .f = median_iqr_fxn,
                 .id = 'variable')
  median_rows <- dplyr::mutate(median_rows,
                               output = purrr::pmap(list(median = median,
                                                         Q25 = Q25,
                                                         Q75 = Q75),
                                                    list))
  median_rows <- dplyr::select(median_rows,
                               -median, -Q25, -Q75)
  # count_rows <- mutate()

  tbl <- dplyr::bind_rows(freq = count_rows,
                   mean_sd = mean_rows,
                   median_iqr = median_rows, .id = 'type')
  return(tbl)

}







