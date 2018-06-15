#' Create a table one
#'
#'
#' This is a function created to provide characteristics of a study group with
#' an option to stratify by some variable (usually an exposure) The output of
#' this function is designed to be used with pander in rmarkdown, but all row
#' name formatting can be removed with the option: emphasis = 'n'.
#'
#' @param data the data frame or design object of the data you which to
#' characterize
#' @param rowvars A vector of row positions or names.
#' @param colvar The position or name of the variable to stratify by, set
#' to NULL for no stratification.(p values cannot be returned if NULL)
#' @param sigfig The number of significant digits to use for mean, sd, median,
#' and IQR.
#' @param rowvar_names An optional vector of row names to be used for
#' variables. Must be the same length and in the same order as rowvars.
#' @param incl_missing Set to T to include missing values (default)
#' @param incl_pvalues Set to T to include p values (p values are only
#' calculated on non missing observations) (default = FALSE)
#' @param emphasis Set to 's' for to indent categories for categorical values,
#' 'b' to bold just variable names, and 'n' for no emphasis.
#' @param MedIQR optional vector of continuous variables to return median and
#' IQR instead of mean and SD.
#' @param ... Arguments passed through methods to table 1.
#' @return  a matrix with N and percentages for categorical variables and mean
#' and sd for continuous ones. If variables are passed via the MedIQR argument,
#' median and IQR is calculated instead for those variable. In addition, only
#' the 2nd factor of binary categorical variables is displayed. All
#' determinations of categorical,
#' binary, or continuous are performed automatically.
#' Character variables are converted to factors. Variables are displayed in
#' the following order: binary, non-binary categorical, and continuous.
#' If no stratification variable is provided, summary statistics on the
#' entire sample are provided. No p-values can be provided in this case.
#' If a design object is passed in lieu of a data frame, weighted numbers
#' using the survey package are provided.
#' (The survey package must be installed in this case.)
#' @keywords table1 tableone characteristic
#' @export
#'




table1 <- function(data, ...){
  UseMethod('table1')
}

#' @describeIn table1 unweighted table 1
#' @export

table1.data.frame <- function(data, rowvars, colvar, sigfig = 4,
                              rowvar_names = NULL, incl_missing = T,
                              incl_pvalues = F,
                              emphasis = c('b', 's', 'n'),
                              MedIQR = NULL){
  emphasis <- match.arg(emphasis)
  nl <- as.list(seq_along(data))
  names(nl) <- names(data)
  rows <- data[, eval(substitute(rowvars), nl, parent.frame()), drop = F]
  median_rows <- which(names(rows) %in% MedIQR)
  class(rows[, median_rows]) <- c('MedIQR', "numeric")
  n_levs <- sapply(lapply(rows, function(x)
    if(is.character(x)) levels(factor(x)) else levels(x)),
    length)
  cls <- sapply(lapply(rows, class), `[[`, 1)
  cls[cls == 'character'] <- 'factor'
  cls[cls == 'MedIQR'] <- 'zzz'
  ord <- order(cls, n_levs)
  rows <- rows[, ord, drop = F]
  if(!is.null(rowvar_names)) names(rows) <- rowvar_names
  if(emphasis == 'b') names(rows) <- paste0('**', names(rows), '**')
  y <- data[, eval(substitute(colvar), nl, parent.frame())]
  Cols <- length(levels(y))
  p_col <- NULL
  if(incl_pvalues) p_col <- ''
  N_pct <- c('', rep('N(%)', Cols), p_col)
  if(sum(cls == 'factor') == 0) N_pct <- NULL
  Mean_sd <- c('', rep('Mean(SD)', Cols), p_col)
  if(sum(cls == 'numeric') == 0) Mean_sd <- NULL
  Median <- c('', rep('Median(IQR)', Cols), p_col)
  if(sum(cls == 'zzz') == 0) Median <- NULL
  tbl <- lapply(rows, returnRow, y = y, p = incl_pvalues)
  cls <- cls[ord]
  cats <- do.call(rbind, tbl[cls == 'factor'])
  means <- do.call(rbind, tbl[cls == 'numeric'])
  medians <- do.call(rbind, tbl[cls == 'zzz'])
  tbl <- rbind(N_pct, cats, Mean_sd, means, Median, medians)
  if(incl_pvalues) p_col <- 'P value'
  Stratified_N <- table(y)
  Stratified_N <- paste0(levels(y), ' \\n N = ', Stratified_N)
  Header <- c('', Stratified_N, p_col)
  tbl <- rbind(Header, tbl)
  rownames(tbl) <- NULL
  return(tbl)
}

