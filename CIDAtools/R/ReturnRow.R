#' These are internal functions for returning the correct rows (based on class)
#' for table 1
#'
#' @param x the row variable
#' @param y the column variable
#'
#' @importFrom stats aggregate quantile

returnRow <- function(x, y){
  UseMethod('returnRow')
}

#' @describeIn returnRow method for factors
#' @export
#'

returnRow.factor <- function(x, y){
  home_env <- eval(getHome(), parent.frame())
  name <- eval(getName())
  N <- table(x, y)
  pct <- round(prop.table(N, 2)*100, 0)
  N_pct <- matrix(paste0(N, '(', pct, ')' ),
                  byrow = F, ncol = ncol(N))
  table <- cbind(dimnames(N)[[1]], N_pct)
  table <- rbind(c(name, rep('', ncol(N))), table)
  return(table)
}

#' @describeIn returnRow method for numerics
#' @export

returnRow.numeric <- function(x, y){
  home_env <- eval(getHome(), parent.frame())
  name <- eval(getName())
  sigfig <- eval(quote(sigfig), parent.frame(home_env - 2))
  y <- as.data.frame(y)
  mean <- aggregate(x, by = y, mean, na.rm = T, simplify = F)
  sd <- aggregate(x, by = y, sd, na.rm = T)
  mean <- format(mean, trim = T, drop0trailing = F, digits = sigfig)
  sd <- format(sd, trim = T, drop0trailing = F, digits = sigfig)
  mean_sd <- paste0(mean$x, "(", sd$x, ")")
  row <- c(name, mean_sd)
  return(row)
}

#' @describeIn returnRow method for characters - converts to factor and then
#' runs returnRow.factor
#' @export

returnRow.character <- function(x, y){
  x <- factor(x)
  returnRow(x, y)
}

#' @describeIn returnRow methods for numerics with request for Median and IQR
#' @export


returnRow.MedIQR <- function(x, y){
  home_env <- eval(getHome(), parent.frame())
  name <- eval(getName())
  sigfig <- eval(quote(sigfig), parent.frame(home_env - 2))
  y <- as.data.frame(y)
  row <- aggregate(x, by = y, quantile,
                   probs = c(0.5, 0.25, 0.75),
                   simplify = F,
                   na.rm = T)$x
  row <- lapply(row, format, trim = T, digits = sigfig,
                drop0trailing = F)
  row <- sapply(row, paste0,
                ... =  c("(", "-", ")"),
                collapse = '')
  row <- c(name, row)
  return(row)
}



