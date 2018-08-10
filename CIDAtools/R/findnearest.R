#' Find the nearest observation to another observation
#'
#'
#' This function finds the nearest y to every x. Y's may be duplicated.
#'
#' @param x a vector to find matches for
#' @param y a vector to find the matches
#' @param direction default = both, ascending for only y matches before x and
#' descending for only matches after x.
#' @return  a list of length 2 with a y matched to every x, note if direction =
#' 'ascending' or 'descending', NAs will be returned for x values with no y
#' values before or after, respectively.
#' @keywords findnearest
#' @references This function borrowed heavily from this stack exchange post:
#' https://stats.stackexchange.com/questions/161379/quickly-finding-nearest-time-observation
#' @export
#'



findnearest <- function(x, y,
                        direction = c('both', 'ascending', 'descending')) {
  direction <- match.arg(direction)
  a <- switch(direction, both = T, ascending = T, descending = F)
  d <- switch(direction, both = T, ascending = F, descending = T)
  x <- x[order(x)]
  y <- y[order(y)]
  if(a) {
    i_lower <- getlower(x, y)
    y_lower <- y[i_lower]
  }
  if(direction == 'ascending') {
    y_lower[y_lower > x] <- NA
    return(list(x, y_lower))
  }
  if(d) {
    i_upper <- getlower(rev(x), rev(y), upper = T)
    y_upper <- rev(rev(y)[i_upper])
  }
  if(direction == 'descending') {
    y_upper[y_upper < x] <- NA
    return(list(x, y_upper))
  }
  lower_nearest <- x - y_lower < y_upper - x
  y <- vector(length = length(y_lower))
  class(y) <- class(y_lower)
  y[lower_nearest] <- y_lower[lower_nearest]
  y[!lower_nearest] <- y_upper[!lower_nearest]
  return(list(x,y))
}

#' Internal function for findnearest
#'
#'
#' @param upper upper value?
#' @return  indexes of y for each x
#' @describeIn findnearest function for finding lower(upper) value

getlower <- function(x, y, upper = FALSE){
  n <- length(y)
  z <- c(y, x)
  j <- i <- order(z, decreasing = upper)
  j[j > n] <- -1
  x_max <- cummax(j)
  x_max[x_max == -1] <- 1
  return (x_max[i > n])
}



