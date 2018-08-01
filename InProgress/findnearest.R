findnearest <- function(x, y,
                        direction = c('both', 'ascending', 'descending')) {
  x <- x[order(x)]
  y <- c(, y, Inf)
  i_lower <- getlower(x, y)
  i_upper <- length(y) + 1 - rev(getlower(rev(-x), rev(-y)))
  y_lower <- y[i_lower]
  y_upper <- y[i_upper]
  lower_nearest <- x - y_lower < y_upper - probe
  i <- ifelse(lower_nearest, i_lower, i_upper) - 1
  i[i < 1 | i > length(target)] <- NA
  pairs <- list(x,y[])
}

getlower <- function(x, y){
  n <- length(y)
  z <- c(y, x)
  j <- i <- order(z)
  j[j > n] <- -1
  x_max <- cummax(j)
  return (x_max[x_max > n])
}



mergeClose <- function(x, y, diff, nearBy,
                       nearBy.x = nearBy, nearBy.y = nearBy, ...){
  if(!is.data.frame(x) | !is.data.frame(y))
    stop('x and y must be data.frames')
  by_index <-
  ids <- levels(as.factor(x[, by.x]))
  dfx <- lapply(ids, function(i) subset(i, eval(substitute(by.x)) == i,
                                        select = c(eval(substitute(by.x)),
                                                   eval(substitute())))


  # if(!identical(class(x[, nearBy.x]), class(y[, nearBy.y])))
  #   stop('nearBy.x and nearBy.y must be the same class')
  # nb <- x[, nearBy.x]
  # UseMethod('mergeClose', nb)
}

mergeClose.POSIXt <- function(x, y, diff, nearBy,
                              nearBy.x = nearBy, nearBy.y = nearBy,
                              by = setdiff(intersect(names(x), names(y)),
                                           nearBy),
                              by.x = by, by.y = by, bothdirections = TRUE,
                              all = FALSE, all.x = all, all.y = all,
                              sort = TRUE,
                              suffixes = c(".x",".y"), no.dups = TRUE,
                              incomparables = NULL, ...){
  print(nearBy)
  print(by)
}





ids <- levels(as.factor(dfx$RecordID))

indices <- list()
indices$x <- order(dfx$Time)
indices$y <- order(dfx$Time)
n <- length(indices$x)
index <- c(indices$x, indices$y)
j <- i <- order(index)
j[j > n] <- -1
y_max <- cummax(j)
match <- y_max[i > n]
