findnearest <- function(x, y,
                        direction = c('both', 'ascending', 'descending')) {
  x <- x[order(x)]
  y <- y[order(y)]
  i_lower <- getlower(x, y)
  i_upper <- getlower(rev(x), rev(y), upper = T)
  y_lower <- y[i_lower]
  y_upper <- rev(rev(y)[i_upper])
  lower_nearest <- x - y_lower < y_upper - x
  y[lower_nearest] <- y_lower[lower_nearest]
  y[!lower_nearest] <- y_upper[!lower_nearest]
  pairs <- list(x,y)
}

getlower <- function(x, y, upper = FALSE){
  n <- length(y)
  z <- c(y, x)
  j <- i <- order(z, decreasing = upper)
  j[j > n] <- -1
  x_max <- cummax(j)
  return (x_max[i > n])
}




mergeClose <- function(x, y, nearBy,
                       nearBy.x = nearBy, nearBy.y = nearBy, 
                       by = intersect(names(x), names(y)),
                       by.x = by, by.y = by, bothdirections = TRUE,
                       all = FALSE, all.x = all, all.y = all,
                       sort = TRUE,
                       suffixes = c(".x",".y"), no.dups = TRUE,
                       incomparables = NULL, ...){
  if(!is.data.frame(x) | !is.data.frame(y))
    stop('x and y must be data.frames')
  nlx <- as.list(seq_along(x))
  names(nlx) <- names(x)
  nearby_x <- x[, eval(substitute(nearBy.x), nlx, parent.frame()), drop = F]
  nly <- as.list(seq_along(y))
  names(nly) <- names(y)
  nearby_y <- y[, eval(substitute(nearBy.x), nly, parent.frame()), drop = F]
  by <- setdiff(by, c(names(nearby_x), names(nearby_y)))
  


  
  # get dataframe for row variables
  nl <- as.list(seq_along(data))
  names(nl) <- names(data)
  rows <- data[, eval(substitute(rowvars), nl, parent.frame()), drop = F]
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
