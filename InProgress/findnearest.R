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




mergeClose <- function(x, y, diff, nearBy,
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
  nearby_x <- data[, eval(substitute(nearBy.x), nlx, parent.frame()), drop = F]
  nearby_y <- data[, eval(substitute(nearBy.x), nly, parent.frame()), drop = F]
  
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
