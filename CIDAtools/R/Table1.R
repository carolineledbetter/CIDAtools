table1 <- function(data, ...){
  UseMethod('table1')
}

table1.data.frame <- function(data, rowvars, colvar, sigfig = 4,
                              rowvar_names = NULL){
  nl <- as.list(seq_along(data))
  names(nl) <- names(data)
  rows <- data[, eval(substitute(rowvars), nl, parent.frame())]
  ord <- order(sapply(lapply(rows, class), `[[`, 1))
  rows <- rows[, ord]
  if(!is.null(rowvar_names)) names(rows) <- rowvar_names
  y <- data[, eval(substitute(colvar), nl, parent.frame())]
  z <- lapply(rows, returnRow, y = y)
  z
}
