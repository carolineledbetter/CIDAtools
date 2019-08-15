#' These are internal functions for returning the correct rows (based on class)
#' for table 1
#'
#' @param x the row variable
#' @param y the column variable
#' @param p incl_pvalue from Table1
#' @param ... arguments passed to S3 methods
#'
#' @importFrom stats aggregate quantile anova chisq.test fisher.test lm

returnRow <- function(x, ...){
  UseMethod('returnRow')
}

#' @describeIn returnRow method for factors
#' @export
#'

returnRow.factor <- function(x, y, p){
  # get location of home calling environment
  home_env <- getHome()
  # get name of variable
  name <- eval(getName())
  # get emphasis from home calling environment
  emphasis <- eval(substitute(emphasis), parent.frame(home_env))
  # include missing? from home calling environment
  incl_missing <- eval(substitute(incl_missing), parent.frame(home_env))
  if(incl_missing) x <- addNA(x, ifany = T)
  # verbose from home calling environment
  verbose <- eval(substitute(verbose), parent.frame(home_env))

  # get N and pct and paste together
  N <- table(x, y)
  pct <- round(prop.table(N, 2)*100, 0)
  N_pct <- matrix(paste0(format(N, big.mark = ',', trim = T)
                         , '(', pct, ')' ),
                  byrow = F, ncol = ncol(N))
  level_names <- dimnames(N)[[1]]
  level_names[is.na(level_names)] <- 'Missing'
  if(emphasis == 's') level_names <- paste0("\\  ", level_names)
  table <- cbind(level_names, N_pct)
  table <- rbind(c(name, rep('', ncol(N))), table)
  colnames(table) <- NULL
  n_row <- nrow(N)
  if(n_row == 2 & !verbose){
    table <- table[-3, ]
    n_row <- n_row - 1
  }
  if(!p) return(table)
  # add p values if requested
  if(any(table(x, y, exclude = c(NA, NaN)) < 5)){
    # use fishers
    p_val <- try(fisher.test(x, y), silent = T)
    if(length(p_val) == 1) p_val <- NA else p_val <- p_val$p.value
  } else{
    p_val <- chisq.test(x, y)$p.value
  }
  if(!is.na(p_val) & p_val < 0.01){
    p_val <- '<0.01'
    } else {
      p_val <- format(p_val, trim = T, nsmall = 2, digits = 1)
    }
  table <- cbind(table, c(p_val, rep('', n_row)))
  return(table)
}

#' @describeIn returnRow method for numerics
#' @export

returnRow.numeric <- function(x, y, p){
  home_env <- getHome()
  name <- eval(getName())
  sigfig <- eval(quote(sigfig), parent.frame(home_env))
  nsmall <- eval(quote(nsmall), parent.frame(home_env))
  y <- as.data.frame(y)
  mean <- aggregate(x, by = y, mean, na.rm = T, simplify = F)
  sd <- aggregate(x, by = y, sd, na.rm = T)
  mean <- format(mean, trim = T, drop0trailing = F, digits = sigfig,
                 nsmall = nsmall,
                 big.mark = ',')
  sd <- format(sd, trim = T, drop0trailing = F, digits = sigfig,
               nsmall = nsmall,
               big.mark = ',')
  mean_sd <- paste0(mean$x, "(", sd$x, ")")
  row <- c(name, mean_sd)
  p_val <- NULL
  if(p){
    df <- cbind.data.frame(x, y)
    p_val <- anova(lm(x ~ y, df))$`Pr(>F)`[1]
    if(!is.na(p_val) & p_val < 0.01){
      p_val <- '<0.01'
    } else {
      p_val <- format(p_val, trim = T, nsmall = 2, digits = 1)
    }
    row <- c(row, p_val)
    p_val <- ''
  }
  incl_missing <- eval(substitute(incl_missing), parent.frame(home_env))
  if(!incl_missing) return(row)
  if(sum(is.na(x)) == 0) return(row)
  Missing <- MissingCont(x, y)
  row <- matrix(c(row, Missing, p_val), nrow = 2, byrow = T)
  return(row)
}

#' @describeIn returnRow method for characters - converts to factor and then
#' runs returnRow.factor
#' @export

returnRow.character <- function(x, y, p){
  x <- factor(x)
  returnRow(x, y, p)
}

#' @describeIn returnRow methods for numerics with request for Median and IQR
#' @export


returnRow.MedIQR <- function(x, y, p){
  home_env <- getHome()
  name <- eval(getName())
  sigfig <- eval(quote(sigfig), parent.frame(home_env))
  y <- as.data.frame(y)
  row <- aggregate(x, by = y, quantile,
                   probs = c(0.5, 0.25, 0.75),
                   simplify = F,
                   na.rm = T)$x
  row <- lapply(row, format, trim = T, digits = sigfig,
                drop0trailing = F, big.mark = ',')
  row <- sapply(row, paste0,
                ... =  c("(", "-", ")"),
                collapse = '')
  row <- c(name, row)
  incl_missing <- eval(substitute(incl_missing), parent.frame(home_env))
  p_val <- NULL
  if(p){
    row <- c(row, NA)
    p_val <- ''
  }
  if(!incl_missing) return(row)
  if(sum(is.na(x)) == 0) return(row)
  Missing <- MissingCont(x, y)
  row <- matrix(c(row, Missing, p_val), nrow = 2, byrow = T)
  return(row)
}

#' @describeIn returnRow method for integers - returns Median and 25%-75%
#' @export

returnRow.integer <- function(x, y, p){
  returnRow.MedIQR(x, y, p)
}

#' @describeIn returnRow method for logicals - converts to factor and then
#' runs returnRow.factor (with True as the first level)
#' @export

returnRow.logical <- function(x, y, p){
  x <- factor(x, levels = c('TRUE', 'FALSE'))
  returnRow(x, y, p)
}



#' Internal functions for returning the number of missing for continuous
#' variable
#'
#' @param x the row variable
#' @param y the column variable
#'
#' @importFrom stats aggregate


MissingCont <- function(x,y){
  number_missing <- aggregate(x, by = y, function(x) sum(is.na(x)),
                              simplify = T)[, 2]
  pct <- round(number_missing/table(y)*100, 0)
  Missing <- paste0(format(number_missing, big.mark = ',', trim = T),
                    "(", pct, ")")
  Missing <- c('\\ Missing(N%)', Missing)
}
