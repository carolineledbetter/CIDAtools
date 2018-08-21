#' Set Default Analyst Value
#'
#'
#' This function allows you to set the option CIDAtools.analyst permanently
#' (until you change it or reinstall CIDAtools) and will
#' simultanesouly change the default in New Cida Project Template.
#'
#' @param AnalystName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options Analyst
#' @export
#'

setAnalyst <- function(AnalystName){
  if(!is.character(AnalystName)) stop('Analyst Name must be a character string')
  if(length(AnalystName) > 1) {
    warning('Only First String is Used')
    AnalystName <- AnalystName[1]
  }
  AnalErr <- try(setPermanentAnalyst(AnalystName), silent = T)
  msg1 <- NULL
  if(!is.null(AnalErr)) msg1 <- paste0('Default Analyst can not be ',
                                       'saved permanently.\n',
                                       'You will need to set for each ',
                                       'R session.\n')
  site_path = R.home(component = "home")
  Project_setup <- paste0(site_path,
                          '/library/CIDAtools/rstudio/',
                          'templates/project/proj_setup.dcf')
  if(file.access(Project_setup, 2) == -1)
    stop(paste0(msg1,
                'You do not have permission to change\n',
                'New CIDA Project Template'))
  DCF <- read.dcf(file.path(Project_setup), all = T)
  DCF$Default[DCF$Parameter == 'analyst' &
                !is.na(DCF$Parameter)] <- AnalystName
  write.dcf(DCF, file.path(Project_setup))
  return(paste('The default analyst name has been changed to',
               getOption('CIDAtools.analyst')))
}

#' Get pretty numbers of rows
#'
#'
#' Retrieve the number of rows in dataframe of matrix with commas inserted for
#' nice reports.
#'
#' @param x data frame or matrix
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynrow
#' @export
#'



nrowP <- function(x){
  format(nrow(x), big.mark = ',', trim = T)
}

#' Get pretty number of levels
#'
#'
#' Just a wrapper for format(nlevels) with big.mark = , and trim = T
#'
#' @param x factor
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynlevels
#' @export
#'

nLevelsP <- function(x){
  format(nlevels(x), big.mark = ',', trim = T)
}

#' Set Default Analyst Value
#'
#'
#' This is an internal function that writes the Default Analyst name to the
#' users Rprofile.
#'
#' @param Name A string containing the analyst name
#'
setPermanentAnalyst <- function(Name){
  options(CIDAtools.analyst = Name)
  fname = file.path("~/.Rprofile")
  opts <- character()
  if(file.exists(fname)){
    opts <- readLines(fname)
  }
  opts[grep('options\\(CIDAtools.analyst = ', opts, invert = T)] -> opts
  opts <- c(opts, paste0("options(CIDAtools.analyst = '",
                           paste0(Name), "')"))
  if(!file.create(fname, showWarnings = F))
    stop()
  writeLines(opts, fname)
}

#' Remove Default Analyst from ~/.Rprofile
#'
#' This function removes the default analyst set with setAnalyst() from the users
#' .Rprofile. If this is the only entry in .Rprofile it will remove the file as well.
#'
#' @param quiet should a message indicating result be returned, if TRUE will only
#' return TRUE or FALSE
#'
#' @return Message indicating sucess or failue
#' @keywords Analyst remove
#' @export
#'
#'
removeAnalyst <- function(quiet = F){
  fname = file.path("~/.Rprofile")
  if(file.access(fname, 4) != 0){
    if(!quiet){
    return('User does not have an Rprofile or Rprofile can not be read')
    }
    return(FALSE)
  }
  opts <- readLines(fname)
  opts[grep('options\\(CIDAtools.analyst = ', opts, invert = T)] -> opts
  if(file.access(fname, 2) != 0){
    if(!quiet){
      return('You do not have permission to write to users Rprofile')
    }
    return(FALSE)
  }
  if(length(opts) == 0){
    file.remove(fname)
    if(!quiet){
    return('Users .Rprofile is empty and was deleted')
    }
    return(TRUE)
  }
  writeLines(opts, fname)
  if(!quiet)
    return('options(CIDAtools.analyst) has been removed from users profile')
  return(TRUE)
}

#' Convert Interval Notation
#'
#' Converts a vector from Interval Notation to less than equal to, less than,
#' etc.
#'
#' @param x a character vector to be converted
#'
#' @return a character vector of same length of x converted
#' @keywords interval notation
#' @export
#'
convertIntervalNotation <- function(x){
  if(!is.character(x)) stop('x must be a character vector')
  x <- gsub('\\(-Inf, ', '', x)
  x <- gsub(',Inf\\)', '', x)
  x <- gsub('\\[', '\u2265', x)
  x <- gsub('([0-9]+)\\]', '\u2264\\1', x)
  x <- gsub(',', ' - ', x)
  x <- gsub("\\(", '>', x)
  x <- gsub("([0-9]+)\\)", "<\\1", x)
  return(x)
}
