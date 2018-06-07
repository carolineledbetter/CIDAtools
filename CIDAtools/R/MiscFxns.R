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
  setPermanentAnalyst(AnalystName)
  options(CIDAtools.analyst = AnalystName)
  site_path = R.home(component = "home")
  Project_setup <- paste0(site_path,
                          '/library/CIDAtools/rstudio/',
                          'templates/project/proj_setup.dcf')
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
#' Retrieve the number of rows in dataframe of matrix with commas inserted for nice reports.
#'
#' @param x data frame or matrix
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynrow
#' @export
#'



nrowP <- function(x){
  format(nrow(df), big.mark = ',', trim = T)
}
