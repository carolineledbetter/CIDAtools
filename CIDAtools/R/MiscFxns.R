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


setPermanentAnalyst <- function(Name){
  options(CIDAtools.analyst = Name)
  site_path = R.home(component = "home")
  fname = file.path(site_path, "etc", "Rprofile.site")
  opts <- character()
  if(file.exists(fname)){
    opts <- readLines(fname)
  }
  if(sum(grepl("CIDAtools.analyst", opts)) > 0){
    opts <- gsub("CIDAtools.analyst = .+)",
                 paste0("CIDAtools.analyst = '",
                        paste0(Name), "')"),
                 opts)
  } else {
    opts <- c(opts, paste0("(CIDAtools.analyst = '",
                           paste0(Name), "')"))
  }
  if(!file.create(fname, showWarnings = F))
    stop()
  writeLines(opts, fname)
}
