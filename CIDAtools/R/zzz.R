.onLoad <- function(libname, pkgname){
  defaultAnalyst <- getOption('CIDAtools.analyst')
  if(is.null(defaultAnalyst)){
    defaultAnalyst <- 'Default Analyst Name'
  }
  options(CIDAtools.analyst = defaultAnalyst)
  invisible()
}

