.onLoad <- function(libname, pkgname){
  op <- options()
  op.CIDAtools <- list(
    CIDAtools.analyst = 'Default Analyst Name'
  )
  toset <- !(names(op.CIDAtools) %in% names(op))
  if(any(toset)) options(op.CIDAtools[toset])
  
  invisible()
}


