.onLoad <- function(libname, pkgname){
  site_path <- R.home(component = "home")
  Project_setup <- paste0(site_path,
                          '/library/CIDAtools/rstudio/',
                          'templates/project/proj_setup.dcf')
  if(file.access(Project_setup, 4) == 0){
    DCF <- read.dcf(file.path(Project_setup), all = T)
    Analyst <- DCF$Default[DCF$Parameter == 'analyst' &
                             !is.na(DCF$Parameter)]
    options(CIDAtools.analyst = Analyst)
  }
  invisible()
}
