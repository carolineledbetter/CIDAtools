.onLoad <- function(libname, pkgname){
  defaultAnalyst <- getOption('CIDAtools.analyst')
  if(is.null(defaultAnalyst))
    options('CIDAtools.analyst' = 'Default Analyst Name')
  # see if Rprofile exists
  site_path = R.home(component = "home")
  fname = file.path(site_path, "etc", "Rprofile.site")
  if(file.exists(fname)){
    opts <- readLines(fname)
    opts <- gsub("CIDAtools.analyst = .+)",
                 paste0("CIDAtools.analyst = '",
                   paste0(defaultAnalyst), "')"),
                 opts)
    writeLines(opts, fname)
  }

  else {
    file.create(fname)
    writeLines(paste0("options(CIDAtools.analyst = '",
                   paste0(defaultAnalyst), "')"),
               fname)
    }

  invisible()
}


