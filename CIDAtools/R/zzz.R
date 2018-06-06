.onLoad <- function(libname, pkgname){
  defaultAnalyst <- getOption('CIDAtools.analyst')
  if(is.null(defaultAnalyst)){
    defaultAnalyst <- 'Default Analyst Name'
  }
  setPermanentAnalyst(defaultAnalyst)
  invisible()
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
  writeLines(opts, fname)
}


