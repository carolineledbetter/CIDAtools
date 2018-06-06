#' Set Default Analyst Value
#'
#'
#' This function allows you to set the option CIDAtools.analyst and will simultanesouly change the default in New Cida Project Template.
#'
#' @param AnalystName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options Analyst
#' @export
#'

setAnalyst <- function(AnalystName){
  options(CIDAtools.analyst = AnalystName)
  Project_setup <- paste0('/Library/Frameworks/R.framework/Versions/',
                          'Current/Resources/library/CIDAtools/rstudio/',
                          'templates/project/proj_setup.dcf')
  read.dcf(file.path(Project_setup)) -> DCF
  pos <- grep('Analyst', DCF) + nrow(DCF)
  DCF[pos] <- AnalystName
  write.dcf(DCF, file.path(Project_setup))
  return(paste('The default analyst name has been changed to', AnalystName))
}


proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)
  attach(dots)
  ProjectName <- paste0(path)

  if(git_lfs){
  LFS_text <- paste0(c("DataRaw/* filter=lfs diff=lfs merge=lfs -text",
                       "DataProcessed/* filter=lfs diff=lfs merge=lfs -text"),
                     collapse = '\n')
  writeLines(LFS_text,
             con = file.path(path, ".gitattributes"))
  }

  if(git_init){
    git2r::init(path)
  }

  if(nodata){
    gitignore <- paste0(c('*.RD*', '*.csv',
                          '.xls*', '*.txt', 'Data*',
                          '*.dat'), collapse = '\n')
    writeLines(gitignore, con = file.path(path, '.gitignore'))
  }

  readme <- c(paste0("# ", ProjectName, "  "),
              paste0("**PI:**", PI, "  "),
              paste0("**Analyst**:", analyst, "  "),
              "",
              "Details about the folders:",
              '',
              "File | Description",
              "---|---------------------------------------------------------------------",
              "Admin | contains the scope of work and other administrative documents",
              "Background | contains the background information for the analysis",
              "Code | contains all R scripts for this project",
              "DataRaw | contain all raw data provided by investigators",
              "DataProcessed | contains the processed data used for analysis",
              "Dissemination | contains any materials produced for dissemination, ie. Abstracts, Posters, Papers",
              "Reports | contains all output, rmarkdown files and report")


  # write to readme file
  writeLines(paste0(readme, collapse = '\n'),
             con = file.path(path, "ReadMe.md"))

  # create a meta file for project info
  if(meta){
    dir.create(paste0(path, '/.ProjData/'))
    ProjData <- list(ProjectName, PI, analyst)
    names(ProjData) <- list('ProjectName', 'PI', 'analyst')
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }
detach(dots)

}




