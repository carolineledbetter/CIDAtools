proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)
  attach(dots)
  on.exit(detach(dots))
  ProjectName <- paste0(path)

  #############################################################################
  ### Setup ReadMe Files
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

  # readme files
  CreateReadMe(path = path)

  # create a meta file for project info
  if(meta){
    dir.create(paste0(path, '/.ProjData'))
    ProjData <- list(ProjectName = ProjectName, PI = PI, analyst = analyst)
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }

  ############################################################################
  ### setup git


  if(git_lfs){
    LFS_text <- paste0(c("Data*/*/* filter=lfs diff=lfs merge=lfs -text",
                         "*/ReadMe.md !filter !diff !merge text=auto",
                         "*.csv filter=lfs diff=lfs merge=lfs -text",
                         "*.xls* filter=lfs diff=lfs merge=lfs -text",
                         "*.RDa* filter=lfs diff=lfs merge=lfs -text",
                         "*.rda* filter=lfs diff=lfs merge=lfs -text",
                         "*.Rda* filter=lfs diff=lfs merge=lfs -text",
                         "*.RDA* filter=lfs diff=lfs merge=lfs -text"),
                       collapse = ' \n ')
    writeLines(LFS_text,
               con = file.path(path, ".gitattributes"))
  }

  if(git_init){
    if (!requireNamespace('git2r', quietly = T)) {
      warning('git2r is required for git initialization')
    } else{
      repo<- git2r::init(path)
      if(remote_origin != '') git2r::remote_add(repo, 'origin', remote_origin)
      if(initcommit) {
        git2r::add(repo, 'ReadMe.md')
        git2r::commit(repo, message = 'Initial Commit')
      }
    }
  }

  if(nodata){
    gitignore <- paste0(c("# Session Data files",".RData",
                          "# R Data files","*.RData","*.rda","*.rdata","*.rda",
                          "*.csv","*.txt","*.dat",
                          "*.xls*",
                          "*.sas7bdat","*.xport",
                          "*.mdb",
                          "DataRaw/","DataProcessed/"), collapse = '\n')
    writeLines(gitignore, con = file.path(path, '.gitignore'))
  }



}




