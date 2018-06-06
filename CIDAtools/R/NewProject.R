proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)
  attach(dots)
  ProjectName <- paste0(path)

  if(git_lfs){
  LFS_text <- paste0(c("DataRaw/* -X DataRaw/ReadMe.md  filter=lfs diff=lfs merge=lfs -text",
                      " DataProcessed/*  -X DataProcessed/ReadMe.mdfilter=lfs diff=lfs merge=lfs -text"),
                     collapse = '\n')
  writeLines(LFS_text,
             con = file.path(path, ".gitattributes"))
  }

  if(git_init){
    git2r::init(path)
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

  readme <- c("# Admin  ",
              "",
              "This folder contains the scope of work and other relevant files from CIDA admin.  ",
              "",
              "Details about the files:  ",
              "",
              "File | Description",
              "---|---------------------------------------------------------------------",
              "",
              "")
  dir.create(paste0(path, '/Admin'), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste0(readme, collapse = '\n'),
             con = file.path(path, "Admin/ReadMe.md"))

  # create a meta file for project info
  if(meta){
    dir.create(paste0(path, '/.ProjData/'))
    ProjData <- list(ProjectName = ProjectName, PI = PI, analyst = analyst)
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }
detach(dots)

}




