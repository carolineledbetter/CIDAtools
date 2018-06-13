proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)
  attach(dots)
  on.exit(detach(dots))
  ProjectName <- paste0(path)

  if(git_lfs){
  LFS_text <- paste0(c("Data*/** filter=lfs diff=lfs merge=lfs -text",
                      "*/ReadMe.md !filter !diff !merge !text"),
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

  readme <- list()

  readme$Admin <- c("# Admin  ",
                    "",
                    "This folder contains the scope of work and other relevant files from CIDA admin.  ",
                    "",
                    "Details about the files:  ",
                    "",
                    "File | Description",
                    "---|---------------------------------------------------------------------",
                    "",
                    "")
  readme$Background <- c("# Background  ",
                         "  ",
                         "This folder contains documents provided by investigators and the data analysis ",
                         "plan.  ",
                         "  ",
                         "Details about the files:  ",
                         "  ",
                         "File | Description",
                         "---|---------------------------------------------------------------------",
                         "  ")
  readme$Code <- c("This folder contains all the code.  ",
                   "  ",
                   "Details about the files in this folder:",
                   "  ",
                   "File | Description",
                   "---|---------------------------------------------------------------------",
                   "  ")
  readme$DataProcessed <- c("# Processed Data  ",
                            "",
                            "Scripts that created the files in this folder:  ",
                            "",
                            "File | Script",
                            "---|---------------------------------------------------------------------",
                            "")
  readme$DataRaw <- c("# Raw Data",

                      "Details about the files:",

                      "File | Details",
                      "---|---------------------------------------------------------------------",
                      "    ",
                      "")

  readme$Dissemination <- c("# Dissemination",

                            "This folder contains abstracts, posters, papers and anything else produced  ",
                            "for dissemination.  ",

                            "Details about the files:",

                            "File | Description",
                            "---|---------------------------------------------------------------------",
                            "     ",
                            "")
  readme$Reports <- c("# Reports",

                      "This folder contains the rmardown scripts and pdf output of reports.  ",

                      "Details about the files:",

                      "File | Description",
                      "---|---------------------------------------------------------------------",
                      "  ",
                      "")
  createDir <- function(x){
    paste0(path, '/', x)
  }

  createFiles <- function(x){
    file.path(path, paste0(x, '/ReadMe.md'))
  }
  pathnames <- sapply(names(readme), createDir)
  dir_created <- lapply(pathnames, dir.create, showWarnings = F, recursive = T)
  con <- lapply(names(readme), createFiles)
  files_created <- mapply(writeLines, lapply(readme, paste0, collapse = '\n'), con)

  # create a meta file for project info
  if(meta){
    dir.create(paste0(path, '/.ProjData'))
    ProjData <- list(ProjectName = ProjectName, PI = PI, analyst = analyst)
    write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  }


}




