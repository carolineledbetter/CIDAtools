#' Create ReadMe.md files
#'
#' This function creates ReadMe.md in the CIDA templates for the standard
#' file structure
#'
#'@param template Which ReadMe.md files should be created? Default is all. Partial
#'matching does work.
#'@param path Where should they be created? Default is the working directory.
#'@return
#'This function creates the desired readme files. It will not overwrite the file
#'however if it does not exists. It does not return anything.
#'@keywords ReadMe ReadMe.md
#'@export
CreateReadMe <- function(template = c('Admin', 'Background', 'Code', 'DataRaw',
                                      'DataProcessed', 'Dissemination',
                                      'Reports'), path = getwd()){
  # set which ReadMe.md files to create
  template <- match.arg(template, several.ok = T)

  # create list with lines for each template
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

  # Function for creating the directory
  createDir <- function(x){
    paste0(path, '/', x)
  }

  createFiles <- function(x){
    file.path(path, paste0(x, '/ReadMe.md'))
  }

  readme <- readme[template]

  pathnames <- sapply(names(readme), createDir)
  dir_created <- lapply(pathnames, dir.create, showWarnings = F, recursive = T)
  con <- lapply(names(readme), createFiles)
  doNotOverwrite <- sapply(con, file.exists)
  readme <- readme[!doNotOverwrite]
  con <- con[!doNotOverwrite]
  files_created <- mapply(writeLines, lapply(readme, paste0, collapse = '\n'), con)
}
