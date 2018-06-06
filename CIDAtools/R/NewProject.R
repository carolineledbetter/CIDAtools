proj_setup <- function(path, ...){
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  dots <- list(...)
  ProjectName <- paste0(path)
  # PI <- dots$PI
  LFS_text <- paste0(c("DataRaw/* filter=lfs diff=lfs merge=lfs -text",
                       "DataProcessed/* filter=lfs diff=lfs merge=lfs -text"), 
                     collapse = '\n')
  writeLines(LFS_text, 
             con = file.path(path, ".gitattributes"))
  
  readme <- c(paste0("# ", ProjectName, "  "),
              paste0("**PI:**", dots$PI, "  "),
              paste0("**Analyst**:", dots$analyst, "  "),
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
}

  
  

