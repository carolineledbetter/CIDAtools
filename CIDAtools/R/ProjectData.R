#' Set Project Analyst
#' 
#' This function allows you to set the  project analyst. This will overwrite the current value if exists.
#'
#' @param AnalystName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options Analyst ProjData
#' @export
#'
SetProjectAnalyst <- function(AnalystName){
  if(!is.character(AnalystName)) stop('Analyst Name must be a character string')
  if(length(AnalystName) > 1) {
    warning('Only First String is Used')
    AnalystName <- AnalystName[1]
  }
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
  }
  else{
    dir.create(paste0('.ProjData/'))
    ProjData <- list()
    }
  ProjData$analyst <- AnalystName
  write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  return(paste('The Project Analyst name has been changed to', AnalystName))
}

#' Set Project Name
#' 
#' This function allows you to set the  project name. This will overwrite the current value if exists.
#'
#' @param ProjectName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options ProjectName ProjData
#' @export
#'
SetProjectName <- function(ProjectName){
  if(!is.character(ProjectName)) stop('Project Name must be a character string')
  if(length(ProjectName) > 1) {
    warning('Only First String is Used')
    ProjectName <- ProjectName[1]
  }
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
  }
  else{
    dir.create(paste0('.ProjData/'))
    ProjData <- list()
  }
  ProjData$analyst <- ProjectName
  write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  return(paste('The Project name has been changed to', ProjectName))
}

#' Set PI Name
#' 
#' This function allows you to set the Project's PI. This will overwrite the current value if exists.
#'
#' @param PI A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options PI ProjData
#' @export
#'
SetProjectPI <- function(PI){
  if(!is.character(PI)) stop('PI Name must be a character string')
  if(length(PI) > 1) {
    warning('Only First String is Used')
    PI <- PI[1]
  }
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
  }
  else{
    dir.create(paste0('.ProjData/'))
    ProjData <- list()
  }
  ProjData$analyst <- PI
  write.dcf(ProjData, file.path(path, '/.ProjData/Data.dcf'))
  return(paste('The Project PI has been changed to', PI))
}

#' Get Project Analyst
#' 
#' This function returns the Project Analyst Name. If none exists, it
#' will return the value of CIDAtools.analyst option or blank if the option is not set.
#'
#' @return A character string with the analyst name
#' @keywords options Analyst ProjData
#' @export
#'

ProjectAnalyst <- function(){
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('analyst' %in% names(ProjData)) return(ProjData$analyst)
  }
  if(!is.null(getOption('CIDAtools.analyst'))){
    return(getOption('CIDAtools.analyst'))
  }
  return('')
}

#' Get Project Name
#' 
#' This function returns the Project Name or blank if none exists.
#'
#' @return A character string with the project name
#' @keywords options ProjData ProjectName
#' @export
#'

ProjectName <- function(){
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('ProjectName' %in% names(ProjData)) return(ProjData$ProjectName)
  }
  return('')
}

#' Get PI Name
#' 
#' This function returns the PI Name or blank if none exists.
#'
#' @return A character string with the PI name
#' @keywords options ProjData PI
#' @export
#'

ProjectPI <- function(){
  if(file.exists(file.path('.ProjData/Data.dcf'))){
    ProjData <- read.dcf(file.path('.ProjData/Data.dcf'), all = T)
    if('PI' %in% names(ProjData)) return(ProjData$PI)
  }
  return('')
}