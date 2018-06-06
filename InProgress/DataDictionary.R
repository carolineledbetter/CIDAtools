#' Creates an data dictionary 
#' 
#' Creates an data dictionary in a excel file. By default this is in the background
#' folder, but can be changed with the option dicPath
#' The first worksheet will be named contents and will contain the Project Name, 
#' PI, and date created. These options can be set with SetProjectName and 
#' SetProjectPI. If not files or lists are passed, the data dictionary will be created
#' from data frames in the global environment. 
#' 
#' @param files a list of .rda or .RData files that contain dataframes to be included in the dictionary
#' @param datalist a list of dataframes to be included in the data dictionary
#' @param dicPath file path where excel file should be placed. It is the projects
#' background folder by default. 
#' @param maxVal This sets the maximum number of factor levels (variable values) that will be printed. 
#' This keeps R from printing all the values for large factors (for example if the record number is a factor). 
#' The default is 25. 
#' @return This creates an excel data dictionary and returns the file path where it was placed. 
#' @import xlsx
#' @export
#' @keywords DataDictionary
#' 

createExcelDictionary <- function(files = NULL, datalist = NULL, 
                                  dicPath = 'Background/', 
                                  maxVal = 25){
  #check parameters
  if (!is.null(files) & !is.list(files)) stop('Files must be a list')
  if (!is.null(datalist) & !is.list(datalist)) stop('Datalist must be a list')
  if (!is.character(dicPath)) stop('dicPath must be a string')
  if (length(dicPath) > 1) {
    warning('Only first string element will be used')
    dicPath <- dicPath[1]
  }
  if (!is.numeric(maxVal)) stop('maxVal must be a number')
  if (length(maxVal) > 1) {
    warning('Only first element will be used')
    maxVal <- maxVal[1]
  }
  # load .rda and .rdata files
  lapply(files, load)
  # Set Project Name
  Project <- 'Project: "Insert Project Name Here'
  #Set PI
  PI <- 'PI: Insert PI Here'
  
  # set maximum number of values to list
  mVal <- 25
  
  # function to get values (of a variable ie. Male, Female for gender)
  values <- function(variable){
    if(is.character(variable)){
      fxrs <- levels(as.factor(variable))
    }
    else fxrs <- levels(variable)
    if(length(fxrs) > mVal) fxrs <- NULL
    return(fxrs)
  }
  
  # function to make data dictionary
  makedic <- function(df){
    df <- get(df)
    Values <- lapply(df, values)
    Values <- sapply(Values, paste0, collapse = '\n')
    Type <- sapply(df, class)
    # POSIXt classes will have 2 classes need to remove one and 
    # set to date
    Type <- lapply(Type, sub, pattern = 'POSIXt', replacement = "Date")
    Type <- sapply(Type, grep, pattern = 'POSI', value = T, invert = T)
    dic <- data.frame(Variables = names(df), Type, 
                      Values = Values, 
                      stringsAsFactors = F, row.names = NULL)
    return(dic)
  }
  
  
  # get names of all data frames
  dataframes <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
  
  
  # create excel workbook
  wb <- createWorkbook(type = 'xlsx')
  
  # wrap text for values so that each one prints on new line
  Values_Style <- CellStyle(wb, alignment = Alignment(wrapText = T, 
                                                      vertical = 'VERTICAL_TOP'))
  VarType_Style <- CellStyle(wb, 
                             alignment = Alignment(vertical = 'VERTICAL_TOP'))
  
  # bold column names and data table names /set data table names to font size 14 
  
  ColNames_Style <- CellStyle(wb) + Font(wb, isBold = T)
  Contents_Style <- CellStyle(wb) + Font(wb, isBold = T, heightInPoints = 14)
  Hyperlink_Style <- CellStyle(wb) + 
    Font(wb, color = 'blue', underline = 1, heightInPoints = 14)
  
  # Create a contents sheet at the front of the workbook
  contents <- createSheet(wb, 'Contents')
  
  # Add Project and PI
  rowTitle <- createRow(contents, 1:3)
  cellTitle <- createCell(rowTitle, colIndex = 1)
  setCellValue(cellTitle[[1, 1]], value = Project)
  setCellValue(cellTitle[[2, 1]], value = PI)
  # set style of Title
  sapply(cellTitle[, 1], setCellStyle, cellStyle = Contents_Style)
  
  rows <- createRow(contents, 4:(length(dataframes) + 3))
  cells <- createCell(rows, colIndex = 1)
  # Put names of dataframes
  mapply(setCellValue, cell = cells[, 1], dataframes)
  # Hyperlink to their sheets
  mapply(addHyperlink, cell = cells[, 1], 
         address = paste0(dataframes, '!A1'), 
         linkType = 'DOCUMENT')
  # set style of Contents sheet
  sapply(cells[, 1], setCellStyle, cellStyle = Hyperlink_Style)
  autoSizeColumn(contents, 1)
  setRowHeight(rows, multiplier = 2)
  
  
  # make a sheet for each data frame and place contents of dictionary in it
  makesheet <- function(dfname){
    sheet <- createSheet(wb, dfname)
    addDataFrame(makedic(dfname), sheet = sheet, startRow = 1, 
                 row.names = FALSE, 
                 colStyle = list('3' = Values_Style, 
                                 '1' = VarType_Style, 
                                 '2' = VarType_Style), 
                 colnamesStyle =ColNames_Style )
    autoSizeColumn(sheet, 1:3)
  }
  
  
  
  sapply(dataframes, makesheet)
  
  
  saveWorkbook(wb, 'Background/DataDictionary.xlsx')
  
}
#check parameters

# set the .RData object containing all data tables
load(file = 'DataRaw/All.RData')
# Set Project Name
Project <- 'Project: "Insert Project Name Here'
#Set PI
PI <- 'PI: Insert PI Here'

# set maximum number of values to list
mVal <- 25

# function to get values (of a variable ie. Male, Female for gender)
values <- function(variable){
  if(is.character(variable)){
    fxrs <- levels(as.factor(variable))
  }
  else fxrs <- levels(variable)
  if(length(fxrs) > mVal) fxrs <- NULL
  return(fxrs)
}

# function to make data dictionary
makedic <- function(df){
  df <- get(df)
  Values <- lapply(df, values)
  Values <- sapply(Values, paste0, collapse = '\n')
  Type <- sapply(df, class)
  # POSIXt classes will have 2 classes need to remove one and 
  # set to date
  Type <- lapply(Type, sub, pattern = 'POSIXt', replacement = "Date")
  Type <- sapply(Type, grep, pattern = 'POSI', value = T, invert = T)
  dic <- data.frame(Variables = names(df), Type, 
                    Values = Values, 
                    stringsAsFactors = F, row.names = NULL)
  return(dic)
}


# get names of all data frames
dataframes <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]


# create excel workbook
wb <- createWorkbook(type = 'xlsx')

# wrap text for values so that each one prints on new line
Values_Style <- CellStyle(wb, alignment = Alignment(wrapText = T, 
                                                    vertical = 'VERTICAL_TOP'))
VarType_Style <- CellStyle(wb, 
                           alignment = Alignment(vertical = 'VERTICAL_TOP'))

# bold column names and data table names /set data table names to font size 14 

ColNames_Style <- CellStyle(wb) + Font(wb, isBold = T)
Contents_Style <- CellStyle(wb) + Font(wb, isBold = T, heightInPoints = 14)
Hyperlink_Style <- CellStyle(wb) + 
  Font(wb, color = 'blue', underline = 1, heightInPoints = 14)

# Create a contents sheet at the front of the workbook
contents <- createSheet(wb, 'Contents')

# Add Project and PI
rowTitle <- createRow(contents, 1:3)
cellTitle <- createCell(rowTitle, colIndex = 1)
setCellValue(cellTitle[[1, 1]], value = Project)
setCellValue(cellTitle[[2, 1]], value = PI)
# set style of Title
sapply(cellTitle[, 1], setCellStyle, cellStyle = Contents_Style)

rows <- createRow(contents, 4:(length(dataframes) + 3))
cells <- createCell(rows, colIndex = 1)
# Put names of dataframes
mapply(setCellValue, cell = cells[, 1], dataframes)
# Hyperlink to their sheets
mapply(addHyperlink, cell = cells[, 1], 
       address = paste0(dataframes, '!A1'), 
       linkType = 'DOCUMENT')
# set style of Contents sheet
sapply(cells[, 1], setCellStyle, cellStyle = Hyperlink_Style)
autoSizeColumn(contents, 1)
setRowHeight(rows, multiplier = 2)


# make a sheet for each data frame and place contents of dictionary in it
makesheet <- function(dfname){
  sheet <- createSheet(wb, dfname)
  addDataFrame(makedic(dfname), sheet = sheet, startRow = 1, 
               row.names = FALSE, 
               colStyle = list('3' = Values_Style, 
                               '1' = VarType_Style, 
                               '2' = VarType_Style), 
               colnamesStyle =ColNames_Style )
  autoSizeColumn(sheet, 1:3)
}



sapply(dataframes, makesheet)


saveWorkbook(wb, 'Background/DataDictionary.xlsx')
