#' Read in xlsx with fill colour
#'
#' Reads in the fill colour of excel workbooks. Creates a data frame for each
#' sheet in a list if mutliple sheets are requested. Creates a colour column for
#' each colour column specified.
#'
#' @param file the path to the file you intend to read. Can be an xls or xlsx format.
#' @param colorColumns a vector of column numbers for which you want to read the colour
#' @param sheet NULL(default) for all sheets otherwise a vector of sheet numbers
#' or names to read.
#' @param header should the 1st row be read in as a header? defaults to T.
#' @return A data frame for one sheet or a list of data frames for multiple sheets
#' @import xlsx
#' @export
#' @keywords Excel colour color xlsx
#'
read.xlsx.withcolor <- function(file, colorColumns, sheet = NULL, header = T){
  if(!requireNamespace("xlsx", quietly = TRUE))
    stop("package 'xlsx' is required.")
  if(!is.numeric(colorColumns)) stop('Please pass coloured column by number')
  if(!is.null(sheet)){
    if(!is.numeric(sheet)&!is.character(sheet))
      stop('Sheets must be numbers or names')
  }
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  if(header) h <- -1 else h <- seq_along(rows)
  if(!is.null(sheet)) sheets <- sheets[sheet]
  createData <- function(sheet){
    rows  <- getRows(sheet)
    head <- getCells(rows[-h])
    cells <- getCells(rows[h])
    df <- data.frame(matrix(sapply(cells, getCellValue),
                            nrow = length(rows) - header,
                            byrow = T))
    if(!is.null(head)) names(df) <- sapply(head, getCellValue)

    cellColor <- function(style) {
      fg  <- style$getFillForegroundXSSFColor()
      rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
      rgb <- paste(rgb, collapse = "")
      return(rgb)
    }
    getColor <- function(col){
      styles <- sapply(getCells(rows[h], col), getCellStyle)
      colours <- sapply(styles, cellColor)
    }
    colours <- lapply(colorColumns, getColor)
    names(colours) <- paste('colour', seq_along(colours))
    df <- cbind(df, colours)
    return(df)
  }
  z <- lapply(sheets, createData)
  if(length(sheets) == 1) return(z[[1]])
  return(z)
}

