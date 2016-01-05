################################################################
### General R functions for loading and cleaning data
### Jonathan A. Whiteley        R v3.2.2        2016-01-05
################################################################

################################################################
## LOAD DATA

readWorksheets <- function (wb, sheet, pkg="readxl", ...)
{
  if (pkg=="openxlsx") {
    require(openxlsx)
    fun <- readWorkbook
  } else {
    require(readxl)
    fun <- read_excel
  }
  result <- lapply(sheet, function (.sheet)
                   {
                     do.call(fun, list(wb, .sheet, ...))
                   })
  names(result) <- sheet
  return(result)
}

read_sheets <- function (wb, sheet, col_types=NULL, ...)
{
  require(readxl)
  result <- 
    lapply(sheet, function (.sheet)
           {
             if (!is.null(col_types)) {
               ## need the sheet NUMBER to get number of columns
               sheets <- readxl::excel_sheets(wb)
               sheet.num <- match(.sheet, sheets) -1
               ## get the number of columns
               ##  this actually throws errors with xls files; maybe best to just read the first row (with header) and get the number of columns from that
               col.types <- 
                 switch(readxl:::excel_format(wb),
                        xls  = readxl:::xls_col_types( wb, sheet.num, nskip=0, n=1),
                        xlsx = readxl:::xlsx_col_types(wb, sheet.num, nskip=0, n=1)
                        )
               if (length(col_types) != length(col.types))
                 col_types <- rep(col_types, length(col.types))
             }
             read_excel(wb, .sheet, col_types=col_types, ...)
           })
  names(result) <- sheet
  return(result)
}




################################################################
## CLEAN DATA

trim <- function(string)
{
  return( gsub("^\\s+|\\s+$", "", string) )
}


