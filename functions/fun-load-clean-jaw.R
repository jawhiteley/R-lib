################################################################
### General R functions for loading and cleaning data
### Jonathan A. Whiteley        R v3.2.2        2016-02-05
################################################################

################################################################
## LOAD DATA

readWorksheets <- function (wb, sheet, pkg="readxl", ...)
{ ## Read multiple worksheets into a list of data.frames
  ## Still a work in progress.
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
{ ## Read multiple worksheets into a list of data.frames
  ## using 'readxl' package.
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

reforder <- function(vec="", ref="", drop.=FALSE, ...)
{ ## calculate order of a vector based on a (longer) reference list of the same values.
  ord <- match(vec, ref)               # match values in the reference vector.
  ord <- order(ord, ...)               # the order *of the matches* (NA at the end by default)
  if (drop.) {
    ## The same effect can be achieved by passing 'na.last=NA' to order(), above.
    x <- which(!vec %in% ref)          # get indices of missing values    
    ord <- setdiff(ord, x)             # drop unmatched values
  }
  return(ord)
  if (FALSE) {                         # test code
    test.vector <- c("#", sample(LETTERS, 10))
    test.vector
    test.vector[order(match(test.vector, LETTERS))]
    test.vector[reforder(test.vector, LETTERS, drop.=FALSE)]
    test.vector[reforder(test.vector, LETTERS, drop.=TRUE) ]
    test.vector[reforder(test.vector, rep(LETTERS, each=2) ) ]
    test.vector2 <- rep(test.vector, 2) 
    test.vector2[reforder(test.vector2, LETTERS) ]
  }
}

vlookup <- function(lookup_value="", table_array=data.frame(), col_index_num=2, col_lookup=1)
{ ## mimick Excel's vlookup function, for use in calculating values based on lookup tables.
  lookup.match <- match(lookup_value, table_array[[col_lookup]])
  if ("character" %in% class(col_index_num)) 
    col_index_num <- match(col_index_num, colnames(table_array))
  ## Extract values from a combination r,c coordinate pairs.
  ## I feel like there should be a faster and simpler vectorized way to do this (and that I've done it before), but this is the best I could come up with.
  lookup.coords <- cbind(row=lookup.match, col=col_index_num)
  values <- apply(lookup.coords, 1, function (x)
                  {
                    val <- table_array[x['row'], x['col']]
                    if (is.null(val)) val <- NA
                    if ("factor" %in% class(val)) val <- as.character(val)
                    val
                  })
  return( values )
  if (FALSE) {                         # test code
    vlookup( c(8:16, NA), data.frame(x=1:length(LETTERS), y=letters, z=LETTERS) )
    vlookup( c(8:16, NA), data.frame(x=1:length(LETTERS), y=letters, z=LETTERS), rep(c(2, 3, NA), length=length(c(8:16, NA))) )
  }
}



##==============================================================
## Column and Object Names

convert_names <- function(obj=NULL, new.names="")
{ ## Convert names of an object using a reference *named* vector,
  ## where the names are the new names of the object, and the values are the current names of the object, in the desired order.
  match2new <- match(new.names, names(obj)) 
  if ( any(is.na(match2new)) )
    warning("Some new.names not found in object names")
  names(obj) <- names(new.names)[match2new] # ensure the proper order is maintained
  if (is.null(dim(obj))) {
    obj[names(new.names)]
  } else {
    obj[names(new.names), ]
  }
}

regex_match <- function (patterns="", val="", ...)
{
  if ("list" %in% class(patterns))
  {
    regex_match.list(patterns=patterns, val=val, ...)
  } else {
    regex_match.base(patterns=patterns, val=val, ...)
  }
}

regex_match.base <- function (patterns="", val="", fun=grep, ...)
{ ## run regular expression matching on a list of patterns - until at least one match is found.
  result.list <- lapply(patterns, fun, val, ...)
  result.lengths <- sapply(result.list, length)
  result <- result.list[ which(result.lengths > 0)[1] ]
  return(result)
}

regex_match.list <- function(patterns="", val="", fun=grep, ...) 
{
  result <- sapply(patterns, function (pat) regex_match.base(pat, val, fun, ...) )
  return(result)
}

if (FALSE) {    # Test code
  test.pats <- COL_NAMES[[1]][1:10]
  test.vals <- names(test.pats)
  regex_match(test.pats, test.vals, value=TRUE, perl=TRUE, ignore.case=TRUE)
  test.pats[[1]] <- ".*"               # ensure duplicates
  regex_match(test.pats, test.vals, value=TRUE, perl=TRUE, ignore.case=TRUE)
  regex_match.base(test.pats, test.vals, value=TRUE, perl=TRUE, ignore.case=TRUE)
}



##==============================================================

