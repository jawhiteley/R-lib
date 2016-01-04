##################################################
# Jonathan Whiteley		R v2.12		2011-04-21
##################################################

strip_empty_dims  <- function( data = NULL, dim = c(1, 2), 
							  rows = NULL, cols = NULL, col.class = NULL ) {
  ## strip whole rows or columns if they are entirely empty (NA or NaN)
  ## `data` is the data frame to strip empty values from.
  ## `rows` is a vector of indices or logical values of 
  ##        which rows to check for empty values (default all)
  ## `cols` see `rows` above (but applied to columns)
  ## `dim`  the dimensions to strip if all empty
  ## `col.class` is a vector of classes: columns that match will be included
  ##             This applies to the `cols` argument.

  if (!is.data.frame(data) && !is.matrix(data)) stop(
	  "The first argument must be a \"data.frame\" or \"matrix\"."
     )

  cols <- as.vector(cols)
  if ( is.null(cols) ) {
	cols <-  1:ncol(data)
  }
  if ( !is.null(col.class) ) {
	check.cols <- which( lapply(data[, cols], class) %in% as.vector(col.class) )
	check.cols <- cols[check.cols]
  } else {
	check.cols <- cols
  }
  rows <- as.vector(rows)
  if ( is.null(rows) ) {
	rows <-  1:nrow(data)
  }  
  
  empty.rows <- rows[ which( apply( data[rows, check.cols], 1, function(x) all(is.na(x)) ) ) ]
  empty.cols <- check.cols[ which( apply( data[rows, check.cols], 2, function(x) all(is.na(x)) ) ) ]

  if (any(dim == 1) & length(empty.rows) > 0 ) {
	data <- data[-empty.rows, ]
  }
  if (any(dim == 2) & length(empty.cols) > 0 )  {
	data <- data[, -empty.cols]
  }
  
  return(data)
  
  if (FALSE) {  # Testing
	test.df <-  data.frame( )
	df.proc <- strip_empty_dims(test.df)
	df.proc
  }
}


