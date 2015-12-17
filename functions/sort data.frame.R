# FROM:  http://www.imachordata.com/wp-content/uploads/2011/01/sort.data_.frame_.r

sort.data.frame <- function(x, by){
    # Author: Kevin Wright
    # with some ideas from Andy Liaw
    # http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
 
    # x: A data.frame
    # by: A one-sided formula using + for ascending and - for descending
    #     Sorting is left to right in the formula
  
    # Useage is:
    # library(nlme);
    # data(Oats)
    # sort(Oats, by= ~nitro-Variety)
 
    if(by[[1]] != "~")
        stop("Argument 'by' must be a one-sided formula.")
 
    # Make the formula into character and remove spaces
    formc <- as.character(by[2]) 
    formc <- gsub(" ", "", formc) 
    # If the first character is not + or -, add +
    if(!is.element(substring(formc, 1, 1), c("+", "-")))
        formc <- paste("+", formc, sep = "")
 
    # Extract the variables from the formula
    vars <- unlist(strsplit(formc, "[\\+\\-]"))    
    vars <- vars[vars != ""] # Remove any extra "" terms
 
    # Build a list of arguments to pass to "order" function
    calllist <- list()
    pos <- 1 # Position of + or -
    for(i in 1:length(vars)){
        varsign <- substring(formc, pos, pos)
        pos <- pos + 1 + nchar(vars[i])
        if(is.factor(x[, vars[i]])){
            if(varsign == "-") {
                calllist[[i]] <- -rank(x[, vars[i]])
            } else {
                calllist[[i]] <- rank(x[, vars[i]])
            }
        } else {
            if(varsign == "-") {
                calllist[[i]] <- -x[, vars[i]]
            } else {
                calllist[[i]] <- x[,vars[i]]
            }
        }
    }
    return(x[do.call("order", calllist), ])
}

#adapted from reshape
sort_df<-function (data, vars = names(data), decreasing=T) 
{
    if (length(vars) == 0 || is.null(vars)) 
        return(data)
    data[do.call("order", c(data[, vars, drop = FALSE], decreasing = decreasing)), , drop = FALSE]
}