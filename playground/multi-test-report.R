tmpfile <- tempfile("report", fileext = ".txt")

# Assemble example vector of results (T/F) and names (e.g., file names)
ex_names <- paste("test", 1:9, sep = "-")
ex_results <- rep_len(T, length(ex_names))
ex_results[ceiling(runif(1)*length(ex_results))] <- F
names(ex_results) <- ex_names

# Second set of results
ex_results2 <- rep_len(T, length(ex_names))
ex_results2[ceiling(runif(1)*length(ex_results))] <- NA
names(ex_results2) <- ex_names

# Function to convert named logical vector to table of PASS/FAIL
results2df <- function (
    x, 
    colnames = c("Test", "Outcome"), 
    t = "PASS", f = "**FAIL**", na = "--"
) {
  x_names <- names(x)
  if (is.null(x_names))
    x_names <- 1:length(x)
  out <- data.frame(names(x), as.logical(x))
  names(out) <- colnames
  out[, 2] <- ifelse(x, t, f)
  out[is.na(x), 2] <- na
  return(out)
}

results2df(ex_results)
results2df(ex_results) |> knitr::kable()
results2df(ex_results2) |> knitr::kable()
results2df(ex_results2, c("File", "Outcome"))
