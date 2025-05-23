################################################################################
# Mock-up of multiple related test results and reporting to a log file
# e.g., validation tests on a list of files; need record of tests and results
# Jonathan Whiteley                2025-04-21                R v4.5.0
################################################################################

# Generate example results ####

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
    colnames = c("Name", "Outcome"), 
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

# OUTPUT to log file ####
library(logr)
tmpfile <- tempfile("report", fileext = ".txt")
if (F)
  file.exists(tmpfile)

# Open the log file ====
log_open(tmpfile, logdir = FALSE)

# Add contents to log file ====
log_info("Results: first example", hide_notes = TRUE)
results2df(ex_results) |> knitr::kable() |> put()
log_info("Results: second example", hide_notes = TRUE)
results2df(ex_results2, c("File", "Outcome")) |> put()

sep("CODE")
log_code()    # will include all code, at this spot in the log file (e.g., at the end) 

# CLEAN UP ====
log_close()
if (F) # open folder with temp file
  system(sprintf("open %s", dirname(tmpfile)))
