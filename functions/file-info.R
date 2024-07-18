################################################################
### File info in R
### 
### Jonathan Whiteley       R v4.2.3        2024-07-17
################################################################

# https://stackoverflow.com/questions/43349064/find-file-owner-in-r
# https://superuser.com/questions/898017/windows-command-to-get-all-information-properties-of-a-file
# https://superuser.com/questions/363278/is-there-a-way-to-get-file-metadata-from-the-command-line
# - wmic is not compatible with UNC paths, and requires backslashes to be escaped
#   (in addition to escaping within R)
# - wmic might also be deprecated, replaced by PowerShell
# https://stackoverflow.com/questions/30744901/how-to-run-powershell-command-in-r
# https://shellgeek.com/get-the-file-owner-using-powershell/
# https://shellgeek.com/powershell-get-file-attributes/
# https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-itemproperty?view=powershell-7.4

if (F) {
  # defaults to working directory
  system('powershell -command "Get-Acl"', intern = TRUE)
  system('powershell -command "Get-Acl -Path \"~\""', intern = TRUE)
  system('powershell -command "Get-Acl -Path \"~\" | Format-List"', intern = TRUE)
  system('powershell -command "Get-Item -Path \"~\" | Format-List -Property * -Force"', intern = TRUE)
  
  # Spaces in the path
  test_path <- normalizePath("~/My Pictures")  # this does not actually exist, as far as PowerShell is concerned - it is some sort of shortcut.
  test_path <- tempfile("test file ", fileext = ".txt") |> normalizePath()
  file.create(test_path)
  system(paste0('powershell -command "Get-Acl -Path \"', test_path, '\" | Format-List"'), intern = TRUE) # error
  system(paste0('powershell -command "Get-Acl -Path \'', test_path, '\' | Format-List"'), intern = TRUE)
  system(paste0('powershell -command "Get-Item -Path \"', test_path, '\" | Format-List -Property * -Force"'), intern = TRUE) # error
  system(paste0('powershell -command "Get-Item -Path \'', test_path, '\' | Format-List -Property * -Force"'), intern = TRUE)
  result_status1 <- 
    system(paste0('powershell -command "Get-Acl -Path \"', test_path, '\" | Format-List"'), intern = TRUE) # error
  result_status0 <- 
    system(paste0('powershell -command "Get-Acl -Path \'', test_path, '\' | Format-List"'), intern = TRUE)
  attr(result_status1, "status")  # 1
  attr(result_status0, "status")  # NULL
}

# Goal: write a function that combines results of `file.info()` and PowerShell
#  commands on Windows to provide extensive file information, including
#  - creation date
#  - owner (user who created the file)
#  - last modification date
#  - user who made last modification
#    - "Last Saved By" is application-specific (e.g., Office files)
#  - last access date (included in output from file.info())
#  for a list (vector) of files (i.e., output of dir())
# Useful for file management, migration, cleanup, etc.

library(tidyverse)

powershell_list2df <- function (txt, width = NA) {
  # helper function to parse output of "Get-Acl" PowerShell command
  # Assume "Format-List" for now (more detail)
  require(tidyverse)
  # check if the result has a status of 1 and skip if it does
  if (!is.null(attr(txt, "status")) && attr(txt, "status") > 0)
    return()
  # remove empty lines, collapse into a single string for read_fwf()
  txt_clean <- txt[which(!txt == "")] |>
    paste(collapse = "\n")
  # derive width of property names, if not specified
  if (is.null(width) || is.na(width)) {
    wfreq <- regexpr(":", txt) %>% table() %>% 
      as_tibble(.name_repair = "universal")
    # get the most common width (match index) that is greater than 0
    width <- wfreq %>% 
      filter(as.numeric(.) > 0) %>% 
      filter(n == max(n)) %>% 
      pull((.)) %>% 
      as.numeric()
  }
  # read text as fixed-width table
  txt_df <- read_fwf(
    txt_clean, 
    col_positions = fwf_widths( c(width, NA), col_names = c("property", "value") ),
    show_col_types = FALSE
  ) %>% 
    mutate(
      # strip trailing spaces and ":"
      property = str_replace(property, "\\s+\\:\\s*$", ""), #%>% tolower(),
      # remove extranous path information added by PowerShell
      value = str_replace(value, "Microsoft\\.PowerShell\\.Core\\\\FileSystem::", "")
    ) %>% 
    fill(property, .direction = "down") %>% 
    summarize(value = paste(value, collapse = "\n"), .by = property)
  # pivot to a data frame with properties as columns & return result
  pivot_wider(txt_df, names_from = property, values_from = value) %>% return()
}

file_info_win <- function (files) {
  require(tidyverse)
  # Assume the main argument is a *vector of file paths*
  # start with file.info()
  finfo <- base::file.info(files) %>% 
    # convert row names to regular column
    rownames_to_column("path")
  # Get file owner information
  # Might be faster if I could pass a list of paths directly to PowerShell ('ForEach-Object{...}')
  facl <- lapply(
    files,
    function (path) system(
      paste0('powershell -command "Get-Acl -Path \'', path, '\' | Format-List"'), 
      intern = TRUE)
  )
  fowner <- lapply(facl, powershell_list2df) %>% 
    bind_rows()
  # ensure the object is not empty, for joining later.
  if (nrow(fowner) < 1)
    fowner <- finfo["path"]
  # Get additional file properties from PowerShell
  fitems <- lapply(
    files,
    function (path) system(
      paste0('powershell -command "Get-Item -Path \'', path, '\' | Format-List -Property * -Force"'), 
      intern = TRUE)
  )
  fprops <- lapply(fitems, powershell_list2df) %>% 
    bind_rows()
  # ensure the object is not empty, for joining later.
  if (nrow(fprops) < 1)
    fprops <- finfo["path"]
  # combine results, joining on the *normalized* path of each file.
  # browser()
  file_props <- 
    finfo %>% 
    mutate(path = normalizePath(path)) %>% 
    left_join(
      fowner %>% rename(path = any_of("Path")) %>% 
        mutate(path = normalizePath(path)),
      by = "path"
    ) %>% 
    left_join(
      fprops %>% rename(path = any_of("PSPath")) %>% 
        mutate(path = normalizePath(path)),
      by = "path"
    ) 
  return(file_props)
}



# test code
library(testthat)

test_that("Returned data frame contains information in file.info()", {
  testfile <- tempfile("test", fileext = ".txt")
  cat("test file\n", Sys.time(), file = testfile)
  file_info_base <- file.info(testfile)
  file_info_test <- file_info_win(testfile)
  rownames(file_info_base) <- NULL
  expect_equal(
    file_info_base, 
    file_info_test[, names(file_info_base)]
  )
})

test_that("No errors for a list of files", {
  testfile_list <- c(
    tempfile("test", fileext = ".txt"),
    tempfile("test2", fileext = ".txt")
  )
  file.create(testfile_list)
  expect_no_error( file_info_win(testfile_list) )
})

test_that("No errors for paths with spaces", {
  testfile <- tempfile("test file ", fileext = ".txt")
  file.create(testfile)
  expect_no_error( suppressWarnings(file_info_win(testfile)) )
})

test_that("No errors for paths that don't exist", {
  testfile <- tempfile("test", fileext = ".txt")
  expect_no_error( suppressWarnings(file_info_win(testfile)) )
})


if (F) {
  # lots of warnings related to PowerShell errors 
  # and different results for items (directories) that don't actually exist (e.g., "My Pictures")
  file_info_home <- file_info_win(dir("~"))
}
