################################################################
### File info in R
### 
### Jonathan Whiteley       R v4.2.3        2024-07-17
################################################################

# https://stackoverflow.com/questions/43349064/find-file-owner-in-r
# https://superuser.com/questions/898017/windows-command-to-get-all-information-properties-of-a-file
# https://superuser.com/questions/363278/is-there-a-way-to-get-file-metadata-from-the-command-line
# - wmic might be deprecated, replaced by PowerShell
# https://stackoverflow.com/questions/30744901/how-to-run-powershell-command-in-r
# https://shellgeek.com/get-the-file-owner-using-powershell/
# https://shellgeek.com/powershell-get-file-attributes/
# https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-itemproperty?view=powershell-7.4

# defaults to working directory
system('powershell -command "Get-Acl"', intern = TRUE)
system('powershell -command "Get-Acl -Path \"~\""', intern = TRUE)
system('powershell -command "Get-Item -Path \"~\" | Format-List -Property * -Force"', intern = TRUE)

# Goal: write a function that combines results of `file.info()` and PowerShell
#  commands on Windows to provide extensive file information, including
#  - creation date
#  - owner (user who created the file)
#  - last modification date
#  - user who made last modification
#  - last access date (included in output from file.info())
#  for a list (vector) of files (i.e., output of dir())
# Useful for file management, migration, cleanup, etc.
