

# Purpose: to compare ICF modified output with original EPA gold standard output limited to 'ATL-CMAQ for Ambient1.csv' and 'ATL-CMAQ for Ambient2.csv'


library(readr)
library(openxlsx)


output_folder <- 'Output/' # relative to SIACS wd
epa_output_folder <- 'EPA_Output/' # added to repo
files <- c('ATL-CMAQ for Ambient1.csv', 'ATL-CMAQ for Ambient2.csv') # hard coded filenames. will need ot change if other simulations are being compared.

compare_files <- function(output_file, epa_file) { # Function to compare dataframes and calculate percentage difference
  df_output <- read_csv(paste0(output_folder, output_file), skip = 5)#,header=FALSE) # first row will assign to header so need to skip next 5
  df_epa <- read_csv(paste0(epa_output_folder, epa_file), skip = 5)#,header=FALSE)# # first row will assign to header so need to skip next 5
  
  names(df_output) <- df_output[1, ]   # col names in 7th reow
  names(df_epa) <- df_epa[1, ]

  df_output <- df_output[-1, ]  # kill first row (7th in orig) because col names
  df_epa <- df_epa[-1, ]  # kill first row (7th in orig) because col names

  
  df_output <- as.data.frame(lapply(df_output, function(x) as.numeric(as.character(x)))) # convert to numeric if character
  df_epa <- as.data.frame(lapply(df_epa, function(x) as.numeric(as.character(x))))
  
  per_diff <- (df_output - df_epa) / df_epa * 100 # per diff but watch otu for div zero
  abs_diff <- abs(df_output - df_epa) # abs diff
  names(per_diff) <- names(df_output)  # same column names
  names(abs_diff) <- names(df_output)  # same column names
  
    oplist=list(per_diff=per_diff,abs_diff=abs_diff)
  return(oplist)
}

# write to xlsx
wb <- createWorkbook()

for (file in files) {
  oplist <- compare_files(file, file)
  per_diff=oplist$per_diff; abs_diff=oplist$abs_diff
  sheet_name <- paste0(substr(file, 1, nchar(file) - 4),'_perdiff') # original minus .csv + pdiff
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, per_diff, colNames = TRUE)
  sheet_name <- paste0(substr(file, 1, nchar(file) - 4),'_absdiff') # original minus .csv + abs diff
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, abs_diff, colNames = TRUE)
  }


saveWorkbook(wb, 'Comparison_to_Gold_Standard.xlsx', overwrite = TRUE) # will write to cwd (SIACS) and not output folder

print("Comparison completed and saved to Comparison_to_Gold_Standard.xlsx") 