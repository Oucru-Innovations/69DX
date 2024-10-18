library(tidytable)

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_dir <- '../data'

birthdata <-
  readxl::read_excel(file.path(data_dir, 'raw', 'BirthDataFix.xlsx'), guess_max = 4120) |>
  as_tidytable() |>
  mutate(across(where(is.character), stringi::stri_trans_general, id="Latin-ASCII")) |>
  rename_with(tolower) |>
  rename_with(stringi::stri_trans_general, id="Latin-ASCII") |>
  rename_with(gsub, pattern='(\\s)|(\\n)|(\\r)', replacement='_')

.main_data <- file.path(data_dir, 'raw', '69DX.xlsx')
.sheet_list <- readxl::excel_sheets(.main_data)

for (sheet in .sheet_list){
  assign(
    tolower(sheet),
    readxl::read_excel(.main_data, sheet=sheet, guess_max = 4120) |>
      as_tidytable() |>
      mutate(across(where(is.character), stringi::stri_trans_general, id="Latin-ASCII")) |>
      rename_with(tolower) |>
      rename_with(stringi::stri_trans_general, id="Latin-ASCII") |>
      rename_with(gsub, pattern='(\\s)|(\\n)|(\\r)', replacement='_')
  )
}

save(list=ls(), file=file.path(data_dir, 'imported', 'dx69_imported_data.Rdata'))
