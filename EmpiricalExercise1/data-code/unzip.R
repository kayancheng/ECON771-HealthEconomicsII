#' Unzip zipped folders: HCRIS_v1996/HCRIS_v2010/pos-data

data_in.path <- "EmpiricalExercise1/data/data-in/"
out_data.path <- "EmpiricalExercise1/data/data-out/"
unzip_out_data.path <- paste0(out_data.path, "/unzip_raw/")

## Function to unzip files
unzip_fn <- function(folder_name){
  data_in.path.zip <- paste0(data_in.path,folder_name, ".zip")
  unzip(data_in.path.zip, exdir=unzip_out_data.path)
}

## List of zip files
zip_list <- c("HCRIS_v1996", "HCRIS_v2010", "pos-data")

## Unzip the files if not done yet
if (length(list.files(unzip_out_data.path)) == 0){
  for (i in zip_list){
    unzip_fn(i)
  }
}







