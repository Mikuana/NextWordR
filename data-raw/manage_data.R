library(magrittr)

# Made available by SwiftKey to students in the Johns Hopkins Data Science Specialization on
raw_data_url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'

# Where raw data is stored for raw data management. Assumes source is called from package root
raw_data_dir = file.path('data-raw')

# md5 check sums for raw data files
raw_data_md5 = readRDS(file.path(raw_data_dir, 'raw_data_md5.rds'))

# Manage Data
#
# Check raw data, compare it to md5 sums, and download it if necessary. Management of the raw data
# through functions helps ensure that the processed dictionary of ngrams that is built using this
# data can be easily regenerated, without needed to pass around large intermediate data sets.

rebuild = FALSE  # this controls whether the function attempts to rebuild the raw data

# build a table which contains attributes about the expected files
df = data.frame(
  filename = names(raw_data_md5),
  raw_data_dir = raw_data_dir,
  md5 = raw_data_md5
) %>%
  dplyr::mutate_(
    filepath = 'file.path(raw_data_dir, filename)',
    exists = 'file.exists(filepath)'  # check to see if each file exists
  )

if(all(df$exists)) {  # if all files exist, check them against their md5 sums
  df = dplyr::mutate_(
    df, matches = 'unname(tools::md5sum(filepath)) == md5'
  )
  if(!all(df$matches)) { rebuild = TRUE }  # if any file fails md5 check, rebuild

} else { rebuild = TRUE }  # if any files are missing, rebuild

if(rebuild) {
  print("Something is wrong with the raw-data. Attempting to rebuild")
  zip_path =  base::file.path(raw_data_dir, 'Coursera-SwiftKey.zip')

  if(!file.exists(zip_path)) {
    print("zip doesn't exist. Downloading zip file. Go get some coffee.")
    utils::download.file(raw_data_url, zip_path)
  }

  utils::unzip(zip_path, exdir=raw_data_dir)  # unzip raw data
  file.rename(  # move english files from subfolders up to top raw data folder
    file.path(raw_data_dir, 'final', 'en_US', names(raw_data_md5)),
    file.path(raw_data_dir, names(raw_data_md5))
  )
  unlink(file.path(raw_data_dir, 'final'), recursive = TRUE)  # remove subfolders from unzip

  print("Raw data extracted. Run this function again to check md5 sums.")
} else {
  print("Raw data md5 sums match expected.")
}
