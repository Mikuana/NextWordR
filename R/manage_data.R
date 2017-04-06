#' Manage Data
#'
#' Check raw data, compare it to md5 sums, and download if necessary.
#' TODO: add in collapse of data folder to put  all english texts at top of data
#' TODO: fix paths. It's all screwed up due to conversion to a package.
#'
#' @export
manage_data = function() {
  enus_path = base::file.path('data', 'en_US')

  if(!base::dir.exists(enus_path)) {

    zip_path =  base::file.path('Coursera-SwiftKey.zip')

    if(!base::file.exists(zip_path)) {
      cat("en_US folder and zip don't exist. Downloading zip file. Go get some coffee.")
      utils::download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip')
    }

    utils::unzip(zip_path, exdir='data')
  }

  # Initial md5 sums calculated and saved with the project using
  # data_md5 = sapply(base::list.files(enus_path, full.names=TRUE), function(x) tools::md5sum(x) )
  # saveRDS(data_md5, 'data_md5.Rds')

  md5check = sapply(
    base::list.files(enus_path, full.names=TRUE),
    function(x) tools::md5sum(x)
  ) == readRDS('data_md5.Rds')

  if(md5check %>% base::unname(.) %>% base::all(.)) {
    cat("Data are downloaded, unpacked, and match md5 sums in this project.")
  } else {
    stop("Something went wrong. The md5 sums don't match those commited with this project.")
  }
}
