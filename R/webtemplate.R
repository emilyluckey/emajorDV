
#' Create a webtemplate service
#'
#' @return
#' @export
#'
#' @examples wt <- webtemplateService()
webtemplateService <- function(){
  service <- list()
  service$materializedFramework <- create_materializedFramework

  service
}

#' Create a Materialized Web Framework
#'
#' @param exdir A character (default=".") specifies the directory where downloaded files should be placed.
#' @return none. But will download and unzip to recover the framework
#' @export
#'
#' @examples none
create_materializedFramework <- function(exdir="."){
  downloadUrl <- "https://www.dropbox.com/s/oz385kxex63yoa6/MaterializedFramework.zip?dl=1"

  download_unzip(downloadUrl, exdir)
}


# helpers -----------------------------------------------------------------

download_unzip <- function(downloadUrl,exdir="."){
  filename <- stringr::str_remove(basename(downloadUrl),"\\?[:graph:]+$")
  rootPath <- normalizePath(exdir)
  filename <- file.path(rootPath, filename)
  download.file(
    url=downloadUrl,
    destfile=filename
  )

  unzip(
    zipfile=filename
  )

  unlink(filename)

}
