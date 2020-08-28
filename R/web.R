#' Create a jquery experiemtal index page with body content from the copy of
#'  a webpage inspection element
#'
#' @return
#' @export
#'
#' @examples
create_jqueryPage <- function(){
  clipr::read_clip() -> context
  stringr::str_which(index,"\\{replacementBlock\\}") -> loc_cut
  newContent <-
    c(
      index[1:(loc_cut-1)],
      context,
      index[-(1:loc_cut)]
    )
  writeLines(newContent,
             con="index.html")
  servr::httd(port="8080")-> out
  with(out, paste0("http://", host,":",port)) -> url0
  browseURL(url0)
  file.edit("index.html")
}

#' serve the last modified html (usually the latest knitted Rmd)
#'
#' @param path A character, default="."
#'
#' @return
#' @export
#'
#' @examples none
serveTheLastModified <- function(path="."){
  rootPath <- normalizePath(".")
  if(length(servr::daemon_list())==0){
    servr::httd()
  }
  servr::server_config() -> sconfig
  sconfig$host -> host
  sconfig$port -> port
  list.files(path=path,pattern=".html$", full.names = T) -> allHtmls
  file.info(allHtmls) -> allHtmlsInfo
  with(allHtmlsInfo, which(mtime==max(mtime)))-> loc_theLatest
  stringr::str_remove(allHtmls[[loc_theLatest]],
                      glue::glue("^(.|{rootPath})")) -> html2open
  with(out, paste0("http://", host,":",port,html2open)) -> url0
  browseURL(url0)
}

#' Create web service
#'
#' @return
#' @export
#'
#' @examples none
webService <- function(){
  service <- new.env(parent=globalenv())
  service$serveTheLastModified <- serveTheLastModified
  service$create_jqueryPage <- create_jqueryPage
  service$browse_last <- servr::browse_last
  service
}
