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
#' Add JS html to the end of body
#'
#' @param jsfile A character. The file path of your html js file
#' @param path A character. The path to your latest modified html file
#'
#' @return
#' @export
#'
#' @examples none
addjs <- function(jsfile, path="."){
  jsHtml <- readLines(
    jsfile
  )
  list.files(path=path,pattern=".html$", full.names = T) -> allHtmls
  file.info(allHtmls) -> allHtmlsInfo
  with(allHtmlsInfo, which(mtime==max(mtime)))-> loc_theLatest
  newHtml <- allHtmls[[loc_theLatest]]
  readLines(newHtml) -> newHtmlLines
  stringr::str_which(newHtmlLines) -> loc_bodyEnd
  revisedHtml <- c(newHtmlLines[1:(loc_bodyEnd-1)],
  jsHtml,
  newHtml[-(1:(loc_bodyEnd-1))])
  writeLines(
    revisedHtml,
    con=newHtml
  )
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
  service$addjs <- addjs
  service
}
