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

