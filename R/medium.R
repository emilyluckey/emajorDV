#' Execute HTTP verb based on "VERB resource-path" expression
#'
#' @param HTTPphrase A character of "VERB resource-path" expression
#' @param query A list of query parameters
#'
#' @return A list
#' @export
#'
#' @examples
#' HTTPphrase="GET https://api.medium.com/v1/me"
#' query <- list(
#'   accessToken=token
#' )
#' exec_HTTP(HTTPphrase, query=query)
exec_HTTP <- function(HTTPphrase,query=NULL){
  result <- disect_HTTPphrase(HTTPphrase)
  verb <- as.symbol(result$HTTPverb)

  require(httr)
  rlang::expr(
    (!!verb)(
      url=result$url,
      header=httr::add_headers(
        `Content-Type`="application/json"
      ),
      query=query
    )
  ) -> actionExpr

  rlang::eval_tidy(actionExpr) -> response
  httr::content(response)
}




# helpers -----------------------------------------------------------------

disect_HTTPphrase <- function(HTTPphrase){
  HTTPverb=stringr::str_extract(HTTPphrase,
                                "^[A-Z]+(?=\\shttp)")
  endpoint=stringr::str_extract(HTTPphrase,
                                glue::glue("(?<={HTTPverb}\\s)[:graph:]+$"))
  url=ifelse(stringr::str_detect(endpoint,"http"),
             endpoint,
             file.path(
               stringr::str_remove(baseurl,"/$"),
               stringr::str_remove(endpoint,"^/")))
  result <- list(
    HTTPverb=HTTPverb,
    url=url
  )
  result
}

