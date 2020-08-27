library(httr)
library(httpuv)
#' Github authentication
#'
#' @return
#' @export
#'
#' @examples
github_auth <- function(){
  httr::oauth2.0_token(
    httr::oauth_endpoints("github"),
    httr::oauth_app(
      "emajorDV",
      key="Iv1.644f772be9cc2cb8",
      secret = "e9a4c17e605ba758d81aa0d8f5a14bb37b3b80d4"
    )
  ) -> github.token
  github.token
}

#' Get all github projects
#'
#' @param github.token A list, returned from auth
#'
#' @return
#' @export
#'
#' @examples none
github_getProjects <- function(github.token){
  GET(
    url="https://api.github.com/orgs/emajortaiwan/projects",
    add_headers(
      Accept="application/vnd.github.inertia-preview+json"
    ),
    config = config(
      token=github.token
    )
  ) -> responses

  content(responses) -> response.content

  response.content
}

#' Create github service
#'
#' @return
#' @export
#'
#' @examples none
githubService <- function(){
  new.env(parent = globalenv()) -> service
  service$auth <- github_auth
  service$getProjects <- github_getProjects
  service
}
