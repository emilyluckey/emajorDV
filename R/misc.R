#' hide hidden files in mac
#'
#' @return
#' @export
#'
#' @examples none
hideFiles_mac <- function(){
  system("defaults write com.apple.finder AppleShowAllFiles false; killall Finder")
}
#' show hidden files in mac
#'
#' @return
#' @export
#'
#' @examples none
showFiles_mac <- function(){
  system("defaults write com.apple.finder AppleShowAllFiles true; killall Finder")
}

#' generate a mac system service
#'
#' @return
#' @export
#'
#' @examples none
macService <- function(){
  service = rlang::new_environment(parent=globalenv())
  service$showFiles = showFiles_mac
  service$hideFiles = hideFiles_mac

  return(service)
}


#' Add object to internal package data
#'
#' @param dataObj An object
#'
#' @return
#' @export
#'
#' @examples none
addInternalData <- function(dataObj){
  dataObj <- rlang::enquo(dataObj)
  dataName <- rlang::as_name(dataObj)
  load("R/sysdata.rda") -> existingObjects
  todo0 <- rlang::expr(!!dataObj)
  assign(rlang::as_name(dataObj),rlang::eval_tidy(todo0))
  allObjectnames <- unique(
    c(existingObjects,
      dataName))
  allObjects <- rlang::syms(allObjectnames)
  todo <-
    rlang::expr({
      usethis::use_data(!!!allObjects, internal=T, overwrite = T)
    })
  rlang::eval_tidy(todo)
}

#' Browse EMajor homepage
#'
#' @return
#' @export
#'
#' @examples emajorhome()
emajorhome <- function(){
  browseURL("https://emajortaiwan.github.io/home/")
}
