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
