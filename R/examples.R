#' generate example Rmd files
#'
#' @param exampleName A character.
#'
#' @return
#' @export
#'
#' @examples none
emajor_example <- function(exampleName){
  # exampleName="emajor"
  exampleObj <- rlang::sym(exampleName)
  todo <- rlang::expr({writeLines(
    !!exampleObj,
    file.path(getwd(),paste0(exampleName,".Rmd"))
  )})
  rlang::eval_tidy(
    todo
  )
  message(
    file.path(getwd(),paste0(exampleName,".Rmd")),
    " is generated."
  )

}
