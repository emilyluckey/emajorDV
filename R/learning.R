filename="/Users/martin/Github/civic_and_social_data_visualization/template/homeworkExhibition/homeworkexhibition.Rmd"

library(dplyr)
library(tidyr)
library(lubridate)
learningMaterial = tibble(
  filename=basename(filename),
  date=today(),
  topics="資料視覺展示平台：資料視覺化課程展示範例",
  filepath=filename
)

#' List available course materials
#'
#' @return
#' @export
#'
#' @examples none
list_courses <- function(){
  learningMaterial %>%
    select(
      -filepath
    )

}

#' Get course material based on list_courses
#'
#' @param number An integer. represents the row number from list_courses()
#' @param date A character in YYYY-MM-DD format represent the date of the course in list_courses()
#' @param destfolder A character, path to save the material, default=getwd()
#'
#' @return
#' @export
#'
#' @examples
#' list_course() -> available_courses
#' available_courses
#' get_courseMaterial(number=1)
#' get_courseMaterial(date="2020-08-26")
#' get_courseMaterial(nubmer=1, destfolder="/user")
#'
get_courseMaterial <- function(number=NULL, date=NULL, destfolder=getwd()){
  if(is.null(number)){
    learningMaterial %>%
      filter(
        date==lubridate::ymd(date)
      )
    return()
  } else {
    learningMaterial[number, ]
  }-> courseSelected

  dataToFile(courseSelected, destfolder)
}

#' generate EMajor learning service
#'
#' @return
#' @export
#'
#' @examples none
emajorService <- function(){
  service <- new.env(parent=globalenv())
  service$list_courses <- list_courses
  service$get_courseMaterial <- get_courseMaterial
  service
}

# helpers -----------------------------------------------------------------

textToData = function(filename){
  # readLines(filename) -> filelines
  dataName <-
    basename(filename) %>% #
    stringr::str_remove("\\.[:alnum:]+$")
  assign(dataName, readLines(filename))
  dataObj <- rlang::sym(dataName)
  load("R/sysdata.rda") -> existingObjects
  allObjectnames <- c(existingObjects,
                  dataName)
  allObjects <- rlang::syms(allObjectnames)
  todo <-
    rlang::expr({
      usethis::use_data(!!!allObjects, internal=T, overwrite = T)
    })
  rlang::eval_tidy(todo)
}

# courseSelected <- get_courseMaterial(1)
# destfolder=getwd()
dataToFile = function(courseSelected, destfolder){
  for(.x in seq_along(courseSelected$filename)){
    courseSelected$filename[[.x]] -> filename

    stringr::str_remove(filename,"\\.[:alnum:]+$") -> dataName
    dataObj <- rlang::sym(dataName)
    rlang::expr(
      {
        writeLines(
          !!dataObj,
          file.path(destfolder,filename)
        )
      }
    ) -> todo
    rlang::eval_tidy(
      todo
    )
    savedfile <- file.path(destfolder,filename)
    cat("Learning materials saved at \n",
        savedfile,"\n")
    file.edit(
      savedfile
    )

  }

}
