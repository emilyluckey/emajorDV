#' Create a lesson template
#'
#' @description Use this to create a list object with each element named for the detail of a lesson that can be used to fill in so to be used as an input for add_lesson() function
#' @return A list with names
#' @export
#'
#' @examples newlesson <- create_lessonTemplate()
create_lessonTemplate <- function(){
  lessonTemplate
}

#' Add a new lesson
#'
#' @param newlesson A list create by filling up lessonTemplate
#'
#' @return
#' @export
#'
#' @examples none
add_lesson <- function(newlesson){
  # courseList ={ # obtain course list
  #   jsonlite::fromJSON(
  #     readLines("./course_info/courseList.json")
  #   )
  # }
  httr::GET("https://emajortaiwan.github.io/home/course_info/courseList.json")-> response
  httr::content(response) -> courseList
  newlessonComplete = { # create new lesson
    newlessonComplete <- lessonTemplate
    newlessonComplete[names(newlesson)] <- newlesson
    newlessonComplete
  }

  updateCourseList ={ # update course list
    length(courseList) -> tot
    courseList[[tot+1]] <- newlessonComplete
    jsonlite::toJSON(courseList,
                     auto_unbox = T) -> jsonOut
    writeLines(jsonOut,con="courseList.json")
    courseList
  }
  invisible(updateCourseList)
}

# lesson2 <- create_lessonTemplate()
# lesson2$topic="Knit and Twist: How to Inject Your JS in Rmd Properly"
# lesson2$date=lubridate::today()
#
# add_lesson(lesson2) -> updateLessons

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
#' @param openUrl. A logical. T= open course URL if there is any. (defaul=T)
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
get_courseMaterial <- function(number=NULL, date=NULL, destfolder=getwd(), openUrl=T){
  library(httr)
  GET(
    url="https://emajortaiwan.github.io/home/course_info/courseList.json"
  ) -> response
  content(response) -> response_content

  if(is.null(number)){
    learningMaterial %>%
      filter(
        date==lubridate::ymd(date)
      ) -> courseSelected
  } else {
    learningMaterial[number, ] -> courseSelected
  }

  dataToFile(courseSelected, destfolder, openUrl)
}

browseCourseUrl <- function(number=NULL, date=NULL){
  if(is.null(number)){
    learningMaterial %>%
      filter(
        date==lubridate::ymd(date)
      )-> courseSelected
  } else {
    learningMaterial[number, ]-> courseSelected
  }

  browseURL(courseSelected$url[[1]])
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
  service$browseCourseUrl <- browseCourseUrl
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


# courseSelected <- get_courseMaterial(1)
# destfolder=getwd()
dataToFile = function(courseSelected, destfolder, openUrl){
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
    if(courseSelected$url[[.x]] !="" && openUrl){
      browseURL(courseSelected$url[[.x]])
    }

  }

}
