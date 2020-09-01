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

  github.token = {
    httr::oauth2.0_token(
      httr::oauth_endpoints("github"),
      httr::oauth_app(
        "EMajor courses",
        key="0ece2f021066b9583c8d",
        secret = "b8e0700fe7f7ca262693fb77e1e2a2059016b3ac"
      ),
      scope="repo"
    ) -> github.token
    github.token
  }

  endpoint <- "https://api.github.com"
  headerExpr <- rlang::expr({
    httr::add_headers(
      Accept="application/vnd.github.v3+json"
    )
  })

  owner <- "emajortaiwan"
  repo <- "home"

  fileSha = {
    path <- "course_info"
    resource <- "/repos/{owner}/{repo}/contents/{path}"
    httr::GET(
      url=file.path(endpoint, glue::glue(resource), fsep=""),
      rlang::eval_tidy(headerExpr),
      config=httr::config(
        token=github.token
      )
    ) -> course_info
    httr::content(course_info) -> course_info
    purrr::keep(course_info, ~{.x$name=="courseList.json"}) -> courseListJson
    courseListJson[[1]]$sha -> fileSha
    fileSha
  }


  courseList = {
    httr::GET("https://emajortaiwan.github.io/home/course_info/courseList.json")-> response
    httr::content(response) -> courseList
    courseList
  }

  newlessonComplete = { # create new lesson
    newlessonComplete <- lessonTemplate
    newlessonComplete[names(newlesson)] <- newlesson
    newlessonComplete$id=ids::random_id()
    newlessonComplete
  }

  courseJson ={ # update course list
    length(courseList) -> tot
    courseList[[tot+1]] <- newlessonComplete
    jsonlite::toJSON(courseList,
                     auto_unbox = T) -> jsonOut
    jsonOut
    # writeLines(jsonOut,con="courseList.json")
    # courseList
  }


  githubUpdate = {
    path <- "course_info/courseList.json"
    resource <- "/repos/{owner}/{repo}/contents/{path}"
    httr::PUT(
      url=file.path(endpoint, glue::glue(resource), fsep=""),
      rlang::eval_tidy(headerExpr),
      config=httr::config(
        token=github.token
      ),
      body = list(
        message=newlesson$topic,
        sha= fileSha,
        content=openssl::base64_encode(jsonOut)
      ),
      encode="json"
    ) -> response
    httr::content(response)
  }
  invisible(githubUpdate)
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
  courseJson <- get_courseListJson()
  courseList <-
    purrr::map(
      courseJson,
      ~.x[c("topic","description","date")]
    )
  purrr::map_dfr(
    courseList,
    ~within(
      .x,
      {
        topic=ifelse(length(topic)==0,"",topic)
        description=ifelse(length(description)==0,"",description)
        date=ifelse(length(date)==0,"",date)
      }
    )
  ) -> df_courseList
  df_courseList
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
  httr::GET(
    url="https://emajortaiwan.github.io/home/course_info/courseList.json"
  ) -> response
  httr::content(response) -> response_content

  if(is.null(number)){
    purrr::keep(response_content, ~{.x$date==lubridate::ymd(date)}) ->
      courseSelected
  } else {
    response_content[number] -> courseSelected
  }

  dataToFile(courseSelected, destfolder, openUrl)
}

browseCourseUrl <- function(number=NULL, date=NULL){
  httr::GET(
    url="https://emajortaiwan.github.io/home/course_info/courseList.json"
  ) -> response
  httr::content(response) -> response_content

  if(is.null(number)){
    purrr::keep(response_content, ~{.x$date==lubridate::ymd(date)}) ->
      courseSelected
  } else {
    response_content[number] -> courseSelected
  }

  oneCourse <- courseSelected[[1]]
  if(oneCourse$onlineUrl !="" && openUrl){
    url0 <- oneCourse$onlineUrl
    sessionInfo() -> info
    if(stringr::str_detect(info$running,"[mM][aA][cC]")){
      system(glue::glue('open -a "Google Chrome" {url0}'))
    } else {
      browseURL(url0)
    }
  }
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
  service$create_lessonTemplate <- create_lessonTemplate
  add_lesson -> service$add_lesson
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
  oneCourse <- courseSelected[[1]]
  for(.x in seq_along(oneCourse$downloadUrl)){
    link <- oneCourse$downloadUrl[[.x]]$link
    filename <- URLdecode(stringr::str_extract(basename(link),"[:graph:]+(?=\\?)"))
    finalDestPath = file.path(destfolder,"course_material")
    dir.create(
      finalDestPath,
      recursive = T
    )
    filenamePath=file.path(finalDestPath,filename)

    download.file(
      link,
      filenamePath
    )
    if(oneCourse$downloadUrl[[.x]]$zip){
      unzip(
        filenamePath,
        exdir=finalDestPath
      )
    }
    unlink(
      filenamePath
    )
    message(
      "Course materials are in\n",
      finalDestPath
    )
    rstudioapi::selectFile(
      caption="Select a file to open:",
      path=finalDestPath) -> fileSelected
    file.edit(fileSelected)

    if(oneCourse$onlineUrl !="" && openUrl){
      url0 <- oneCourse$onlineUrl
      sessionInfo() -> info
      if(stringr::str_detect(info$running,"[mM][aA][cC]")){
        system(glue::glue('open -a "Google Chrome" {url0}'))
      } else {
        browseURL(url0)
      }
    }

  }

}

get_courseListJson <- function(){
    httr::GET("https://emajortaiwan.github.io/home/course_info/courseList.json")-> response
    httr::content(response) -> courseList
    courseList
  }

