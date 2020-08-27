#' Create snapshot from SVG
#'
#' @param svgfilename A character. file path of the svg file
#' @param width A character. default="100" px
#' @param destfolder A character. default="./snapshot"
#'
#' @return
#' @export
#'
#' @examples none
create_snapshot <- function(svgfilename, width="100", destfolder="./snapshot"){
  oneSVG <- magick::image_read_svg(svgfilename)
  # magick::image_info(oneSVG) -> metaSVG
  # asratio <- with(metaSVG, width/height)
  magick::image_scale(
    oneSVG,
    width
  ) -> smallSVG
  saveFilename <-
    basename(svgfilename) %>%
    stringr::str_replace(".svg$",".png")
  magick::image_write(
    smallSVG, path=file.path(destfolder,saveFilename),
    format="png"
  )
  message(file.path(destfolder,saveFilename))
}
#' Launch Boxy SVG and open filename inside
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples none
svgOpen <- function(filename){
  system(glue::glue("open -a 'Boxy SVG' {filename}"))
}

# svgfile <-
#   "/Users/martin/Github/course-HS-society-and-citizen/img/gg_GDPRank_base3.svg"

#' insert SVG as an object html
#'
#' @param svgfile
#'
#' @return
#' @export
#'
#' @examples none
insertSVG <- function(svgfile){
  htmltools::withTags(
    object(
      type="image/svg+xml",
      data=svgfile))
}
