read.xlsx.mult <- function(file,sheet="ALL",encoding="UTF-8"){
  library(xlsx);library(dplyr)
  if(sheet=="ALL"){
    getSheets(loadWorkbook(file)) %>%
      names() -> name
  } else {
    sheet -> name
  }
  list() -> a
  for (i in seq_along(name)) {
    read.xlsx(file,sheetName=name[[i]],encoding=encoding) -> a[[i]]
  }
  names(a) <- name
  a
}
