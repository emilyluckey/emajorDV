#' Theme for geom_col
#'
#' @return
#' @export
#'
#' @examples ggplot(data=myData)+geom_col(aes(x=x,y=y))+emajor_themeCol()
emajor_themeCol <- function(){
  theme(
    axis.text.x = element_text(size = 11),
    plot.background = element_rect(colour = "white"),
    plot.margin = margin(30,0,20,20), #margin?
    plot.title = element_text(size = 16, face = 2),
    plot.subtitle = element_text(size = 12, color = "#565b64"),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.2),
    panel.grid.major.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.length = unit(5,"pt"),
    axis.text.y = element_text(size = 13),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text.align = 1,
    legend.spacing.x = unit(0.3, "cm"),
    legend.margin=margin(b = -0.2, unit='cm'),
    legend.key = element_blank(),
    text = element_text(family = "NSerifC") #在grid 使用此字體會有error
  )
}

#' Theme for geom_bar
#'
#' @return
#' @export
#'
#' @examples ggplot(data=myData)+geom_bar(aes(x=x))+emajor_themeBar()
emajor_themeBar <- function(){
  theme(
    axis.text.x = element_text(hjust = -0.1, vjust = -0.5, size = 11),
    plot.background = element_rect(colour = "white"),
    plot.margin = margin(30,0,20,20), #margin?
    plot.title = element_text(size = 16, face = 2),
    plot.subtitle = element_text(size = 12, color = "#565b64"),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.2),
    panel.grid.major.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.length = unit(5,"pt"),
    axis.text.y = element_text(size = 13),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text.align = 1,
    legend.spacing.x = unit(0.3, "cm"),
    legend.margin=margin(b = -0.2, unit='cm'),
    legend.key = element_blank(),
    text = element_text(family = "NSerifC") #在grid 使用此字體會有error
  )
}

#' emajor_theme
#'
#' @param baseplot A list, 底圖(ggplot 物件)
#' @param bar_col An integer. 是否為geom_bar 或 geom_col物件，是則1;否則0
#'
#' @return
#' @export
#'
#' @examples none
emajor_theme <- function(baseplot,bar_col){
  deparse(substitute(baseplot)) -> name #原底圖的名稱
  paste0(name,"_m") -> name_m #新的名稱
  baseplot+theme(
    plot.background = element_rect(colour = "white"),
    plot.margin = margin(30,0,20,20), #margin?
    plot.title = element_text(size = 16, face = 2),
    plot.subtitle = element_text(size = 12, color = "#565b64"),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.2),
    panel.grid.major.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.length = unit(5,"pt"),
    axis.text.y = element_text(size = 13),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text.align = 1,
    legend.spacing.x = unit(0.3, "cm"),
    legend.margin=margin(b = -0.2, unit='cm'),
    legend.key = element_blank(),
    text = element_text(family = "NSerifC") #在grid 使用此字體會有error
  ) -> modified
  if (bar_col == 1){
    print(name_m)
    modified+theme(
      axis.text.x = element_text(size = 11)
    ) -> modified
    assign(name_m,modified,envir=.GlobalEnv) %>% print(.) #assign modified to the value of name_m
  }else{
    print(name_m)
    modified+theme(
      axis.text.x = element_text(hjust = -0.1, vjust = -0.5, size = 11)
    ) -> modified
    assign(name_m, modified, envir = .GlobalEnv)%>% print(.)
  }
}

#' 產生單格成品
#'
#' @param modifiedPlot A list of ggplot object modified by emajor_theme()
#' @param sources A character
#' @param png_filename A character
#'
#' @return
#' @export
#'
#' @examples none
emajor_grid_single <- function(modifiedPlot, sources, png_filename){
  ggplotGrob(modifiedPlot) -> grobPlot

  theme_tag <- rectGrob( #top left rect
    x = 0, y = 0.995, width = 0.02,just = c("left","top"),
    height = 0.05, gp = gpar(fill="#E74C3C", col = "red"))

  theme_line <-rectGrob( #top line
    x = 0, y = 0.995, width = 1,just = c("left","top"),
    height = 0.003, gp = gpar( fill = "#E74C3C", col = "red"))

  source_text <- textGrob( #bottom sources
    sources, x = 0.05,y=0.03 ,just = "left", #relative to x,y location
    gp = gpar(fontface = 3L, fontsize = 9, col="#7c8290")
  )

  emajor_text <- textGrob( #bottom-right emajor tag
    "E.Major", x = 0.95,y=0.03 ,
    gp = gpar(fontface = "bold", fontsize = 10, col="#E74C3C", alpha = 0.5, fontfamily = "NSerifC")
  )

  grid.newpage() #show
  grid.draw(modifiedPlot)
  grid.draw(theme_tag)
  grid.draw(theme_line)
  grid.draw(source_text)
  grid.draw(emajor_text)

  png(png_filename, width = 480, height = 480) #save to png
  grid.newpage()
  grid.draw(modifiedPlot)
  grid.draw(theme_tag)
  grid.draw(theme_line)
  grid.draw(source_text)
  grid.draw(emajor_text)
  dev.off()

}


#' 產生雙格成品
#'
#' @param modifiedPlot1 A list of ggplot object modified by emajor_theme()
#' @param modifiedPlot2 A list of ggplot object modified by emajor_theme()
#' @param sources A character
#' @param png_filename A character
#'
#' @return
#' @export
#'
#' @examples none
emajor_grid_composite <- function(modifiedPlot1,modifiedPlot2, sources, png_filename){
  #ggplot to Grob object
  ggplotGrob(modifiedPlot1) -> grobPlot1
  ggplotGrob(modifiedPlot2) -> grobPlot2

  #Grob element
  theme_tag <- rectGrob( #top left rect
    x = 0, y = 0.995, width = 0.02,just = c("left","top"),
    height = 0.05, gp = gpar(fill="#E74C3C", col = "red"))
  theme_line <-rectGrob( #top line
    x = 0, y = 0.995, width = 1,just = c("left","top"),
    height = 0.003, gp = gpar( fill = "#E74C3C", col = "red"))
  source_text <- textGrob( #bottom sources
    sources, x = 0.03,y=0.03 ,just = "left", #relative to x,y location
    gp = gpar(fontface = 3L, fontsize = 9, col="#7c8290")
  )
  emajor_text <- textGrob( #bottom-right emajor tag
    "E.Major", x = 0.95,y=0.03 ,
    gp = gpar(fontface = "bold", fontsize = 10, col="#E74C3C", alpha = 0.5, fontfamily = "NSerifC")
  )

  #show
  grid.newpage()
  grid.arrange(grobPlot1, grobPlot2,ncol=2)
  grid.draw(theme_tag)
  grid.draw(theme_line)
  grid.draw(source_text)
  grid.draw(emajor_text)

  #save to png
  png(png_filename, width = 800, height = 400)
  grid.newpage()
  grid.arrange(grobPlot1, grobPlot2,ncol=2)
  grid.draw(theme_tag)
  grid.draw(theme_line)
  grid.draw(source_text)
  grid.draw(emajor_text)
  dev.off()

}
