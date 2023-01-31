#' Add Waapihk theme to ggplot chart
#'
#' This function allows you to add the Waapihk theme to your ggplotgraphics.
#' @keywords waapihk_style
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#' geom_line(colour = "#007f7f", size = 1) +
#' geom_hline(yintercept = 0, size = 1, colour="#333333") +
#' waapihk_style()

theme_waapihk <- function(
    base_size=18,
    base_family="Open Sans"
  ) {

  font <- base_family

  ggplot2::theme(
    plot.margin=unit(c(.5,.8,0,.8),"cm"),
    plot.background=ggplot2::element_rect( fill="#ffffff", colour=NA ),
    plot.title = ggplot2::element_text(family=font, size=24, lineheight=1.2, face="bold", color="#222222", margin=ggplot2::margin(0,0,0,0)),
    plot.subtitle = ggplot2::element_text(family=font, size=16, lineheight=1, margin=ggplot2::margin(5,0,10,0)),
    #This leaves the caption text element empty, because it is set elsewhere in the finalize plot function
    plot.caption = ggplot2::element_blank(),



    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.key = element_rect(fill="transparent", colour=NA),
    legend.key.width=unit(3, "mm"),
    legend.key.height=unit(3, "mm"),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.text=ggplot2::element_text(family=font, size=14, color="#222222", lineheight=1.2),



    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title=ggplot2::element_text(family=font, size=14, color="#000000"),
    axis.text=ggplot2::element_text(family=font, size=12, color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_line(color="#777777"),
    axis.line = ggplot2::element_line(color="#777777"),
    # axis.line.x=ggplot2::element_blank(),
    # axis.line.y=ggplot2::element_blank(),



    #Grid lines
    panel.grid.major = ggplot2::element_blank(),
    # panel.grid.major.x = ggplot2::element_blank(),
    # panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.minor = ggplot2::element_blank(),
    # panel.grid.minor.x = ggplot2::element_blank(),
    # panel.grid.minor.y = ggplot2::element_line(color="#cbcbcb"),
    panel.spacing=unit(2, "lines"),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill="#ffffff"),


    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="#ffffff"),
    strip.text = ggplot2::element_text(family=font, face="bold", color="#555555", size =14, margin=ggplot2::margin(5,0,10,0), hjust=0)


  )
}


