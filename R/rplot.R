theme_frame <- function(text_size = 12) {
    ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.ticks = ggplot2::element_blank(),
        # add distance between axis and axis labels
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
        # add distance between axis and axis ticks
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
        # set all text to the same size
        text = ggplot2::element_text(size = text_size)

    )
}
