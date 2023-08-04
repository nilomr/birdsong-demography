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


#' Custom ggplot2 theme
#'
#' This function returns a custom ggplot2 theme with a minimalistic design.
#'
#' @return A ggplot2 theme object.
#' @import ggplot2
#' @export
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     labs(title = "Custom ggplot2 theme", subtitle = "A minimalistic design") +
#'     titheme()
#'
#' @export
titheme <- function() {
    ggplot2::theme(
        text = ggplot2::element_text(
            size = 12, family = "Roboto Condensed",
            colour = "#272727"
        ),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
            colour = "#c0c0c0", fill = NA, linewidth = 1
        ),
        panel.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        aspect.ratio = .8,
        panel.grid.major.y = ggplot2::element_line(
            color = "#ececec",
            linewidth = 0.5,
            linetype = 1
        ),
        axis.title.y = ggplot2::element_text(
            margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)
        ),
        axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
        ),
        strip.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        strip.text = ggplot2::element_text(
            size = 12, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 10)
        ),
        plot.subtitle = ggplot2::element_text(
            margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
        ),
        legend.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        legend.box.background = ggplot2::element_rect(
            fill = "white", colour = NA
        ),
        legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.spacing.y = ggplot2::unit(0.3, "lines"),
        # title to roboto condensed, size 12
        plot.title = ggplot2::element_text(
            size = 13, face = "bold", colour = "black",
            margin = ggplot2::margin(t = 0, r = 0, b = 10, l = 0)
        )
    )
}




#' Custom color palette
#'
#' This function returns a custom color palette with 2 or 3 colors.
#'
#' @param n An integer specifying the number of colors in the palette.
#' @return A named vector of color hex codes.
#' @examples
#' # get a 2-color palette
#' titpalette(2)
#'
#' # get a 3-color palette
#' titpalette(3)
#'
#' # error: n must be 2 or 3
#' titpalette(4)
#'
#' @export
titpalette <- function(n = 3) {
    # Define the lookup table
    lookup <- list(
        `2` = c("#75c8ae", "#e5771e"),
        `3` = c("#75c8ae", "#886557", "#e5771e"),
        `4` = c("#f75435", "#684c41", "#4fa3a5", "#fdae38")
    )

    # Check if n is a valid key in the lookup table
    if (!as.character(n) %in% names(lookup)) {
        stop("n must be one of ", paste(names(lookup), collapse = ", "))
    }

    # Return the corresponding palette
    return(lookup[[as.character(n)]])
}
