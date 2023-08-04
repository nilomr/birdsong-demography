# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))

box::use(R / io[read_csv_file])
box::use(mef = marginaleffects)
box::use(patchwork[...])

# FUNCTION DEFINITIONS ───────────────────────────────────────────────────── #



# color palette:
rgb3 <- c("#75c8ae", "#5a3d2b", "#e5771e")
rgb2 <- c("#75c8ae", "#e5771e")


titheme <- function() {
    ggplot2::theme(
        text = ggplot2::element_text(size = 12, family = "Helvetica"),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
            colour = "black", fill = NA, linewidth = 1.5
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
    )
}



#'
#' This function back-transforms a variable in df2 to the original scale of
#' the same variable in df1. Assumes that variable was mean-centered and
#' scaled by the standard deviation.
#'
#' @param df1 A data frame containing the original unscaled data.
#' @param df2 A data frame containing the scaled data.
#' @param v The name of the variable to back-transform in df1.
#' @param v2 The name of the variable to create or update in df2.
#'
#' @return A data frame with the back-transformed variable.
#'
#' @examples
#' df1 <- data.frame(x = rnorm(100, 10, 2))
#' df2 <- data.frame(x = scale(df1$x))
#' df2 <- og_scale(df1, df2, "x")
#'
#' @export
og_scale <- function(df1, df2, v = NULL, v2 = NULL) {
    if (!is.null(v2) && !v2 %in% names(df2)) {
        stop("v2 is not in df2")
    } else if (is.null(v2) && !v %in% names(df2) &&
        !v %in% names(df1)) {
        stop("v is not in df2 or df1")
    }
    # back transform v in df2 to the original scale of v in df1
    if (!is.null(v2)) {
        df2 <- df2 |>
            dplyr::mutate(
                !!dplyr::sym(v2) := !!dplyr::sym(v2) * sd(df1[[v]]) +
                    mean(df1[[v]])
            )
    } else {
        df2 <- df2 |>
            dplyr::mutate(
                !!dplyr::sym(v) := !!dplyr::sym(v) * sd(df1[[v]]) +
                    mean(df1[[v]])
            )
    }
    return(dplyr::as_tibble(df2))
}


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# Load song_sharing_data
sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "cont_pairwise_data.csv")
) |>
    dplyr::mutate(year = as.factor(year), year2 = as.factor(year2))
main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)


# PREPARE THE DATA ───────────────────────────────────────────────────────── #


rm1_data <- main_data |>
    dplyr::filter(
        year %in% c(2020, 2021, 2022),
        # !is.na(resident), !is.na(father),
        n_vocalisations > 0,
        !is.na(april_lay_date),
        !is.na(father),
    ) |>
    dplyr::mutate(
        sampling_effort = total_recordings - missing_recordings,
        year = as.factor(year)
    )

rm1_data_std <- rm1_data |>
    dplyr::mutate(
        sampling_effort = (sampling_effort - mean(sampling_effort)) /
            sd(sampling_effort),
        april_lay_date = (april_lay_date - mean(april_lay_date)) /
            sd(april_lay_date),
    )

if1 <- brms::bf(
    repertoire_size ~ 1 + resident + s(april_lay_date) +
        s(sampling_effort) + year + (1 | father)
)

rm1 <- brms::brm(
    if1,
    data = rm1_data_std,
    family = brms::cratio(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rm1"),
    file_refit = "on_change",
)

brms::conditional_effects(rm1)

plot(rm1)
brms::pp_check(rm1, type = "bars")
brms::pp_check(rm1, type = "ecdf_overlay") + titheme()
jtools::effect_plot(rm1,
    pred = resident, interval = TRUE,
    int.type = "confidence", int.width = 0.95
)

print(brms::hypothesis(rm1, "residentTRUE < 0"))

# EXTRACT POSTERIOR PREDICTIONS ──────────────────────────────────────────── #


ndi1 <- marginaleffects::datagrid(
    model = rm1,
    resident = c(TRUE, FALSE),
    april_lay_date = mean(rm1_data_std$april_lay_date, na.rm = TRUE),
    sampling_effort = mean(rm1_data_std$sampling_effort, na.rm = TRUE),
    year = unique(rm1_data_std$year)
) |> dplyr::as_tibble()

rm1preds <- marginaleffects::predictions(
    rm1,
    re_formula = NULL, newdata = ndi1
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()

rm1preds <- og_scale(rm1_data, rm1preds, v = "nest_distance")

unique(rm1preds$repertoire_size)


# PLOT POSTERIORS ────────────────────────────────────────────────────────── #


flabels <- c(`0` = "Same Age", `1` = "1 Year Apart", `2+` = "2+ Years Apart") |>
    ggplot2::as_labeller()


rm1preds |>
    ggplot2::ggplot(ggplot2::aes(x = group, y = draw, fill = as.factor(resident))) +
    ggplot2::geom_point()

marginaleffects::plot_predictions(rm1, condition = "resident")

# plot immigration status
rm1p1 <-
    ggplot2::ggplot() +
    # use ggdist to add ribbons
    ggdist::stat_halfeye(
        data = rm1preds,
        ggplot2::aes(
            x = draw,
            fill = as.factor(resident_status),
            color = as.factor(resident_status)
        ),
        alpha = 0.7
    ) +
    # use the rgb3 colour palette
    ggplot2::scale_fill_manual(
        values = rev(rgb3)
    ) +
    ggplot2::scale_color_manual(
        values = rev(rgb3)
    ) +
    ggplot2::guides(colour = "none") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.08, 0)) +
    titheme() +
    ggplot2::theme(aspect.ratio = .5) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            title = "Resident",
            override.aes = list(alpha = 1),
            byrow = TRUE
        )
    ) +
    #
    ggplot2::labs(
        x = "Song Repertoire Similarity",
        y = NULL,
    )

rm1p1

# plot resident_status

rm1p2 <- ggplot2::ggplot() +
    # use ggdist to add ribbons
    ggdist::stat_halfeye(
        data = rm1preds,
        ggplot2::aes(
            x = draw,
            fill = as.factor(year_born_diff),
            color = as.factor(year_born_diff)
        ),
        alpha = 0.7
    ) +
    # use the rgb3 colour palette
    ggplot2::scale_fill_manual(
        values = rev(rgb3)
    ) +
    ggplot2::scale_color_manual(
        values = rev(rgb3)
    ) +
    ggplot2::guides(colour = "none") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.08, 0)) +
    titheme() +
    ggplot2::theme(aspect.ratio = .5) +
    ggplot2::guides(
        fill = ggplot2::guide_legend(
            title = "Age Difference",
            override.aes = list(alpha = 1),
            byrow = TRUE
        )
    ) +
    #
    ggplot2::labs(
        x = "Song Repertoire Similarity",
        y = NULL,
    )


immigration_p1 <- rm1p2 +
    # remove x title, ticks, tick labels
    ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
    ) +
    rm1p1 + plot_layout(guides = "collect", ncol = 1)


ggplot2::ggsave(
    file.path(config$path$figures, "immigration_p1.png"),
    plot = immigration_p1,
    width = 1000,
    height = 900,
    units = "px",
    dpi = 300,
    scale = 1.8
)
