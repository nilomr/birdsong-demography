# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD DATA ──────────────────────────────────────────────────────────────


disp_m_1 <- readRDS(file.path(config$path$fits, "disp_m_1.rds"))


# MARGINAL EFFECT OF NATAL AND TERRITORY DISTANCE ────────────────────────── #


disp_m_1_draws1 <- marginaleffects::avg_slopes(disp_m_1,
    variables = "natal_distance",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        nest_distance = min(disp_m_1$data$nest_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()

disp_m_1_draws2 <- marginaleffects::avg_slopes(disp_m_1,
    variables = "nest_distance",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        natal_distance = min(disp_m_1$data$natal_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate()

disp_m_1_distdraws <- dplyr::bind_rows(
    disp_m_1_draws1 |>
        dplyr::mutate(term = "natal_distance"),
    disp_m_1_draws2 |>
        dplyr::mutate(term = "nest_distance")
)

p1 <- disp_m_1_distdraws |>
    ggplot(aes(
        x = draw,
        fill = term,
    )) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_slab(
        alpha = .8
    ) +
    scale_fill_manual(
        name = "Variable",
        labels = c("Natal\nDistance", "Territory\nDistance"),
        values = blues
    ) +
    titheme() +
    theme(
        aspect.ratio = .4,
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        y = "Density",
        title = "Dispersal",
    )

p2 <- disp_m_1_distdraws |>
    ggplot(aes(x = draw, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_pointinterval(
        alpha = .7,
        point_size = 2,
        interval_size_range = c(0.7, 1.5),
        position = position_dodge(width = .2, preserve = "single"),
        show.legend = FALSE
    ) +
    titheme() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(
            margin =
                margin(t = 10, r = 0, b = 0, l = 0)
        ),
        aspect.ratio = .1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
        x = "Change in Cultural Similarity"
    ) +
    scale_color_manual(values = blues)

fullplot <- (p1 / p2) +
    scale_x_continuous(
        breaks = c(-0.015, -0.010, -0.005, 0, 0.005),
        labels = c(-0.015, -0.010, -0.005, 0, 0.005)
    ) &
    coord_cartesian(xlim = c(-0.015, 0.003)) &
    guides(
        colour = "none",
        fill = guide_legend(
            title = "Variable",
            byrow = TRUE, title.position = "top"
        )
    ) &
    theme(legend.spacing.y = unit(.2, "cm"))


saveRDS(fullplot, file.path(config$path$figures, "disp_m_1.rds"))
