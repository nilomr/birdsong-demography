# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD DATA ──────────────────────────────────────────────────────────────


imm_m_1 <- readRDS(file.path(config$path$fits, "imm_m_1.rds"))


# PLOT EFFECT OF RESIDENT STATUS ON SIMILARITY ───────────────────────────── #


imm_m_1_draws <- marginaleffects::avg_slopes(imm_m_1,
    variables = "resident_status",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        nest_distance = min(dm1_data_std$nest_distance),
        year_born_diff = "1",
        year = 2020
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()


p1 <- imm_m_1_draws |>
    ggplot(aes(
        x = draw,
        fill = contrast,
    )) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_slab(
        alpha = .8
    ) +
    scale_fill_manual(
        labels = c(
            "Both immigrants\nvs both residents",
            "One resident\nvs both resident"
        ),
        values = yellows
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
        title = "Immigration"
    )

p2 <- imm_m_1_draws |>
    ggplot(aes(x = draw, color = contrast)) +
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
        aspect.ratio = .1, # remove y axis text:
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
        x = "Change in Cultural Similarity"
    ) +
    scale_color_manual(values = yellows)

fullplot <- (p1 / p2) &
    scale_x_continuous(
        breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
    ) &
    coord_cartesian(xlim = c(-0.02, 0.015)) &
    guides(
        colour = "none",
        fill = guide_legend(
            title = "Comparison",
            byrow = TRUE, title.position = "top"
        )
    ) &
    theme(legend.spacing.y = unit(.2, "cm"))

saveRDS(fullplot, file.path(config$path$figures, "imm_m_1.rds"))
