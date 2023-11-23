# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds, blues, yellows])
box::use(R / utils[og_scale, match_grid, partial_residuals])
box::use(patchwork[...])
box::use(ggplot2[...])

# ──── LOAD DATA ──────────────────────────────────────────────────────────────


age_m_1 <- readRDS(file.path(config$path$fits, "age_m_1.rds"))
disp_m_1 <- readRDS(file.path(config$path$fits, "disp_m_1.rds"))
imm_m_1 <- readRDS(file.path(config$path$fits, "imm_m_1.rds"))


# MARGINAL EFFECT OF NATAL AND TERRITORY DISTANCE ────────────────────────── #


disp_m_1_draws1 <- marginaleffects::avg_slopes(disp_m_1,
    variables = "natal_distance",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        nest_distance = mean(disp_m_1$data$nest_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()

disp_m_1_draws2 <- marginaleffects::avg_slopes(disp_m_1,
    variables = "nest_distance",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        natal_distance = mean(disp_m_1$data$natal_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate()

disp_m_1_draws <- dplyr::bind_rows(
    disp_m_1_draws1 |>
        dplyr::mutate(term = "natal_distance"),
    disp_m_1_draws2 |>
        dplyr::mutate(term = "nest_distance")
)


# PLOT EFFECT OF RESIDENT STATUS ON SIMILARITY ───────────────────────────── #


imm_m_1_draws <- marginaleffects::avg_slopes(imm_m_1,
    variables = "resident_status",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        nest_distance = min(imm_m_1$data$nest_distance),
        year_born_diff = "1",
        year = 2020
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate(term = contrast)


# ──── MARGINAL EFFECT OF AGE DIFFERENCE ──────────────────────────────────────

age_m_1_draws <- marginaleffects::avg_slopes(age_m_1,
    variables = "year_born_diff",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        year_born_diff = c("0", "1", "2", "3", "4+"),
        nest_distance = mean(age_m_1$data$nest_distance),
        natal_distance =
            mean(age_m_1$data$natal_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate(term = contrast)



# ──── JOIN AND PLOT ─────────────────────────────────────────────────────────

# bind all the data rogether, indicating which model it came from
disp_m_1_draws <- disp_m_1_draws |>
    dplyr::mutate(model = "disp_m_1")

age_m_1_draws <- age_m_1_draws |>
    dplyr::mutate(model = "age_m_1")

imm_m_1_draws <- imm_m_1_draws |>
    dplyr::mutate(model = "imm_m_1")

all_draws <- dplyr::bind_rows(
    disp_m_1_draws,
    age_m_1_draws,
    imm_m_1_draws
)


fill_colors <- c(
    "natal_distance" = blues[1],
    "nest_distance" = blues[2],
    "1 - 0" = reds[4],
    "2 - 0" = reds[3],
    "3 - 0" = reds[2],
    "4+ - 0" = reds[1],
    "Neither - Both" = yellows[1],
    "One - Both" = yellows[2]
)

individual_plot <- plot <-
    all_draws |>
    ggplot(aes(x = draw, y = model, fill = term, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_halfeye(alpha = .9) +
    scale_fill_manual(
        values = fill_colors,
        guide = guide_legend(
            byrow = TRUE, title.position = "top",
            keywidth = 0.8, keyheight = 0.8,
            title = "Comparison",
            override.aes = list(linetype = 0, shape = NA)
        )
    ) +
    scale_color_manual(
        values = sapply(fill_colors, colorspace::darken, .2),
    ) +
    scale_y_discrete(
        limits = rev(c("disp_m_1", "imm_m_1", "age_m_1")),
        expand = c(0.1, 0.1),
        labels = c(
            "imm_m_1" = "Immigration",
            "age_m_1" = "Age Difference",
            "disp_m_1" = "Dispersal"
        )
    ) +
    # set x axis limits to -0.03, 0.03
    scale_x_continuous(
        limits = c(-0.017, 0.007),
        expand = c(0, 0),
        breaks = c(-0.03, -0.02, -0.015, -0.01, -0.005, 0, 0.005, 0.01, 0.02)
    ) +
    labs(x = "Marginal Effect", y = "") +
    guides(color = "none") +
    titheme() +
    theme(
        legend.box.just = "right",
        aspect.ratio = 1,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(.2, "cm")
    )

individual_mefs <- individual_plot +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 12, face = "bold"))

ggsave(
    file.path(config$path$figures, "individual_all.svg"),
    individual_plot,
    bg = "transparent",
    device = svglite::svglite,
    width = 5, height = 4, dpi = 300
)
