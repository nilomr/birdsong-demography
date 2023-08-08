# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


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

dm1_data <- sharing_data |>
    dplyr::filter(father != father2) |>
    dplyr::filter(resident_status == "Both") |>
    dplyr::filter(!is.na(natal_distance), !is.na(dispersal_distance)) |>
    dplyr::mutate(year_born_diff = as.numeric(year_born_diff)) |>
    dplyr::filter(!is.na(father), !is.na(father2)) |>
    # recode year_born_diff
    dplyr::mutate(
        year_born_diff = dplyr::case_when(
            year_born_diff == 0 ~ "0",
            year_born_diff == 1 ~ "1",
            year_born_diff >= 2 ~ "2+"
        )
    )

# Standardize the predictors
dm1_data_std <- dm1_data |>
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance),
        natal_distance = (natal_distance - mean(natal_distance)) /
            sd(natal_distance)
    )

# DEFINE AND FIT THE MODEL ───────────────────────────────────────────────── #


df1 <- brms::bf(
    mean_dist1 ~ 1 + natal_distance * nest_distance + year_born_diff +
        year + (1 | mm(father, father2))
)

dm1 <- brms::brm(
    df1,
    data = dm1_data_std,
    family = gaussian(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(4),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "dm1"),
    file_refit = "on_change",
)


# MODEL CHECKS ───────────────────────────────────────────────────────────── #

# brms::pp_check(dm1)
# brms::pp_check(dm1, type = "stat_2d")
# brms::stancode(dm1)
# plot(dm1)

c2 <- titpalette(2)[2]
bayesplot::color_scheme_set(c(
    c2, c2, c2, c2,
    "#292929", "#292929"
))

dm1_ppcheck <- brms::pp_check(dm1, type = "stat_2d", ndraws = 1000) +
    titheme() +
    labs(
        y = "SD", x = "Mean",
        title = "Posterior predictive check",
        subtitle = "Model: dm1"
    ) +
    theme(
        legend.justification = c(1, 1),
        aspect.ratio = 1,
        panel.grid.major = element_line(
            color = "#ececec",
            linewidth = 0.5,
            linetype = 1
        ),
        legend.spacing.y = unit(0.3, "lines"),
        # make plot background white
        panel.background = element_rect(fill = "white")
    )

ggsave(
    file.path(config$path$figures, "dm1_ppcheck.png"),
    plot = dm1_ppcheck,
    width = 10,
    height = 10,
    units = "cm",
    dpi = 300,
    # add a white background
    bg = "white",
    scale = 1.3
)


# PLOTTING ───────────────────────────────────────────────────────────────── #

# PREDICTIONS FOR TERRITORY AND NATAL DISTANCE ───────────────────────────── #

dm1grid <- marginaleffects::datagrid(
    model = dm1,
    nest_distance = dm1_data_std$nest_distance,
    natal_distance = c(
        max(dm1_data_std$natal_distance),
        mean(dm1_data_std$natal_distance),
        min(dm1_data_std$natal_distance)
    ),
    year_born_diff = "1",
    year = "2021"
)

data <- dm1_data_std
e <- residuals(dm1, summary = TRUE)[, "Estimate"]
grid <- match_grid(dm1grid, data)

dm1_grid2 <- marginaleffects::predictions(dm1,
    newdata = grid,
    type = "response", re_formula = NULL
)
dm1_grid2[["type"]] <- NULL
dm1_grid2[["estimate_with_error"]] <- dm1_grid2[["estimate"]] + e
dm1_grid2 <- dplyr::as_tibble(dm1_grid2)

dm1_grid2 <- og_scale(dm1_data, dm1_grid2, v = "nest_distance")
dm1_grid2 <- og_scale(dm1_data, dm1_grid2, v = "natal_distance")

# round natal_distance to the nearest integer
dm1_grid2 <- dm1_grid2 |>
    dplyr::mutate_at(vars(natal_distance), round, 0) |>
    dplyr::mutate_at(vars(natal_distance), as.factor)

p_colors <- colorspace::desaturate(titpalette(3, order = c(2, 3, 1)), .4)
f_colors <- titpalette(3, order = c(2, 3, 1))
l_colors <- colorspace::darken(titpalette(3, order = c(2, 3, 1)), .5)

dm1_p1 <- dm1_grid2 |>
    ggplot(aes(nest_distance)) +
    geom_point(
        aes(
            y = estimate_with_error,
            color = natal_distance
        ),
        alpha = 0.5,
        size = .3,
    ) +
    scale_color_manual(values = p_colors) +
    geom_ribbon(
        aes(
            ymin = conf.low, ymax = conf.high,
            fill = natal_distance
        ),
        alpha = 0.9,
    ) +
    scale_fill_manual(
        values = f_colors,
        labels = paste0(unique(dm1_grid2$natal_distance), "m")
    ) +
    guides(colour = "none") +
    ggnewscale::new_scale_color() +
    geom_line(
        aes(y = estimate, color = natal_distance),
        linewidth = .5
    ) +
    scale_color_manual(values = l_colors) +
    facet_wrap(
        vars(natal_distance),
        nrow = 1,
        # remove facet labels
        labeller = label_bquote(
            ""
        )
    ) +
    scale_x_continuous(
        expand = c(.05, .05), breaks = c(0, 1500, 3000),
        labels = c("0m", "1500m", "3000m")
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        # add breaks and labels every .5
        breaks = seq(0, 1, .05),
        labels = seq(0, 1, .05),
        # limit to NA to 1
        limits = c(NA, 1.05)
    ) +
    labs(
        title = "Effect of Territory & Natal Distance on Vocal Similarity",
        x = "Territory Distance",
        y = "Vocal Similarity",
        fill = "Natal\nDistance"
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )


ggsave(
    file.path(config$path$figures, "effect-of-territory-distance.png"),
    plot = dm1_p1,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)



# MARGINAL EFFECT OF AGE DIFFERENCE ─────────────────────────────────────── #


dm1_agedraws <- marginaleffects::comparisons(dm1,
    variables = "year_born_diff",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        nest_distance = mean(dm1_data_std$nest_distance),
        natal_distance =
            mean(dm1_data_std$natal_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()



p1 <- dm1_agedraws |>
    ggplot(aes(
        x = draw,
        fill = contrast,
    )) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_slab(
        alpha = .8
    ) +
    scale_fill_manual(
        name = "Contrast",
        labels = c("Same age\nvs 1y apart", "Same age\nvs >1y apart"),
        values = titpalette(2, o = c(2, 1))
    ) +
    titheme() +
    theme(
        aspect.ratio = .5,
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(
        breaks = c(-0.009, -0.006, -0.003, 0, 0.003),
        labels = c(-0.009, -0.006, -0.003, 0, 0.003)
    ) +
    labs(
        y = "Density",
        title = "Effect of Age Difference on Vocal Similarity"
    )

p2 <- dm1_agedraws |>
    ggplot(aes(x = draw, color = contrast)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_pointinterval(
        alpha = .7,
        point_size = 4,
        interval_size_range = c(0.8, 2),
        position = position_dodge(width = .2, preserve = "single"),
        show.legend = FALSE
    ) +
    scale_x_continuous(
        breaks = c(-0.009, -0.006, -0.003, 0, 0.003),
        labels = c(-0.009, -0.006, -0.003, 0, 0.003)
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
        x = "Change in Vocal Similarity"
    ) +
    scale_color_manual(values = titpalette(2, o = c(2, 1)))

fullplot <- (p1 / p2) &
    guides(fill = guide_legend(byrow = TRUE, title.position = "top")) &
    coord_cartesian(xlim = c(-0.009, 0.003)) &
    guides(
        colour = "none",
        fill = guide_legend(
            title = "Contrast",
            byrow = TRUE, title.position = "top"
        )
    ) &
    theme(legend.spacing.y = unit(.2, "cm"))

ggsave(
    file.path(config$path$figures, "age.png"),
    plot = fullplot,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)


# MARGINAL EFFECT OF NATAL AND TERRITORY DISTANCE ────────────────────────── #


dm1_draws1 <- marginaleffects::comparisons(dm1,
    variables = "natal_distance",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        nest_distance = min(dm1_data_std$nest_distance),
        year = "2020",
        year_born_diff = unique(dm1_data_std$year_born_diff)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate()

dm1_draws2 <- marginaleffects::comparisons(dm1,
    variables = "nest_distance",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        natal_distance = min(dm1_data_std$natal_distance),
        year = "2020",
        year_born_diff = unique(dm1_data_std$year_born_diff)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate()

dm1_distdraws <- dplyr::bind_rows(
    dm1_draws1 |>
        dplyr::mutate(term = "natal_distance"),
    dm1_draws2 |>
        dplyr::mutate(term = "nest_distance")
)


p1 <- dm1_distdraws |>
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
        values = titpalette(2, o = c(2, 1))
    ) +
    titheme() +
    theme(
        aspect.ratio = .5,
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(
        breaks = c(-0.015, -0.010, -0.005, 0, 0.005),
        labels = c(-0.015, -0.010, -0.005, 0, 0.005)
    ) +
    labs(
        y = "Density",
        title = "Effect of Territory & Natal Distance on Vocal Similarity",
        subtitle = "Marginal Effect at Minimum Distance"
    )

p2 <- dm1_distdraws |>
    ggplot(aes(x = draw, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_pointinterval(
        alpha = .7,
        point_size = 4,
        interval_size_range = c(0.8, 2),
        position = position_dodge(width = .2, preserve = "single"),
        show.legend = FALSE
    ) +
    scale_x_continuous(
        breaks = c(-0.015, -0.010, -0.005, 0, 0.005),
        labels = c(-0.015, -0.010, -0.005, 0, 0.005)
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
        x = "Change in Vocal Similarity with Distance"
    ) +
    scale_color_manual(values = titpalette(2, o = c(2, 1)))

fullplot <- (p1 / p2) &
    guides(fill = guide_legend(byrow = TRUE, title.position = "top")) &
    coord_cartesian(xlim = c(-0.015, 0.003)) &
    guides(
        colour = "none",
        fill = guide_legend(
            title = "Variable",
            byrow = TRUE, title.position = "top"
        )
    ) &
    theme(legend.spacing.y = unit(.2, "cm"))

ggsave(
    file.path(config$path$figures, "distance_margeffs.png"),
    plot = fullplot,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)
