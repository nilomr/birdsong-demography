# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, yellows])
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


im1_data <- sharing_data |>
    dplyr::filter(
        year == year2, !is.na(father), !is.na(father2),
        !is.na(resident_status),
        !is.na(nest_distance),
        !is.na(age2)
    ) |>
    dplyr::mutate(
        resident_status = as.factor(resident_status),
        resident2 = as.factor(resident2),
        year_born_diff = dplyr::case_when(
            year_born_diff == 0 ~ "0",
            year_born_diff == 1 ~ "1",
            year_born_diff >= 2 ~ "2+"
        )
    ) |>
    dplyr::select(
        mean_dist1, year, year2, year_born_diff, father, father2,
        resident_status, nest_distance
    )


im1_data_std <- im1_data |>
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance)
    )


# DEFINE AND FIT IM1 ─────────────────────────────────────────────────────── #


if1 <- brms::bf(
    mean_dist1 ~ 0 + resident_status + year_born_diff + nest_distance +
        year + (1 | mm(father, father2))
)

im1 <- brms::brm(
    if1,
    data = im1_data_std,
    family = gaussian(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "im1"),
    file_refit = "on_change",
)


# MODEL CHECKS ───────────────────────────────────────────────────────────── #

c2 <- titpalette(2)[2]
bayesplot::color_scheme_set(c(
    c2, c2, c2, c2,
    "#292929", "#292929"
))

bayesplot::color_scheme_view()
dm1_ppcheck <- brms::pp_check(im1, type = "stat_2d", ndraws = 1000) +
    titheme() +
    labs(
        y = "SD", x = "Mean",
        title = "Posterior predictive check",
        subtitle = "Model: im1"
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
    file.path(config$path$figures, "im1_ppcheck.png"),
    plot = dm1_ppcheck,
    width = 10,
    height = 10,
    units = "cm",
    dpi = 300,
    # add a white background
    bg = "white",
    scale = 1.3
)


# PLOT EFFECT OF RESIDENT STATUS ON SIMILARITY ───────────────────────────── #


im1_draws <- marginaleffects::avg_slopes(im1,
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

# Hypothesis test that immigrant birds are more dissimilar than resident birds
# when birds are in the same area and are one year apart
hy1m1 <- marginaleffects::predictions(
    im1,
    newdata = marginaleffects::datagrid(
        nest_distance = min(im1_data_std$nest_distance),
        resident_status = unique(im1_data_std$resident_status),
        year_born_diff = "1",
        year = "2020"
    )
) |>
    marginaleffects::posterior_draws(shape = "DxP") |>
    brms::hypothesis("b3 - b1 < 0")

print(hy1m1$hypothesis, digits = 2)


p1 <- im1_draws |>
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

p2 <- im1_draws |>
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

saveRDS(fullplot, file.path(config$path$figures, "individual_immigration.rds"))

ggsave(
    file.path(config$path$figures, "individual_immigration.png"),
    plot = fullplot,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)
