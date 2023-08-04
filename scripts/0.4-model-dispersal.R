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

rgb3 <- titpalette(3)
bayesplot::color_scheme_set(c(
    rgb3[3], rgb3[3], rgb3[3], rgb3[3],
    "black", "black"
))

bayesplot::color_scheme_view()
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

p_colors <- rev(colorspace::desaturate(titpalette(3), .3))
f_colors <- rev(titpalette(3))
l_colors <- rev(colorspace::darken(titpalette(3), .5))

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
        limits = c(NA, 1)
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
        labels = c("0 vs 1y apart", "0 vs >1y apart"),
        values = titpalette(2)
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
    scale_color_manual(values = titpalette(2))

fullplot <- (p1 / p2) &
    guides(fill = guide_legend(byrow = TRUE, title.position = "top")) &
    coord_cartesian(xlim = c(-0.009, 0.003)) &
    guides(
        colour = "none",
        fill = guide_legend(reverse = TRUE)
    )

ggsave(
    file.path(config$path$figures, "age.png"),
    plot = fullplot,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)



dm1_agedraws |>
    ggplot(aes(x = draw)) +
    ggdist::stat_halfeye(aes(fill = contrast)) +
    labs(
        x = "Effect of an increase in age difference on vocal similarity",
        fill = "Age difference"
    )




# Second panel (effect of interaction between territory distance,
# natal distance, and age difference)


nd1 <- marginaleffects::datagrid(
    model = dm1,
    natal_distance = min(dm1_data_std$natal_distance):max(dm1_data_std$natal_distance),
    nest_distance = c(
        max(dm1_data_std$nest_distance),
        mean(dm1_data_std$nest_distance), min(dm1_data_std$nest_distance)
    ),
    year_born_diff = c("0", "1", "2+"),
    year = "2020"
) |> dplyr::as_tibble()

dm1preds <- marginaleffects::predictions(
    dm1,
    re_formula = NULL, newdata = nd1
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()


dm1preds <- og_scale(dm1_data, dm1preds, v = "natal_distance")
dm1preds <- og_scale(dm1_data, dm1preds, v = "nest_distance")
dm1preds <- dm1preds |>
    dplyr::mutate_at(vars(nest_distance), round, 0)


flabels <- c(`0` = "Same Age", `1` = "1 Year Apart", `2+` = "2+ Years Apart") |>
    as_labeller()
# plot estimate vs nest_distance in m2preds
dm1p2 <-
    ggplot() +
    # use ggdist to add ribbons
    ggdist::stat_lineribbon(
        data = dm1preds,
        aes(
            x = natal_distance, y = draw,
            fill = as.factor(nest_distance),
            color = as.factor(nest_distance)
        ),
        alpha = 0.3,
        .width = c(.5, .7, .95)
    ) +
    # use the rgb3 colour palette
    scale_fill_manual(
        values = rev(titpalette(3))
    ) +
    scale_color_manual(
        values = rev(titpalette(3))
    ) +
    theme(aspect.ratio = 1.3) +
    guides(colour = "none") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_wrap(
        vars(year_born_diff),
        ncol = 1,
        labeller = flabels
    ) +
    # turn facet labels 90 degrees
    titheme() +
    guides(
        fill = guide_legend(
            title = "Territory Distance",
            override.aes = list(alpha = 1),
            byrow = TRUE
        )
    ) +
    theme(
        legend.spacing.y = unit(.3, "cm"),
        strip.text = element_text(
            margin = margin(t = 0, r = 0, b = 6, l = 0)
        ),
    ) +
    labs(
        x = "Natal Distance (m)",
        y = NULL,
    )

dm1p2

# change title of legend and make legend colors full opacity
# remove y axis title

# combine plots

dispersal_p1 <- dm1_p1 + dm1p2

ggsave(
    file.path(config$path$figures, "dispersal_p1.png"),
    plot = dispersal_p1,
    width = 1000,
    height = 900,
    units = "px",
    dpi = 300,
    scale = 2.2
)





snd <- marginaleffects::datagrid(
    model = dm1,
    nest_distance = min(dm1_data_std$nest_distance):max(dm1_data_std$nest_distance),
    natal_distance = c(
        min(dm1_data_std$natal_distance),
        mean(dm1_data_std$natal_distance), max(dm1_data_std$natal_distance)
    ),
    year_born_diff = c("0", "1", "2+")
) |> dplyr::as_tibble()

dm1_slopes <- marginaleffects::slopes(
    dm1,
    variables = "nest_distance",
    newdata = marginaleffects::datagrid(
        natal_distance = c(
            min(dm1_data_std$natal_distance),
            median(dm1_data_std$natal_distance), max(dm1_data_std$natal_distance)
        )
    ),
    re_formula = NULL
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()


marginaleffects::plot_slopes(dm1,
    variables = "nest_distance", condition = "natal_distance"
) + titheme()


# Plot marginal effects of territory distance by natal distance
ggplot(dm1_slopes_b, aes(
    x = draw, fill = factor(natal_distance)
)) +
    ggdist::stat_halfeye(slab_alpha = .7) +
    labs(
        x = "Marginal Effect of Territory Distance on Song Similarity",
        fill = "Natal distance",
        title = "Posterior pred dist of mean acoustic distance"
    ) +
    # add a dashed vertical line at 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    # use the rgb3 colour palette
    scale_fill_manual(values = rev(rgb3)) +
    titheme()

performance::check_model(dm1)


# round the variables uyntil the 2nd decimal place
dm1preds <- dm1preds |>
    dplyr::mutate_at(vars(natal_distance, nest_distance), round, 2)

ggplot(dm1preds, aes(x = draw, fill = as.factor(natal_distance))) +
    ggdist::stat_halfeye(alpha = .5) +
    labs(
        x = "Mean acoustic similarity",
        fill = "Natal distance",
        title = "Posterior pred dist of mean acoustic distance"
    ) +
    # add the tite for the facet wrap
    facet_wrap(~nest_distance, nrow = 1, labeller = label_parsed) +
    # add a diverging continuous colour scale for nest_distance, with
    scale_fill_brewer(palette = "RdBu", direction = -1) +
    titheme()


brms::conditional_effects(dm2)
brms::pp_check(dm2)
plot(dm2)




dm2_data <- sharing_data |>
    dplyr::filter(
        year == year2, !is.na(father), !is.na(father2),
        nest_distance < 1000
    ) |>
    dplyr::filter(!is.na(resident_status), !is.na(nest_distance)) |>
    dplyr::mutate(resident_status = as.factor(resident_status))

dm2_data_std <- dm2_data |>
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance)
    )

df2 <- brms::bf(
    mean_dist1 ~ 1 + resident_status +
        year + (1 | mm(father, father2))
)

dm2 <- brms::brm(
    df2,
    data = dm2_data_std,
    family = gaussian(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "dm2"),
    file_refit = "on_change",
)

brms::conditional_effects(dm2)

plot(dm1)
brms::pp_check(m3, type = "bars")
