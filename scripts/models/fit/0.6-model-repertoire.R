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

songtype_data <- read_csv_file(
    file.path(config$path$derived_data, "pairwise_data.csv")
)

main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)


# REPERTOIRE SIZE: RESIDENTS VS IMMIGRANT BIRDS ──────────────────────────── #


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

# MODEL CHECKS ───────────────────────────────────────────────────────────── #

brms::conditional_effects(rm1)

c2 <- titpalette(2)[2]
bayesplot::color_scheme_set(c(
    c2, c2, c2, c2,
    "#292929", "#292929"
))
rm1_ppcheck <-
    brms::pp_check(rm1, type = "bars", ndraws = 100) +
    titheme() +
    scale_x_continuous(breaks = seq(0, 13, 1))

ggsave(
    file.path(config$path$figures, "rm1_ppcheck.png"),
    rm1_ppcheck,
    width = 10,
    height = 10,
    units = "cm",
    dpi = 300,
    # add a white background
    bg = "white",
    scale = 1.3
)

print(brms::hypothesis(rm1, "residentTRUE < 0")$hypothesis, digits = 2)



# REPERTOIRE NOVELTY: RESIDENTS VS IMMIGRANTS ────────────────────────────── #


rm2_data <- main_data |>
    dplyr::filter(
        year %in% c(2020, 2021, 2022),
        # !is.na(resident), !is.na(father),
        n_vocalisations > 0,
        !is.na(april_lay_date),
        !is.na(father),
    ) |>
    dplyr::mutate(
        sampling_effort = total_recordings - missing_recordings,
        year = as.factor(year),
        prop_rare = rare_n / repertoire_size
    )

rm2_data_std <- rm2_data |>
    dplyr::mutate(
        sampling_effort = (sampling_effort - mean(sampling_effort)) /
            sd(sampling_effort),
        april_lay_date = (april_lay_date - mean(april_lay_date)) /
            sd(april_lay_date),
    )


# Do immigrant and resident birds differ in the proportion of rare songs?
if2 <- brms::bf(
    prop_rare ~ 0 + resident + s(april_lay_date) +
        s(sampling_effort) + year + (1 | father)
)

rm2 <- brms::brm(
    if2,
    data = rm2_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rm2"),
    file_refit = "on_change",
)


print(brms::hypothesis(rm2, "residentTRUE = 0")$hypothesis, digits = 2)


# does the n of rare songs increase linearly with repertoire size?

if3 <- brms::bf(
    rare_n ~ 1 + repertoire_size + s(april_lay_date) +
        s(sampling_effort) + year + (1 | father)
)

rm3 <- brms::brm(
    if3,
    data = rm2_data_std,
    family = poisson(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rm3"),
    file_refit = "on_change",
)
brms::conditional_effects(rm3)
brms::pp_check(rm3, type = "bars", ndraws = 100)



# Plot fit of rm3 (repertoire size only)

rm3grid <- marginaleffects::datagrid(
    model = rm3,
    repertoire_size = seq(
        min(rm2_data_std$repertoire_size, na.rm = TRUE),
        max(rm2_data_std$repertoire_size, na.rm = TRUE),
        by = 1
    )
)

data <- rm2_data_std
e <- residuals(rm3, summary = TRUE)[, "Estimate"]
grid <- match_grid(rm3grid, data)

rm3_grid2 <- marginaleffects::predictions(rm3,
    newdata = grid,
    type = "response", re_formula = NULL
)
rm3_grid2[["type"]] <- NULL
rm3_grid2[["estimate_with_error"]] <- rm3_grid2[["estimate"]] + e

rm3_grid2[["estimate_with_error"]] <- ifelse(
    rm3_grid2[["estimate_with_error"]] < 0,
    0,
    rm3_grid2[["estimate_with_error"]]
)
rm3_grid2 <- dplyr::as_tibble(rm3_grid2)

# mean predictions
pred <- marginaleffects::predictions(rm3,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        repertoire_size = seq(
            min(rm2_data_std$repertoire_size, na.rm = TRUE),
            max(rm2_data_std$repertoire_size, na.rm = TRUE),
            by = 1
        )
    )
) |>
    marginaleffects::posterior_draws()

p_colors <- colorspace::desaturate(titpalette(3, order = c(2, 3, 1)), .4)

# from p_colors[1] create a 3-color gradient based on opacity
p_colors2 <- rev(c(
    p_colors[1],
    colorspace::lighten(p_colors[1], .5, space = "combined"),
    colorspace::lighten(p_colors[1], .8, space = "combined")
))
rm3plot1 <-
    pred |>
    ggplot(aes(x = repertoire_size, y = draw)) +
    ggdist::stat_interval(alpha = .5) +
    geom_point(
        data = rm3_grid2,
        aes(
            y = estimate_with_error
        ),
        alpha = 0.5,
        size = .3,
        color = p_colors[1],
        position = position_jitter(width = 0.2, height = 0.2)
    ) +
    scale_color_manual(values = p_colors2) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0.1, 0),
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(min(rm2_data_std$repertoire_size, na.rm = TRUE),
            max(rm2_data_std$repertoire_size, na.rm = TRUE),
            by = 2
        )
    ) +
    labs(
        x = "Repertoire Size",
        y = "Number of Rare Songs",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )

ggsave(
    file.path(config$path$figures, "rare_vs_repsize.png"),
    rm3plot1,
    width = 3.5, height = 3.5, dpi = 300, bg = "white"
)
