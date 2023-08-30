# ──── CONFIGURATION ──────────────────────────────────────────────────────────

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid, get_spatial_preds, get_raster])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD DATA ──────────────────────────────────────────────────────────────


# Load neighbourhood data
neighbour_df <- read_csv_file(
    file.path(config$path$derived_data, "neighbour_data.csv")
)

# ──── PREPARE DATA ───────────────────────────────────────────────────────────

turn_data <- neighbour_df |>
    dplyr::filter(year %in% c(2021, 2022)) |>
    dplyr::select(
        pnum, prop_shared, mean_age, x, y, year, prop_rare,
        n_current_songs, neighbours, prop_same_birds, prop_immigrant,
        mean_dispersal_distance
    ) |>
    dplyr::mutate(
        year = as.factor(year)
    ) |>
    dplyr::filter(
        !is.na(mean_dispersal_distance), !grepl("empty", pnum)
    )
saveRDS(turn_data, file.path(config$path$fits, "turn_data.rds"))


turn_data_std <- turn_data |>
    dplyr::mutate_at(
        vars(
            mean_age, neighbours, prop_same_birds,
            prop_immigrant, mean_dispersal_distance
        ),
        function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    )



# ──── DEFINE AND FIT MODELS ──────────────────────────────────────────────────

# prop_shared should be interpreted as the proportion of the songs from the previous
# calendar year that are still around in a neighbourhood in the current year
# (below as 1 - prop_shared


# Individual turnover vs cultural turnover

# This model just to calculate for spatial autocorrelation in the residuals
turn_f_0 <- brms::bf(
    prop_shared ~ 0 + prop_same_birds + year,
    hu ~ prop_same_birds + year
)

turn_m_0 <- brms::brm(
    turn_f_0,
    data = turn_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "turn_m_0"),
    file_refit = "never",
)


turn_f_1 <- brms::bf(
    prop_shared ~ 0 + prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4),
    hu ~ prop_same_birds + year
)

turn_m_1 <- brms::brm(
    turn_f_1,
    data = turn_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "turn_m_1"),
    file_refit = "never",
)


# Age vs cultural turnover, controlling for individual turnover
turn_f_2 <- brms::bf(
    prop_shared ~ 0 + mean_age + prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4),
    hu ~ mean_age
)

turn_m_2 <- brms::brm(
    turn_f_2,
    data = turn_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "turn_m_2"),
    file_refit = "never",
)


# Full model
turn_f_3 <- brms::bf(
    prop_shared ~ 0 + prop_immigrant + mean_dispersal_distance + mean_age +
        prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4),
    hu ~ 1
)

turn_m_3 <- brms::brm(
    turn_f_3,
    data = turn_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "turn_m_3"),
    file_refit = "never",
)
