# CONFIGURATION ------------------------------------------------------------

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, blues, yellows, reds])
box::use(R / utils[og_scale, match_grid, get_spatial_preds, get_raster])
box::use(patchwork[...])
box::use(ggplot2[...])


# LOAD DATA ----------------------------------------------------------------


# Load neighbourhood data
neighbour_df <- read_csv_file(
    file.path(config$path$derived_data, "neighbour_data.csv")
)


# PREPARE DATA ---------------------------------------------------------------

div_data <- neighbour_df |>
    dplyr::select(
        pnum, prop_shared, mean_age, mean_age_sd, x, y, year, novelty, diversity,
        n_unique_current_songs, neighbours, n_current_songs, recorded, prop_same_birds,
        prop_immigrant, mean_dispersal_distance
    ) |>
    dplyr::mutate(
        year = as.factor(year)
    ) |>
    dplyr::filter(
        !is.na(mean_dispersal_distance), !grepl("empty", pnum), recorded > 2
    )

saveRDS(div_data, file.path(config$path$fits, "div_data.rds"))


div_data_std <- div_data |>
    dplyr::mutate_at(
        vars(
            mean_age, neighbours, prop_same_birds,
            prop_immigrant, mean_dispersal_distance, n_unique_current_songs
        ),
        function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    )


# ──── ESTIMATE EFEFCTS ON DIVERSITY (RELATIVE AND NOVELTY) ───────────────────


# On cultural novelty

div_f_1 <- brms::bf(
    novelty ~ 0 + prop_immigrant + mean_dispersal_distance + prop_same_birds +
        mean_age + s(n_current_songs) + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)


div_m_1 <- brms::brm(
    div_f_1,
    data = div_data_std,
    family = brms::lognormal(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.9),
    file = file.path(config$path$fits, "div_m_1"),
    file_refit = "never",
)

# On cultural diversity

div_f_2 <- brms::bf(
    diversity ~ 0 + prop_immigrant + mean_dispersal_distance + prop_same_birds +
        mean_age + s(n_current_songs) + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)

div_m_2 <- brms::brm(
    div_f_2,
    data = div_data_std,
    family = brms::lognormal(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.9),
    file = file.path(config$path$fits, "div_m_2"),
    file_refit = "never",
)



# ──── SUBSIDIARY MODELS ──────────────────────────────────────────────────────

# Relationship between diversity and novelty

div_f_3 <- brms::bf(
    novelty ~ 0 + diversity + s(n_current_songs) + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)

div_m_3 <- brms::brm(
    div_f_3,
    data = div_data_std,
    family = brms::lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "div_m_3"),
    file_refit = "never",
)

# Diversity and novelty not controlling for sampling effect

div_f_3.1 <- brms::bf(
    novelty ~ 0 + diversity + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)

div_m_3.1 <- brms::brm(
    div_f_3.1,
    data = div_data_std,
    family = brms::lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "div_m_3.1"),
    file_refit = "never",
)
