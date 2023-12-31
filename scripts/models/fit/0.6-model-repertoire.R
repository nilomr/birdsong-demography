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


# ──── PREPARE DATA ───────────────────────────────────────────────────────────


rep_data <- main_data |>
    dplyr::filter(
        year %in% c(2020, 2021, 2022),
        !is.na(resident),
        n_vocalisations > 0,
        !is.na(april_lay_date),
        !is.na(father),
    ) |>
    dplyr::mutate(
        sampling_effort = total_recordings - missing_recordings,
        year = as.factor(year),
        immigrant = !resident,
        age = as.numeric(age)
    ) |>
    # only keep the columns we need later in the analysis]
    dplyr::select(
        repertoire_size, average_frequency, sampling_effort,
        dispersal_distance, year, immigrant, father, age
    )

standardize <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

rep_data_std <- rep_data |>
    dplyr::mutate_at(
        dplyr::vars(sampling_effort, dispersal_distance),
        standardize
    )

# REPERTOIRE SIZE: RESIDENTS VS IMMIGRANT BIRDS ──────────────────────────── #


rep_f_1 <- brms::bf(
    repertoire_size ~ 1 + immigrant + s(sampling_effort) + year + (1 | father)
)

rep_m_1 <- brms::brm(
    rep_f_1,
    data = rep_data_std,
    family = brms::cratio(),
    iter = 2800,
    warmup = 1500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rep_m_1"),
    file_refit = "never",
)


# REPERTOIRE SIZE: DISPERSAL ──────────────────────────────────────────────── #


rep_f_1.1 <- brms::bf(
    repertoire_size ~ 1 + dispersal_distance + s(sampling_effort) +
        year + (1 | father)
)

rep_m_1.1 <- brms::brm(
    rep_f_1.1,
    data = rep_data_std,
    family = brms::cratio(),
    iter = 2800,
    warmup = 1500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rep_m_1.1"),
    file_refit = "never",
)


# ──── REPERTOIRE SIZE: AGE ───────────────────────────────────────────────────

rep_f_1.2 <- brms::bf(
    repertoire_size ~ 1 + age + s(sampling_effort) + year + (1 | father)
)

rep_m_1.2 <- brms::brm(
    rep_f_1.2,
    data = rep_data_std,
    family = brms::cratio(),
    iter = 2800,
    warmup = 1500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rep_m_1.2"),
    file_refit = "never",
)


# ──── REPERTOIRE NOVELTY: IMMIGRATION ────────────────────────────────────────


repnov_f_1 <- brms::bf(
    average_frequency ~ 1 + immigrant + s(sampling_effort) +
        year + (1 | father)
)

repnov_m_1 <- brms::brm(
    repnov_f_1,
    data = rep_data_std,
    family = brms::lognormal(),
    iter = 2800,
    warmup = 1500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "repnov_m_1"),
    file_refit = "on_change",
)


# ──── REPERTOIRE NOVELTY: DISPERSAL ──────────────────────────────────────────


repnov_f_1.1 <- brms::bf(
    average_frequency ~ 1 + dispersal_distance +
        s(sampling_effort) + year + (1 | father)
)

repnov_m_1.1 <- brms::brm(
    repnov_f_1.1,
    data = rep_data_std,
    family = brms::lognormal(),
    iter = 2800,
    warmup = 1500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "repnov_m_1.1"),
    file_refit = "on_change",
)


# ──── REPERTOIRE NOVELTY: AGE ────────────────────────────────────────────────

repnov_f_1.2 <- brms::bf(
    average_frequency ~ 1 + age + s(sampling_effort) +
        year + (1 | father)
)

repnov_m_1.2 <- brms::brm(
    repnov_f_1.2,
    data = rep_data_std,
    family = brms::lognormal(),
    iter = 2800,
    warmup = 1500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "repnov_m_1.2"),
    file_refit = "on_change",
)
