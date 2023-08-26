# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds])
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

age_m_1_data <- sharing_data |>
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
            year_born_diff == 2 ~ "2",
            year_born_diff == 3 ~ "3",
            year_born_diff >= 4 ~ "4+",
        )
    )

# Standardize the predictors
age_m_1_data_std <- age_m_1_data |>
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance),
        natal_distance = (natal_distance - mean(natal_distance)) /
            sd(natal_distance)
    )

# DEFINE AND FIT THE MODEL ───────────────────────────────────────────────── #

age_f_1 <- brms::bf(
    mean_dist1 ~ 1 + natal_distance + nest_distance + year_born_diff +
        year + (1 | mm(father, father2))
)

age_m_1 <- brms::brm(
    age_f_1,
    data = age_m_1_data_std,
    family = gaussian(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(4),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "age_m_1"),
    file_refit = "on_change",
)





main_data |>
    dplyr::filter(year %in% c(2020, 2021, 2022), recorded == TRUE, n_vocalisations > 0) |>
    # get unique pnums and count them
    dplyr::distinct(pnum) |>
    dplyr::count()
