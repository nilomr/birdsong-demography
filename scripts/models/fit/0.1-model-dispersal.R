# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, blues])
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

disp_m_1_data <- sharing_data |>
    dplyr::filter(father != father2) |>
    dplyr::filter(resident_status == "Both") |>
    dplyr::filter(!is.na(natal_distance), !is.na(dispersal_distance)) |>
    dplyr::filter(!is.na(father), !is.na(father2))

# Standardize the predictors
disp_m_1_data_std <- disp_m_1_data |>
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance),
        natal_distance = (natal_distance - mean(natal_distance)) /
            sd(natal_distance)
    )

# DEFINE AND FIT THE MODEL ───────────────────────────────────────────────── #


disp_f_1 <- brms::bf(
    mean_dist1 ~ 0 + natal_distance * nest_distance + year_born_diff +
        year + (1 | mm(father, father2))
)

disp_m_1 <- brms::brm(
    disp_f_1,
    data = disp_m_1_data_std,
    family = gaussian(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "disp_m_1"),
    file_refit = "never",
)
