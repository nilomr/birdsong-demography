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

# PREPARE THE DATA ───────────────────────────────────────────────────────── #


imm_m_1_data <- sharing_data |>
    dplyr::filter(
        year == year2, !is.na(father), !is.na(father2),
        !is.na(resident_status),
        !is.na(nest_distance),
        !is.na(age2)
    ) |>
    dplyr::mutate(
        resident_status = as.factor(resident_status),
        resident2 = as.factor(resident2)
    ) |>
    dplyr::select(
        mean_dist1, year, year2, year_born_diff, father, father2,
        resident_status, nest_distance
    )

imm_m_1_data_std <- imm_m_1_data |>
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance)
    )


# DEFINE AND FIT imm_m_1 ─────────────────────────────────────────────────────── #


imm_f_1 <- brms::bf(
    mean_dist1 ~ 0 + resident_status + year_born_diff + nest_distance +
        year + (1 | mm(father, father2))
)

imm_m_1 <- brms::brm(
    imm_f_1,
    data = imm_m_1_data_std,
    family = gaussian(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "imm_m_1"),
    file_refit = "on_change",
)
