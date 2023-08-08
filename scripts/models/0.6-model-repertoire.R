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
        year = as.factor(year)
    )

rm2_data_std <- rm2_data |>
    dplyr::mutate(
        sampling_effort = (sampling_effort - mean(sampling_effort)) /
            sd(sampling_effort),
        april_lay_date = (april_lay_date - mean(april_lay_date)) /
            sd(april_lay_date),
    )


if2 <- brms::bf(
    rare_n ~ 1 + resident + s(april_lay_date) +
        s(sampling_effort) + year + (1 | father)
)

rm2 <- brms::brm(
    if2,
    data = rm2_data_std,
    family = poisson(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    control = list(adapt_delta = 0.99),
    file = file.path(config$path$fits, "rm2"),
    file_refit = "on_change",
)

brms::pp_check(rm2, type = "bars", ndraws = 100)
print(brms::hypothesis(rm2, "residentTRUE = 0")$hypothesis, digits = 2)
