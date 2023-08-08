# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))

box::use(R / io[read_csv_file])
box::use(mef = marginaleffects)


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# Load song_sharing_data
sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "cont_pairwise_data.csv")
) |>
    dplyr::mutate(year = as.factor(year), year2 = as.factor(year2))
main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)


# FIT THE MODELS ─────────────────────────────────────────────────────────── #


m1_data <- sharing_data |>
    dplyr::filter(year == year2, nest_distance < 2000) |>
    # standardize the predictors
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) / sd(nest_distance)
    ) |>
    dplyr::mutate(year = as.factor(year)) |>
    dplyr::filter(!is.na(father), !is.na(father2))

f1 <- brms::bf(
    mean_dist1 ~ 1 + s(nest_distance, by = resident_status) +
        year + (1 | mm(father, father2))
)
m1 <- brms::brm(
    f1,
    data = m1_data,
    family = gaussian(),
    # prior = f1p,
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    # control = list(adapt_delta = 0.99, max_treedepth = 15),
    file = file.path(config$path$fits, "m1"),
    file_refit = "on_change",
)


marginaleffects::plot_predictions(m1, condition = c("nest_distance", "year"))
brms::pp_check(m1)

comparisons(mod,
    variables = list(PClass = c("1st", "3rd"))
) # Step 1: Quantity


condition <- list("natal_distance", "dispersal_distance")

# plot predictions using the marginaleffects package
marginaleffects::plot_predictions(m1, condition = list("nest_distance", "resident_status"), point = .2) +
    ggplot2::theme_bw() +
    # limit y axis to 0.9 - end of y range
    ggplot2::ylim(0.9, NA)
mfx <- marginaleffects::slopes(m1, variables = "nest_distance")
broom::tidy(mfx)

pred <- marginaleffects::predictions(
    m1,
    newdata = datagrid(
        pnum = NA,
        pnum2 = NA,
        nest_distance = min(m1_data$nest_distance):0,
        year = unique(m1_data$year)
    ),
    re.form = NA
)

brms::pp_check(m1)

# get stan code
m1 |>
    brms::stancode() |>
    cat()





m3_data <- sharing_data |>
    dplyr::filter(year == year2, nest_distance < 200) |>
    # convert resident status to a factor
    dplyr::mutate(resident_status = as.factor(resident_status)) |>
    dplyr::select(
        year, pnum, pnum2,
        age_difference, nest_distance, resident_status,
        shared, total
    ) |>
    # remove rows where resident status is NA
    dplyr::filter(!is.na(resident_status)) |>
    # standardize the predictors
    dplyr::mutate(
        nest_distance = (nest_distance - mean(nest_distance)) /
            sd(nest_distance)
    )

f3 <- brms::bf(
    shared | trials(total) ~ 1 + nest_distance + resident_status +
        year + (1 | mm(pnum, pnum2))
)

m3 <- brms::brm(
    f3,
    data = m3_data,
    family = brms::zero_inflated_binomial(),
    # prior = f1p,
    iter = 2000,
    warmup = 1000,
    chains = 4,
    cores = 4,
    threads = brms::threading(4),
    backend = "cmdstanr",
    # control = list(adapt_delta = 0.99, max_treedepth = 15),
    file = file.path(config$path$fits, "m3"),
    file_refit = "on_change",
)

brms::conditional_effects(m3)

plot(m3)
brms::pp_check(m3, type = "bars")
