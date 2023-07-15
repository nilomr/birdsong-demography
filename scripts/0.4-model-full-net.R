# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))

box::use(R / io[read_csv_file])


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# Load song_sharing_data
sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "pairwise_data.csv")
)
main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

# MODEL DEFINITIONS ──────────────────────────────────────────────────────── #

# define the full model


f1 <- brms::bf(
    shared | trials(total) ~ 1 + nest_distance + resident_status +
        year + (1 | mm(pnum, pnum2))
)
f1p <- c(
    brms::prior(normal(0, 10), class = Intercept),
    brms::prior(normal(0, 10), class = b)
)


# FIT THE MODELS ─────────────────────────────────────────────────────────── #

m1 <- brms::brm(
    f1,
    data = sharing_data |>
        dplyr::filter(year == year2, nest_distance < 1000),
    family = brms::zero_inflated_binomial(),
    iter = 2000,
    warmup = 1000,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    # control = list(adapt_delta = 0.99, max_treedepth = 15),
    file = file.path(config$path$fits, "m1"),
    file_refit = "on_change",
)

brms::conditional_effects(m1)
