# CONFIGURATION ------------------------------------------------------------

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, blues, yellows])
box::use(R / utils[og_scale, match_grid, get_spatial_preds, get_raster])
box::use(patchwork[...])
box::use(ggplot2[...])


# LOAD DATA ----------------------------------------------------------------


# Load neighbourhood data
neighbour_df <- read_csv_file(
    file.path(config$path$derived_data, "neighbour_data.csv")
)

manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)


# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$derived_data, "wytham_map", "perimeter.shp")
) |>
    terra::project("+init=epsg:27700")
# remove the 4 smallest polygons in the spatvector
pop_contour_sf <- pop_contour |> sf::st_as_sf()



# PREPARE DATA ---------------------------------------------------------------

nm_data <- neighbour_df |>
    dplyr::filter(year %in% c(2021, 2022)) |>
    dplyr::select(
        pnum, prop_shared, mean_age, mean_age_sd, x, y, year, novelty, diversity,
        n_unique_current_songs, neighbours, prop_same_birds, prop_immigrant,
        mean_dispersal_distance
    ) |>
    dplyr::mutate(
        year = as.factor(year)
    ) |>
    dplyr::filter(
        !is.na(mean_dispersal_distance), !grepl("empty", pnum)
    )


nm_data_std <- nm_data |>
    dplyr::mutate_at(
        vars(
            mean_age, mean_age_sd, neighbours, prop_same_birds,
            prop_immigrant, mean_dispersal_distance, n_unique_current_songs
        ),
        function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    )

# calculate the average and sd prop_shared by year
nm_data_std |>
    dplyr::group_by(year) |>
    dplyr::summarise(
        mean_prop_shared = mean(prop_shared, na.rm = TRUE),
        sd_prop_shared = sd(prop_shared, na.rm = TRUE)
    )




# ESTIMATE EFEFCT OF DISPERSAL -----------------------------------------------


# Dispersal on novelty
nf2 <- brms::bf(
    novelty ~ 0 + prop_immigrant + mean_dispersal_distance +
        n_unique_current_songs + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)

nm2 <- brms::brm(
    nf2,
    data = nm_data_std,
    family = brms::lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm2"),
    file_refit = "on_change",
)


# open nm2.rds
readRDS(file.path(config$path$fits, "nm2.rds")) |>
    summary() |>
    print()

# TODO: fit models here, then open to plot in a file, and to summarise in other


# Dispersal on diversity
df2 <- brms::bf(
    diversity ~ 0 + prop_immigrant + mean_dispersal_distance +
        n_unique_current_songs + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)

dm2 <- brms::brm(
    df2,
    data = nm_data_std,
    family = brms::lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "dm2"),
    file_refit = "on_change",
)


# ESTIMATE EFEFCT OF IMMIGRATION -----------------------------------------------


# Test if the proportion of resident birds affects the proportion of rare songs
nf3 <- brms::bf(
    novelty ~ 0 + prop_immigrant + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)


nm3 <- brms::brm(
    nf3,
    data = nm_data_std,
    family = brms::lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm3"),
    file_refit = "on_change",
)


# Test if the proportion of resident birds affecs the proportion of rare songs,
# Since we know immigrants have bigger repertoires, we can test if controlling
# for the interaction with total neighbourhood repertoire changes things
# (the proportion of rare songs changes with repertoire size)

nf4 <- brms::bf(
    novelty ~ 0 + prop_immigrant + n_unique_current_songs + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)


nm4 <- brms::brm(
    nf4,
    data = nm_data_std,
    family = brms::lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm4"),
    file_refit = "on_change",
)


# does individual turnover affect the proportion of rare songs?

nf5 <- brms::bf(
    novelty ~ 0 + prop_same_birds + mean_age + year + n_unique_current_songs +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE)
)
nm5 <- brms::brm(
    nf5,
    data = nm_data_std,
    family = brms::lognormal(),
    iter = 1500,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm5"),
    file_refit = "on_change",
)





brms::hypothesis(nm5, "prop_same_birds > 0")$hypothesis
brms::hypothesis(nm5, "mean_age > 0")$hypothesis

plot(nm5)



# PLOT PREDICTIONS FOR NM2 AND NM4 (DIPS, IMM ON RARE SONGS) -----------------



# Levels for the ribbon
levels <- 30


# P1: dispersal

nm2grid <- marginaleffects::datagrid(
    model = nm2,
    mean_dispersal_distance = nm_data_std$mean_dispersal_distance,
    prop_immigrant = mean(nm_data_std$prop_immigrant),
    n_unique_current_songs = mean(nm_data_std$n_unique_current_songs)
)

data <- nm_data_std
e <- residuals(nm2, summary = TRUE)[, "Estimate"]
grid <- match_grid(nm2grid, data)

nm2_grid2 <- marginaleffects::predictions(nm2,
    newdata = grid,
    type = "response", re_formula = NULL
)
nm2_grid2[["type"]] <- NULL
nm2_grid2[["estimate_with_error"]] <- nm2_grid2[["estimate"]] + e
nm2_grid2 <- dplyr::as_tibble(nm2_grid2)


# mean predictions
pred <- marginaleffects::predictions(nm2,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        mean_dispersal_distance = seq(
            min(nm_data_std$mean_dispersal_distance),
            max(nm_data_std$mean_dispersal_distance),
            by = 0.1
        ),
        prop_immigrant = mean(nm_data_std$prop_immigrant),
        n_unique_current_songs = mean(nm_data_std$n_unique_current_songs)
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm2_grid2 <- og_scale(nm_data, nm2_grid2, v = "mean_dispersal_distance")
pred <- og_scale(nm_data, pred, v = "mean_dispersal_distance")


nm2plot1 <-
    pred |>
    ggplot(aes(x = mean_dispersal_distance, y = draw)) +
    geom_point(
        data = nm2_grid2,
        aes(
            y = estimate_with_error
        ),
        alpha = 0.5,
        size = .3,
        color = blues[2]
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = blues[2], fill = blues[2]
    ) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0, 0),
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(300, 900, 1500),
        labels = c("350m", "900m", "1500m")
    ) +
    labs(
        x = "Mean Dispersal Distance",
        y = "Cultural Novelty",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )



# P2: Immigration

nm4grid <- marginaleffects::datagrid(
    model = nm4,
    prop_immigrant = nm_data_std$prop_immigrant,
    n_unique_current_songs = mean(nm_data_std$n_unique_current_songs)
)

data <- nm_data_std
e <- residuals(nm4, summary = TRUE)[, "Estimate"]
grid <- match_grid(nm4grid, data)

nm4_grid2 <- marginaleffects::predictions(nm4,
    newdata = grid,
    type = "response", re_formula = NULL
)
nm4_grid2[["type"]] <- NULL
nm4_grid2[["estimate_with_error"]] <- nm4_grid2[["estimate"]] + e
nm4_grid2 <- dplyr::as_tibble(nm4_grid2)

# mean predictions
pred <- marginaleffects::predictions(nm4,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        prop_immigrant = seq(
            min(nm_data_std$prop_immigrant),
            max(nm_data_std$prop_immigrant),
            by = 0.1
        ),
        n_unique_current_songs = mean(nm_data_std$n_unique_current_songs)
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm4_grid2 <- og_scale(nm_data, nm4_grid2, v = "prop_immigrant")
pred <- og_scale(nm_data, pred, v = "prop_immigrant")


nm4plot1 <-
    pred |>
    ggplot(aes(x = prop_immigrant, y = draw)) +
    geom_point(
        data = nm4_grid2,
        aes(
            y = estimate_with_error
        ),
        alpha = 0.5,
        size = .3,
        color = yellows[2]
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = yellows[1], fill = yellows[1]
    ) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0, 0),
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(0.35, 0.5, 0.65, 0.8, 0.95)
    ) +
    labs(
        x = "Proportion of Immigrants",
        y = "Cultural Diversity",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )



# P3: Age -------------------------------------------------------------------
# TODO: plot both age and turnover!


nm5grid <- marginaleffects::datagrid(
    model = nm5,
    mean_age = nm_data_std$mean_age,
    prop_same_birds = mean(nm_data_std$prop_same_birds),
    n_unique_current_songs = mean(nm_data_std$n_unique_current_songs)
)

data <- nm_data_std
e <- residuals(nm5, summary = TRUE)[, "Estimate"]
grid <- match_grid(nm5grid, data)

nm5_grid2 <- marginaleffects::predictions(nm5,
    newdata = grid,
    type = "response", re_formula = NULL
)
nm5_grid2[["type"]] <- NULL
nm5_grid2[["estimate_with_error"]] <- nm5_grid2[["estimate"]] + e
nm5_grid2 <- dplyr::as_tibble(nm5_grid2)


plot(nm5)

# mean predictions
pred <- marginaleffects::predictions(nm5,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        mean_age = seq(
            min(nm_data_std$mean_age),
            max(nm_data_std$mean_age),
            by = 0.1
        ),
        prop_same_birds = mean(nm_data_std$prop_same_birds),
        n_unique_current_songs = mean(nm_data_std$n_unique_current_songs)
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm5_grid2 <- og_scale(nm_data, nm5_grid2, v = "mean_age")
pred <- og_scale(nm_data, pred, v = "mean_age")


nm5plot1 <-
    pred |>
    ggplot(aes(x = mean_age, y = draw)) +
    geom_point(
        data = nm5_grid2,
        aes(
            y = estimate_with_error
        ),
        alpha = 0.5,
        size = .3,
        color = blues[2]
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = blues[2], fill = blues[2]
    ) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0, 0),
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        # breaks = c(300, 900, 1500),
        # labels = c("350m", "900m", "1500m")
    ) +
    labs(
        x = "Mean Dispersal Distance",
        y = "Cultural Diversity",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )





imm_disp_neighplot <- ((nm2plot1 + nm4plot1) + theme(
    legend.position = "none"
)) + plot_annotation(tag_levels = "A") &
    coord_cartesian(ylim = c(0.55, 0.78)) &
    theme(plot.tag = element_text(size = 10))


ggsave(
    file.path(config$path$figures, "imm_disp_neighbourhood.svg"),
    imm_disp_neighplot,
    width = 6, height = 3.5, dpi = 300, bg = "white"
)
