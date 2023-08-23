# CONFIGURATION ------------------------------------------------------------

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
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
        pnum, prop_shared, mean_age, mean_age_sd, x, y, year, prop_rare,
        n_songs_current, neighbours, prop_same_birds, prop_resident,
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
            prop_resident, mean_dispersal_distance
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




# ESTIMATE EFEFCT OF DISPERSAL AND IMMIGRATION ON TOTAL/RARE REPERTOIRE --------


# Test if how far the birds have come from affects the proportion of rare songs,
# controlling for the proportion of residents and the neighbourhood size
nf2 <- brms::bf(
    prop_rare ~ 0 + prop_resident * mean_dispersal_distance + neighbours + year +
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


# Test if the proportion of resident birds affecs the proportion of rare songs
nf3 <- brms::bf(
    prop_rare ~ 0 + prop_resident + year +
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
# Since we know immigrants have bigegr repertoires, we can test if controlling
# for the interaction with total neighbourhood repertoire changes things
# (the proportion of rare songs changes with repertoire size)

nf4 <- brms::bf(
    prop_rare ~ 0 + prop_resident * n_songs_current + year +
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



plot(nm4)
brms::conditional_effects(nm4)




# PLOT PREDICTIONS FOR NM2 AND NM3 (DIPS, IMM ON RARE SONGS) -----------------



nm2grid <- marginaleffects::datagrid(
    model = nm2,
    mean_dispersal_distance = nm_data_std$mean_dispersal_distance,
    prop_resident = mean(nm_data_std$prop_resident),
    neighbours = mean(nm_data_std$neighbours)
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
        prop_resident = mean(nm_data_std$prop_resident),
        neighbours = mean(nm_data_std$neighbours)
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
        color = p_colors[1]
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = l_colors[1], fill = f_colors[1]
    ) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0, 0),
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(350, 750, 1150, 1550, 1950),
        labels = c("350m", "750m", "1150m", "1550m", "1950m")
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



# ------ second panel (immigration)

nm4grid <- marginaleffects::datagrid(
    model = nm4,
    prop_resident = nm_data_std$prop_resident,
    n_songs_current = mean(nm_data_std$n_songs_current)
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
        prop_resident = seq(
            min(nm_data_std$prop_resident),
            max(nm_data_std$prop_resident),
            by = 0.1
        ),
        n_songs_current = mean(nm_data_std$n_songs_current)
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm4_grid2 <- og_scale(nm_data, nm4_grid2, v = "prop_resident")
pred <- og_scale(nm_data, pred, v = "prop_resident")



nm4plot1 <-
    pred |>
    ggplot(aes(x = 1 - prop_resident, y = draw)) +
    geom_point(
        data = nm4_grid2,
        aes(
            y = estimate_with_error
        ),
        alpha = 0.5,
        size = .3,
        color = p_colors[2]
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = l_colors[2], fill = f_colors[2]
    ) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0, 0),
    ) +
    scale_x_continuous(
        expand = c(0, 0)
        # breaks = c(350, 750, 1150, 1550, 1950),
        # labels = c("350m", "750m", "1150m", "1550m", "1950m")
    ) +
    labs(
        x = "Proportion of Immigrants",
        y = "Cultural Novelty",
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
    file.path(config$path$figures, "imm_disp_neighbourhood.png"),
    imm_disp_neighplot,
    width = 6, height = 3.5, dpi = 300, bg = "white"
)
