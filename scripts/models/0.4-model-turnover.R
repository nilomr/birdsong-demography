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
        pnum, prop_shared, mean_age, x, y, year, prop_rare,
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
            mean_age, neighbours, prop_same_birds,
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



# Population-level change
myears <- manual_labels |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4))
myears_list <- lapply(unique(myears$year), function(x) myears$class_label[myears$year == x])
myears_list <- lapply(myears_list, unique)
dplyr::tibble(
    prop_shared_1_2 = length(intersect(myears_list[[1]], myears_list[[2]]))
    / length(myears_list[[2]]),
    prop_shared_2_3 = length(intersect(myears_list[[2]], myears_list[[3]]))
    / length(myears_list[[2]]),
    prop_shared_1_3 = length(intersect(myears_list[[1]], myears_list[[3]]))
    / length(myears_list[[3]])
)


# DEFINE AND FIT MODELS ------------------------------------------------------


# prop_shared should be interpreted as the proportion of the songs from the previous
# calendar year that are still around in a neighbourhood in the current year

# CULTURAL TURNOVER MODELS ---------------------------------------------------
# (below as 1 - prop_shared)


nf0 <- brms::bf(
    prop_shared ~ 0 + mean_age + year,
    hu ~ mean_age
)

nm0 <- brms::brm(
    nf0,
    data = nm_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 250,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm0"),
    file_refit = "on_change",
)


nf0.1 <- brms::bf(
    prop_shared ~ 0 + prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE),
    hu ~ prop_same_birds
)

nm0.1 <- brms::brm(
    nf0.1,
    data = nm_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm0.1"),
    file_refit = "on_change",
)


nf1 <- brms::bf(
    prop_shared ~ 0 + mean_age + prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE),
    hu ~ mean_age
)

nm1 <- brms::brm(
    nf1,
    data = nm_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm1"),
    file_refit = "on_change",
)


# Immigration vs cultural turnover
# NOTE: this should be proportion of new birds that are immigrants?
nf2 <- brms::bf(
    prop_shared ~ 0 + prop_resident + prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE),
    hu ~ prop_resident + prop_same_birds + year
)

nm2 <- brms::brm(
    nf2,
    data = nm_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm2"),
    file_refit = "on_change",
)

# Dispersal vs cultural turnover

nf3 <- brms::bf(
    prop_shared ~ 0 + mean_dispersal_distance + prop_resident +
        prop_same_birds + year +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE),
    hu ~ mean_dispersal_distance + prop_resident + prop_same_birds + year
)

nm3 <- brms::brm(
    nf3,
    data = nm_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    seed = 42,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm3"),
    file_refit = "on_change",
)

checks = FALSE
if (checks) {
    plot(nm1)
    brms::bayes_R2(nm1)
    brms::conditional_effects(nm1.1)
    brms::pp_check(nm1.1)
    brms::pp_check(nm1)
    brms::conditional_effects(nm1)
    brms::hypothesis(nm1, "mean_age > 0")
}

# PLOT EFFECT OF MEAN NEIGHBOUR AGE ON PROP_SHARED -------------------------

nm1grid <- marginaleffects::datagrid(
    model = nm1,
    mean_age = nm_data_std$mean_age,
    prop_same_birds = mean(nm_data$prop_same_birds)
)

data <- nm_data_std
e <- residuals(nm1, summary = TRUE)[, "Estimate"]
grid <- match_grid(nm1grid, data)

nm1_grid2 <- marginaleffects::predictions(nm1,
    newdata = grid,
    type = "response", re_formula = NULL
)
nm1_grid2[["type"]] <- NULL
nm1_grid2[["estimate_with_error"]] <- nm1_grid2[["estimate"]] + e
nm1_grid2 <- dplyr::as_tibble(nm1_grid2)

# mean predictions
pred <- marginaleffects::predictions(nm1,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        mean_age = seq(
            min(nm_data_std$mean_age),
            max(nm_data_std$mean_age),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm1_grid2 <- og_scale(nm_data, nm1_grid2, v = "mean_age")
pred <- og_scale(nm_data, pred, v = "mean_age")


p_colors <- colorspace::desaturate(titpalette(3, order = c(2, 3, 1)), .4)
f_colors <- titpalette(3, order = c(2, 3, 1))
l_colors <- colorspace::darken(titpalette(3, order = c(2, 3, 1)), .5)
levels <- 30


# take any points in nm1_grid2$estimate_with_error that are at either
# end of the distribution and subtract or add 0. so that they are visible in the plot
nm1_grid2$mean_age[nm1_grid2$mean_age == max(nm1_grid2$mean_age)] <-
    max(nm1_grid2$mean_age) - 0.03
nm1_grid2$mean_age[nm1_grid2$mean_age == 1] <- 1.03


nm1plot1 <-
    pred |>
    ggplot(aes(x = mean_age, y = 1 - draw)) +
    geom_point(
        data = nm1_grid2,
        aes(
            y = 1 - estimate_with_error
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
        limits = c(0.25, 1)
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(1, 1.5, 2, 2.5, 3),
        labels = c(1, 1.5, 2, 2.5, 3)
    ) +
    labs(
        x = "Mean Neighbourhood Age",
        y = "Cultural Turnover"
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )


ggsave(
    file.path(config$path$figures, "age_and_turnover.png"),
    plot = nm1plot1,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)


# PLOT EFFECT OF PROP SAME BIRDS ON PROP_SHARED -------------------------

nm0.1grid <- marginaleffects::datagrid(
    model = nm0.1,
    prop_same_birds = nm_data_std$prop_same_birds
)

data <- nm_data_std
e <- residuals(nm0.1, summary = TRUE)[, "Estimate"]
grid <- match_grid(nm0.1grid, data)

nm0.1_grid2 <- marginaleffects::predictions(nm0.1,
    newdata = grid,
    type = "response", re_formula = NULL
)
nm0.1_grid2[["type"]] <- NULL
nm0.1_grid2[["estimate_with_error"]] <- nm0.1_grid2[["estimate"]] + e
nm0.1_grid2 <- dplyr::as_tibble(nm0.1_grid2)

# mean predictions
pred <- marginaleffects::predictions(nm0.1,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        prop_same_birds = seq(
            min(nm_data_std$prop_same_birds),
            max(nm_data_std$prop_same_birds),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm0.1_grid2 <- og_scale(nm_data, nm0.1_grid2, v = "prop_same_birds")
pred <- og_scale(nm_data, pred, v = "prop_same_birds")



nm0.1plot2 <-
    pred |>
    ggplot(aes(x = 1 - prop_same_birds, y = 1 - draw)) +
    geom_point(
        data = nm0.1_grid2,
        aes(
            y = 1 - estimate_with_error,
            x = 1 - prop_same_birds
        ),
        alpha = 0.5,
        size = .3,
        color = p_colors[3]
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = l_colors[3], fill = f_colors[3]
    ) +
    guides(colour = "none") +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0.25, 1)
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    labs(
        x = "Individual Turnover",
        y = "Cultural Turnover"
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )


ggsave(
    file.path(config$path$figures, "bird_turn_and_cult_turn.png"),
    plot = nm0.1plot2,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)


# PLOT EFFECT OF PROP RESIDENT ON PROP_SHARED -------------------------

nm2grid <- marginaleffects::datagrid(
    model = nm2,
    prop_resident = nm_data_std$prop_resident,
    prop_same_birds = mean(nm_data$prop_same_birds)
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
        prop_resident = seq(
            min(nm_data_std$prop_resident),
            max(nm_data_std$prop_resident),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm2_grid2 <- og_scale(nm_data, nm2_grid2, v = "prop_resident")
pred <- og_scale(nm_data, pred, v = "prop_resident")


nm2plot1 <-
    pred |>
    ggplot(aes(x = 1 - prop_resident, y = 1 - draw)) +
    geom_point(
        data = nm2_grid2,
        aes(
            y = 1 - estimate_with_error,
            x = 1 - prop_resident
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
        limits = c(0.25, 1)
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    labs(
        x = "Proportion of Immigrants",
        y = "Cultural Turnover"
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )


# PLOT EFFECT OF MEAN DISPERSAL DISTANCE ON PROP_SHARED -------------------------

nm3grid <- marginaleffects::datagrid(
    model = nm3,
    mean_dispersal_distance = nm_data_std$mean_dispersal_distance,
    prop_resident = mean(nm_data$prop_resident),
    prop_same_birds = mean(nm_data$prop_same_birds)
)

data <- nm_data_std
e <- residuals(nm3, summary = TRUE)[, "Estimate"]
grid <- match_grid(nm3grid, data)

nm3_grid2 <- marginaleffects::predictions(nm3,
    newdata = grid,
    type = "response", re_formula = NULL
)
nm3_grid2[["type"]] <- NULL
nm3_grid2[["estimate_with_error"]] <- nm3_grid2[["estimate"]] + e
nm3_grid2 <- dplyr::as_tibble(nm3_grid2)

# mean predictions
pred <- marginaleffects::predictions(nm3,
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        mean_dispersal_distance = seq(
            min(nm_data_std$mean_dispersal_distance),
            max(nm_data_std$mean_dispersal_distance),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nm3_grid2 <- og_scale(nm_data, nm3_grid2, v = "mean_dispersal_distance")
pred <- og_scale(nm_data, pred, v = "mean_dispersal_distance")


nm3plot1 <-
    pred |>
    ggplot(aes(x = mean_dispersal_distance, y = 1 - draw)) +
    geom_point(
        data = nm3_grid2,
        aes(
            y = 1 - estimate_with_error,
            x = mean_dispersal_distance
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
        limits = c(0.25, 1)
    ) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = c(350, 750, 1150, 1550, 1950),
        labels = c("350m", "750m", "1150m", "1550m", "1950m")
    ) +
    labs(
        x = "Mean Dispersal Distance",
        y = "Cultural Turnover"
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )



# PLOT EFFECT ESTIMATES nm1, mean_age + prop_same_birds-------------------------


nm1_draws1 <- marginaleffects::comparisons(nm1,
    variables = "mean_age",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        prop_same_birds = min(nm_data$prop_same_birds)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate()

nm1_draws2 <- marginaleffects::comparisons(nm0.1,
    variables = "prop_same_birds",
    type = "response",
    re_formula = NULL
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble() |>
    dplyr::mutate()

nm1_distdraws <- dplyr::bind_rows(
    nm1_draws1 |>
        dplyr::mutate(term = "mean_age"),
    nm1_draws2 |>
        dplyr::mutate(term = "prop_same_birds")
) |>
    # 1- draws in mean_age so that the effect is in the right direction
    dplyr::mutate(draw = ifelse(term == "mean_age", -draw, draw))


p1 <- nm1_distdraws |>
    ggplot(aes(
        x = draw,
        fill = term,
    )) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_slab(
        alpha = .8
    ) +
    scale_fill_manual(
        name = "Variable",
        labels = c("Neighbourhood\nAge", "Individual\nTurnover"),
        values = titpalette(2, o = c(2, 1))
    ) +
    titheme() +
    theme(
        aspect.ratio = .5,
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        y = "Density",
        title = "Effects of Neighbourhood Age and Individual Turnover",
        subtitle = "Total effect of individual turnover, direct effect of neighbourhood age"
    )

p2 <- nm1_distdraws |>
    ggplot(aes(x = draw, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_pointinterval(
        alpha = .7,
        point_size = 4,
        interval_size_range = c(0.8, 2),
        position = position_dodge(width = .2, preserve = "single"),
        show.legend = FALSE
    ) +
    titheme() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(
            margin =
                margin(t = 10, r = 0, b = 0, l = 0)
        ),
        aspect.ratio = .1, # remove y axis text:
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
        x = "Change in Cultural Turnover",
    ) +
    scale_color_manual(values = titpalette(2, o = c(2, 1)))

fullplot <- ((p1 / p2) &
    guides(fill = guide_legend(byrow = TRUE, title.position = "top"))) &
    coord_cartesian(xlim = c(-0.04, 0.06)) &
    guides(
        colour = "none",
        fill = guide_legend(
            title = "Variable",
            byrow = TRUE, title.position = "top"
        )
    ) &
    theme(legend.spacing.y = unit(.2, "cm"))

ggsave(
    file.path(config$path$figures, "age_individual_turn_margeffs.png"),
    plot = fullplot,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)






# PLOT LEVELS OF TURNOVER AT THE NEIGHBOURHOOD LEVEL -----------------------


nf0spat <- brms::bf(
    prop_shared ~ 1 + gp(x, y, by = year, k = 30, c = 5 / 4, scale = FALSE),
    hu ~ 1
)

nm0spat <- brms::brm(
    nf0spat,
    data = nm_data_std,
    family = brms::hurdle_lognormal(),
    iter = 1000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(2),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nm0spat"),
    file_refit = "on_change",
)


resolution <- 25
ndraws <- 1000
type <- "estimate" # or "se"

nm0spat_preds <- get_spatial_preds(nm0spat, nm_data_std, resolution, ndraws)

# Palette
magma_centre <- viridis::viridis_pal(option = "magma")(20)[10]
magma_centre <- colorspace::desaturate(magma_centre, 0.5)

plist <- list()

for (year in unique(nm0spat_preds$year)) {
    nm0spat_rast <- get_raster(
        nm0spat_preds, pop_contour, year, type, resolution,
        fact = 4
    )
    crange <- nm0spat_rast |>
        dplyr::select(!!type) |>
        range()
    lims <- seq(crange[1], crange[2], length.out = 4)
    limlabs <- lims |> round(3)

    # 1- nm0spat_rast (so it represents change, more intuitive)
    nm0spat_rast[type] <- 1 - nm0spat_rast[type]

    plist[[year]] <-
        ggplot() +
        geom_raster(data = nm0spat_rast, aes(x = x, y = y, fill = !!sym(type))) +
        geom_sf(
            data = pop_contour_sf, fill = NA, linewidth = .5, colour = magma_centre
        ) +
        scale_fill_gradientn(
            colours = viridis::viridis_pal(option = "magma")(50),
            # labels every two second decimal places
            breaks = lims,
            labels = limlabs,
        ) +
        theme_void() +
        titheme() +
        labs(
            fill = "", title = year,
            x = "", y = ""
        ) +
        guides(fill = guide_colorbar(ticks.colour = NA)) +
        theme(
            # remove legend title
            legend.title = ggplot2::element_blank(),
            legend.key.width = ggplot2::unit(0.5, "cm"),
            legend.background =
                ggplot2::element_rect(fill = "transparent", color = NA),
            legend.box.background =
                ggplot2::element_rect(fill = "transparent", color = NA),
            # title inside rigth corner of plot
            plot.title = ggplot2::element_text(
                hjust = .95, vjust = 1, margin = ggplot2::margin(t = 10, b = -20, r = 10)
            )
        )
}


# combine plots and collect legend
p <- patchwork::wrap_plots(plist, align = "v") +
    # add a title and subtitle
    patchwork::plot_annotation(
        title = "Song Cultural Turnover across the Population",
        subtitle = paste(
            "Posterior spatial predictions of rates of cultural turnover"
        ),
        theme = titheme()
    )

ggsave(
    file.path(config$path$figures, "nm0spat_map_both.png"),
    p,
    width = 10, height = 5, dpi = 300
)


# Now the average map, it's too many figures already


nm0spat_rast <- nm0spat_preds |>
    # group year and get the mean estimate and se for each pair of unique x,y
    dplyr::group_by(x, y) |>
    dplyr::summarise(
        estimate = mean(estimate),
        se = mean(se)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(x, y, !!type) |>
    terra::rast(type = "xyz", crs = terra::crs(pop_contour)) |>
    terra::disagg(fact = 4, method = "bilinear") |>
    terra::mask(pop_contour) |>
    terra::as.data.frame(xy = TRUE) |>
    dplyr::as_tibble()


# 1- nm0spat_rast (so it represents change, more intuitive)
nm0spat_rast[type] <- 1 - nm0spat_rast[type]

crange <- nm0spat_rast |>
    dplyr::select(!!type) |>
    range()
lims <- seq(crange[2], crange[1], length.out = 4)
limlabs <- lims |> round(2)

nm0plot_all <-
    ggplot() +
    geom_raster(data = nm0spat_rast, aes(x = x, y = y, fill = !!sym(type))) +
    geom_sf(
        data = pop_contour_sf, fill = NA, linewidth = .5, colour = magma_centre
    ) +
    scale_fill_gradientn(
        colours = viridis::viridis_pal(option = "magma")(50),
        breaks = lims,
        labels = limlabs,
    ) +
    theme_void() +
    titheme() +
    labs(
        fill = "Cultural Turnover",
        x = "",
        y = ""
    ) +
    guides(fill = guide_colorbar(
        ticks.colour = NA,
        barwidth = 5, barheight = .5,
        # title below bar
        title.position = "bottom"
    )) +
    theme(
        # add a margin to the right
        plot.margin = ggplot2::margin(r = 10),
        # remove legend title
        # legend horizontal below the plot
        legend.position = c(0.5, -.2),
        legend.direction = "horizontal",
        # legend title below legend
        legend.background =
            ggplot2::element_rect(fill = "transparent", color = NA),
        legend.box.background =
            ggplot2::element_rect(fill = "transparent", color = NA)
    )


ggsave(
    file.path(config$path$figures, "nm0spat_map_mean.png"),
    nm0plot_all,
    width = 5, height = 5, dpi = 300, bg = "white"
)




# COMBINE AGE, TURNOVER AND SPATIAL PLOTS ----------------------------------


turnover_main <- (nm0plot_all) +
    (nm0.1plot2 + labs(
        x = "Individual\nTurnover"
    )) +
    (nm1plot1 +
        labs(x = "Mean Neigh.\nAge") +
        theme(
            # remove y axis text and title
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
        )) + plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 10))


# & patchwork::plot_annotation(
#     title = "Song Cultural Turnover across the Population",
#     subtitle = "Posterior spatial predictions for rates of cultural turnover", theme = titheme()
# )


ggsave(
    file.path(config$path$figures, "nm1_turnover_main.png"),
    turnover_main,
    width = 6, height = 3.5, dpi = 300, bg = "white"
)
