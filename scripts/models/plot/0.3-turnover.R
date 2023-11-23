# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, blues, yellows, reds, persian])
box::use(R / utils[
    og_scale, match_grid, get_spatial_preds,
    get_raster, partial_residuals, calc_avg_slopes
])
box::use(patchwork[...])
box::use(ggplot2[...])
box::use(dplyr[...])

# Levels for the ribbon
levels <- 30 # increase for final plots

# ──── LOAD DATA ──────────────────────────────────────────────────────────────

turn_data <- readRDS(file.path(config$path$fits, "turn_data.rds"))
turn_m_1 <- readRDS(file.path(config$path$fits, "turn_m_1.rds"))
turn_m_2 <- readRDS(file.path(config$path$fits, "turn_m_2.rds"))


# PREPARE THE DATA ───────────────────────────────────────────────────────── #


# Calculate marginal effects of model 3
vars <- c(
    "mean_dispersal_distance",
    "prop_immigrant",
    "mean_age",
    "prop_same_birds"
)

turn_m_2_margeffs <-
    calc_avg_slopes(turn_m_2, vars) |>
    mutate(draw = ifelse(term == "prop_same_birds", draw, -draw))


# get posterior draws for conterfactual demographic scenarios

# Calculate predictions for minimum values
min_values <- marginaleffects::datagrid(
    model = turn_m_2,
    prop_immigrant = min(turn_m_2$data$prop_immigrant),
    mean_dispersal_distance = min(turn_m_2$data$mean_dispersal_distance),
    mean_age = min(turn_m_2$data$mean_age),
    prop_same_birds = mean(turn_m_2$data$prop_same_birds)
)
turn_m_2_cft_mi <- marginaleffects::predictions(turn_m_2, newdata = min_values)

# Calculate predictions for maximum values
max_values <- marginaleffects::datagrid(
    model = turn_m_2,
    prop_immigrant = max(turn_m_2$data$prop_immigrant),
    mean_dispersal_distance = max(turn_m_2$data$mean_dispersal_distance),
    mean_age = max(turn_m_2$data$mean_age),
    prop_same_birds = mean(turn_m_2$data$prop_same_birds)
)
turn_m_2_cft_ma <- marginaleffects::predictions(turn_m_2, newdata = max_values)

# Combine and process predictions
turn_m_2_cft <- bind_rows(
    turn_m_2_cft_mi |> marginaleffects::posterior_draws() |>
        mutate(condition = "min"),
    turn_m_2_cft_ma |>
        marginaleffects::posterior_draws() |>
        mutate(condition = "max")
) |>
    mutate(draw = 1 - draw)


# get estimates
turn_m_2_cft_mi |>
    as_tibble() |>
    bind_rows(as_tibble(turn_m_2_cft_ma)) |>
    # round to two decimal places
    mutate_at(
        vars(estimate, conf.low, conf.high),
        function(x) round(x, 2)
    )



# calculate predictions and partial residuals for our variables of interest

# Effect of dispersal distance on cultural turnover (turn_m_2)

turn_m_2_gr_dis <- marginaleffects::datagrid(
    model = turn_m_2,
    mean_dispersal_distance = turn_m_2$data$mean_dispersal_distance
)

turn_m_2_gr_dis_2 <- partial_residuals(turn_m_2, turn_m_2_gr_dis)

turn_m_2_pred_dis <- marginaleffects::predictions(turn_m_2,
    type = "response",
    re_formula = NULL,
    ndraws = 1000,
    newdata = marginaleffects::datagrid(
        mean_dispersal_distance = seq(
            min(turn_m_2$data$mean_dispersal_distance),
            max(turn_m_2$data$mean_dispersal_distance),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
turn_m_2_gr_dis_2 <- og_scale(turn_data, turn_m_2_gr_dis_2, v = "mean_dispersal_distance")
turn_m_2_pred_dis <- og_scale(turn_data, turn_m_2_pred_dis, v = "mean_dispersal_distance")



# Effect of immigration on cultural turnover (turn_m_2)

turn_m_2_gr_imm <- marginaleffects::datagrid(
    model = turn_m_2,
    prop_immigrant = turn_m_2$data$prop_immigrant
)

turn_m_2_gr_imm_2 <- partial_residuals(turn_m_2, turn_m_2_gr_imm)

turn_m_2_pred_imm <- marginaleffects::predictions(turn_m_2,
    type = "response",
    re_formula = NULL,
    ndraws = 1000,
    newdata = marginaleffects::datagrid(
        prop_immigrant = seq(
            min(turn_m_2$data$prop_immigrant),
            max(turn_m_2$data$prop_immigrant),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
turn_m_2_gr_imm_2 <- og_scale(turn_data, turn_m_2_gr_imm_2, v = "prop_immigrant")
turn_m_2_pred_imm <- og_scale(turn_data, turn_m_2_pred_imm, v = "prop_immigrant")


# Effect of age on cultural turnover (turn_m_2)

turn_m_2_gr_age <- marginaleffects::datagrid(
    model = turn_m_2,
    mean_age = turn_m_2$data$mean_age
)

turn_m_2_gr_age_2 <- partial_residuals(turn_m_2, turn_m_2_gr_age)

turn_m_2_pred_age <- marginaleffects::predictions(turn_m_2,
    type = "response",
    re_formula = NULL,
    ndraws = 1000,
    newdata = marginaleffects::datagrid(
        mean_age = seq(
            min(turn_m_2$data$mean_age),
            max(turn_m_2$data$mean_age),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
turn_m_2_gr_age_2 <- og_scale(turn_data, turn_m_2_gr_age_2, v = "mean_age")
turn_m_2_pred_age <- og_scale(turn_data, turn_m_2_pred_age, v = "mean_age")


# Effect of individual turnover on cultural turnover (turn_m_2)

turn_m_2_gr <- marginaleffects::datagrid(
    model = turn_m_2,
    prop_same_birds = turn_m_2$data$prop_same_birds
)

turn_m_2_gr_2 <- partial_residuals(turn_m_2, turn_m_2_gr)

turn_m_2_pred <- marginaleffects::predictions(turn_m_2,
    type = "response",
    re_formula = NULL,
    ndraws = 1000,
    newdata = marginaleffects::datagrid(
        prop_same_birds = seq(
            min(turn_m_2$data$prop_same_birds),
            max(turn_m_2$data$prop_same_birds),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
turn_m_2_gr_2 <- og_scale(turn_data, turn_m_2_gr_2, v = "prop_same_birds")
turn_m_2_pred <- og_scale(turn_data, turn_m_2_pred, v = "prop_same_birds")



# ──── BUILD FIGURES ──────────────────────────────────────────────────────────

# First plot: marginal effects of model 3

fill_colors <- c(
    "mean_dispersal_distance" = blues[1],
    "prop_immigrant" = yellows[1],
    "mean_age" = reds[1],
    "prop_same_birds" = persian[1]
)

turn_m_2_margeffs_plot <- turn_m_2_margeffs |>
    ggplot(aes(x = draw, y = term, fill = term, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_halfeye() +
    scale_fill_manual(
        values = fill_colors,
    ) +
    scale_color_manual(
        values = sapply(fill_colors, colorspace::darken, .2),
    ) +
    scale_y_discrete(
        limits = rev(vars),
        expand = c(0.1, 0.1),
        labels = c(
            "mean_dispersal_distance" = "Dispersal",
            "prop_immigrant" = "Immigration",
            "mean_age" = "Age",
            "prop_same_birds" = "Individual\nTurnover"
        )
    ) +
    annotate(
        geom = "text",
        x = 0, y = 5.35, label = "", size = 5
    ) +
    scale_x_continuous(
        breaks = c(-0.03, -0.015, 0, 0.015, 0.03),
        labels = c("-0.03", "-0.015", "0", "0.015", "0.03")
    ) +
    labs(x = "Marginal Effect", y = "") +
    guides(fill = "none", color = "none") +
    titheme() +
    theme(
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right",
        aspect.ratio = 1,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(.2, "cm")
    )


# Second plot: counterfactual turnover rates


cft_colors <- c("#da873b", "#367480")

turn_m_2_cft_plot <- turn_m_2_cft |>
    ggplot(aes(
        x = draw,
        fill = condition,
        color = condition
    )) +
    ggdist::stat_halfeye(
        alpha = .8
    ) +
    scale_fill_manual(
        values = cft_colors,
        labels = c("min" = "Min.", "max" = "Max."),
        guide = guide_legend(
            title = "Demographic\nScenario",
            byrow = TRUE, title.position = "top",
            keywidth = 0.8, keyheight = 0.8,
            override.aes = list(linetype = 0, shape = NA)
        )
    ) +
    scale_color_manual(values = colorspace::darken(cft_colors, .2)) +
    titheme() +
    theme(
        aspect.ratio = .35,
    ) +
    theme(
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(.05, "cm")
    ) +
    scale_y_continuous(
        expand = c(0.1, 0),
        breaks = c(0, 0.5, 1),
        labels = c("0", "0.5", "1")
    ) +
    scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("0", "0.25", "0.5", "0.75", "1"),
        limits = c(0, 1)
    ) +
    guides(
        colour = "none",
    ) +
    labs(
        y = "Density",
        x = "Cultural Turnover"
    )



# 4 plots: effect of each variable on cultural turnover

# Dispersal
turn_m_2_dis_plot <-
    turn_m_2_pred_dis |>
    ggplot(aes(x = mean_dispersal_distance, y = 1 - draw)) +
    geom_point(
        data = turn_m_2_gr_dis_2,
        aes(
            y = 1 - estimate_with_error
        ),
        alpha = 0.5,
        shape = 21,
        fill = blues[2],
        stroke = NA,
        size = 1
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = blues[1], fill = blues[1]
    ) +
    scale_x_continuous(
        breaks = c(350, 900, 1500),
        labels = c("350m", "900m", "1500m")
    ) +
    scale_y_continuous(
        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8),
        limits = c(0.38, 0.82)
    ) +
    guides(colour = "none") +
    labs(
        x = "x̄ Dispersal",
        y = "Cultural Turnover",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank()
    )

# Immigration


turn_m_2_imm_plot <-
    turn_m_2_pred_imm |>
    ggplot(aes(x = prop_immigrant, y = 1 - draw)) +
    geom_point(
        data = turn_m_2_gr_imm_2,
        aes(
            y = 1 - estimate_with_error
        ),
        alpha = 0.5,
        shape = 21,
        fill = yellows[2],
        stroke = NA,
        size = 1
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = yellows[1], fill = yellows[1]
    ) +
    scale_x_continuous(
        breaks = c(0, 0.3, 0.5, 0.7, 1)
    ) +
    scale_y_continuous(
        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8),
        limits = c(0.38, 0.82)
    ) +
    guides(colour = "none") +
    labs(
        x = "x̄ Immigration",
        y = "Cultural Turnover",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank()
    )

# Age

turn_m_2_age_plot <-
    turn_m_2_pred_age |>
    ggplot(aes(x = mean_age, y = 1 - draw)) +
    geom_point(
        data = turn_m_2_gr_age_2,
        aes(
            y = 1 - estimate_with_error
        ),
        alpha = 0.5,
        shape = 21,
        fill = reds[2],
        stroke = NA,
        size = 1
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = reds[1], fill = reds[1]
    ) +
    scale_x_continuous(
        breaks = c(1, 1.5, 2, 2.5),
        labels = c("1y", "1.5y", "2y", "2.5y")
    ) +
    scale_y_continuous(
        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8),
        limits = c(0.38, 0.82)
    ) +
    guides(colour = "none") +
    labs(
        x = "x̄ Age",
        y = "Cultural Turnover",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank()
    )


# Turnover

turn_m_2_turn_plot <-
    turn_m_2_pred |>
    ggplot(aes(x = 1 - prop_same_birds, y = 1 - draw)) +
    geom_point(
        data = turn_m_2_gr_2,
        aes(
            y = 1 - estimate_with_error
        ),
        alpha = 0.5,
        shape = 21,
        fill = persian[2],
        stroke = NA,
        size = 1
    ) +
    ggdist::stat_lineribbon(
        .width = ppoints(levels), alpha = 1 / levels,
        color = persian[1], fill = persian[1]
    ) +
    scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("0", "0.25", "0.5", "0.75", "1"),
    ) +
    scale_y_continuous(
        breaks = c(0.4, 0.5, 0.6, 0.7, 0.8),
        limits = c(0.38, 0.82)
    ) +
    guides(colour = "none") +
    labs(
        x = "Individual Turn.",
        y = "Cultural Turnover",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank()
    )





# Spatial distribution of turnover (GP from model 3)

# Settings and colours
resolution <- 50
ndraws <- 1000
type <- "estimate" # or "se"

palette <- scico::scico(30, palette = "lipari")
pal_centre <- palette[5]
pal_centre <- colorspace::desaturate(pal_centre, 0.5)

# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$derived_data, "wytham_map", "perimeter.shp")
) |>
    terra::project("+init=epsg:27700")
pop_contour_sf <- pop_contour |> sf::st_as_sf()


turn_m_2_spp <- get_spatial_preds(turn_m_2, turn_m_2$data, resolution, ndraws)
turn_m_2_spp <- turn_m_2_spp |>
    dplyr::group_by(x, y) |>
    dplyr::summarise(
        estimate = mean(estimate),
        se = mean(se)
    )

turn_m_2_rast <- get_raster(turn_m_2_spp, pop_contour, NULL, type, resolution,
    fact = 4
) |> dplyr::mutate(estimate = 1 - estimate)
crange <- turn_m_2_rast |>
    dplyr::select(!!type) |>
    range()
lims <- seq(crange[1] + 0.02, crange[2] - 0.02, length.out = 3)
limlabs <- lims |> round(2)



turn_m_2_spat <- ggplot() +
    geom_raster(data = turn_m_2_rast, aes(x = x, y = y, fill = !!sym(type))) +
    geom_sf(
        data = pop_contour_sf, fill = NA, linewidth = .3, colour = pal_centre
    ) +
    scale_fill_gradientn(
        colours = palette,
        breaks = lims,
        labels = limlabs
    ) +
    labs(
        fill = "Cultural\nTurnover",
        x = "Longitude", y = "Latitude"
    ) +
    guides(fill = guide_colorbar(
        ticks.colour = NA,
        barwidth = 3.5,
        barheight = .6,
        # title below bar
        title.position = "top"
    )) +
    # set x scale limits
    scale_x_continuous(
        expand = c(0, 0),
        limits = c(min(turn_m_2_rast$x) - 200, max(turn_m_2_rast$x) + 800),
    ) +
    titheme() +
    theme(
        aspect.ratio = NULL,
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.direction = "horizontal",
        legend.spacing.y = unit(.2, "cm")
    )

turnover_all <- (turn_m_2_margeffs_plot | turn_m_2_turn_plot |
    (turn_m_2_age_plot + theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
    )) | (turn_m_2_spat / turn_m_2_cft_plot)) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 12, face = "bold"))


ggsave(
    file.path(config$path$figures, "turnover_all.svg"),
    device = svglite::svglite,
    turnover_all,
    width = 10, height = 4, dpi = 300, bg = "white"
)
