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

# Levels for the ribbon
levels <- 30 # increase for final plots

# ──── LOAD DATA ──────────────────────────────────────────────────────────────

div_data <- readRDS(file.path(config$path$fits, "div_data.rds"))
nov_m_1 <- readRDS(file.path(config$path$fits, "nov_m_1.rds"))
div_m_1 <- readRDS(file.path(config$path$fits, "div_m_1.rds"))
nov_m_2 <- readRDS(file.path(config$path$fits, "nov_m_2.rds"))
nov_m_2.1 <- readRDS(file.path(config$path$fits, "nov_m_2.1.rds"))
div_m_2 <- readRDS(file.path(config$path$fits, "div_m_2.rds"))
div_m_2.1 <- readRDS(file.path(config$path$fits, "div_m_2.1.rds"))


# PREPARE THE DATA ───────────────────────────────────────────────────────── #


# Calculate marginal effects of models 1 and 2 and bind them together
vars <- c(
    "mean_dispersal_distance",
    "prop_immigrant",
    "mean_age",
    "prop_same_birds"
)

nov_m_12_margeffs_mefs <- dplyr::bind_rows(
    calc_avg_slopes(nov_m_1, vars),
    calc_avg_slopes(div_m_1, vars)
)

# Calculate sampling effect on diversity and novelty and bind them together
nov_m_2_mefs <- dplyr::bind_rows(
    calc_avg_slopes(nov_m_2, "diversity"),
    calc_avg_slopes(nov_m_2.1, "diversity")
)


# calculate predictions and partial residuals for our variables of interest

# Effect of dispersal distance on cultural novelty

nov_m_1_gr <- marginaleffects::datagrid(
    model = nov_m_1,
    mean_dispersal_distance = nov_m_1$data$mean_dispersal_distance
)

nov_m_1_gr_2 <- partial_residuals(nov_m_1, nov_m_1_gr)

nov_m_1_pred <- marginaleffects::predictions(nov_m_1,
    type = "response",
    re_formula = NULL,
    ndraws = 500,
    newdata = marginaleffects::datagrid(
        mean_dispersal_distance = seq(
            min(nov_m_1$data$mean_dispersal_distance),
            max(nov_m_1$data$mean_dispersal_distance),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
nov_m_1_gr_2 <- og_scale(div_data, nov_m_1_gr_2, v = "mean_dispersal_distance")
nov_m_1_pred <- og_scale(div_data, nov_m_1_pred, v = "mean_dispersal_distance")



# Effect of age on cultural diversity (div_m_1)

div_m_1_gr <- marginaleffects::datagrid(
    model = div_m_1,
    mean_age = div_m_1$data$mean_age
)

div_m_1_gr_2 <- partial_residuals(div_m_1, div_m_1_gr)
div_m_1_pred <- marginaleffects::predictions(div_m_1,
    type = "response",
    re_formula = NULL,
    ndraws = 500,
    newdata = marginaleffects::datagrid(
        mean_age = seq(
            min(div_m_1$data$mean_age),
            max(div_m_1$data$mean_age),
            by = 0.1
        )
    )
) |>
    marginaleffects::posterior_draws()

# convert estimates back to the original scale
div_m_1_gr_2 <- og_scale(div_data, div_m_1_gr_2, v = "mean_age")
div_m_1_pred <- og_scale(div_data, div_m_1_pred, v = "mean_age")



# ──── BUILD FIGURES ──────────────────────────────────────────────────────────

# First plot: marginal effects of models 1 and 2

fill_colors <- c(
    "mean_dispersal_distance" = blues[1],
    "prop_immigrant" = yellows[1],
    "mean_age" = reds[1],
    "prop_same_birds" = persian[1]
)

nov_m_12_margeffs <- nov_m_12_margeffs_mefs |>
    ggplot(aes(x = draw, y = term, fill = term, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_halfeye(aes(slab_alpha = model)) +
    scale_fill_manual(
        values = fill_colors,
    ) +
    scale_color_manual(
        values = sapply(fill_colors, colorspace::darken, .2),
    ) +
    ggdist::scale_slab_alpha_discrete(
        range = c(.9, .4),
        labels = c("Cultural\nNovelty", "Cultural\nDiversity"),
        limits = c("nov_m_1", "div_m_1"),
        guide = guide_legend(
            byrow = TRUE, title.position = "top",
            keywidth = 0.8, keyheight = 0.8,
            title = "Outcome",
            override.aes = list(linetype = 0, shape = NA)
        )
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
    # set x axis limits to -0.03, 0.03
    scale_x_continuous(
        limits = c(-0.025, 0.03),
        breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02)
    ) +
    labs(x = "Marginal Effect", y = "") +
    guides(fill = "none", color = "none") +
    titheme() +
    theme(
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.999, 0.999),
        legend.justification = c(1, 1),
        legend.box.just = "right",
        aspect.ratio = 1,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(.2, "cm")
    )


# Second plot: predictions of the effect of dispersal distance on novelty


nov_m_1_disp <-
    nov_m_1_pred |>
    ggplot(aes(x = mean_dispersal_distance, y = draw)) +
    geom_point(
        data = nov_m_1_gr_2,
        aes(
            y = estimate_with_error
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
    guides(colour = "none") +
    scale_y_continuous(
        breaks = c(0.63, 0.68, 0.73, 0.78, 0.83)
    ) +
    scale_x_continuous(
        breaks = c(300, 900, 1500),
        labels = c("350m", "900m", "1500m")
    ) +
    labs(
        x = "x̄ Dispersal",
        y = "Cultural Novelty",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8,
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )

# Third plot: predictions of the effect of age on diversity

div_m_1_age <-
    div_m_1_pred |>
    ggplot(aes(x = mean_age, y = draw)) +
    geom_point(
        data = div_m_1_gr_2,
        aes(
            y = estimate_with_error
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
    guides(colour = "none") +
    scale_y_continuous(
        breaks = c(0.56, 0.60, 0.64, 0.68, 0.72)
    ) +
    scale_x_continuous(
        breaks = c(1, 1.5, 2, 2.5, 3),
        labels = c("1y", "1.5y", "2y", "2.5y", "3y")
    ) +
    labs(
        x = "x̄ Age",
        y = "Cultural Diversity",
    ) +
    guides(colour = "none") +
    titheme() +
    theme(
        aspect.ratio = 1.8
    )



# Spatial distribution of diversity (GP from model 2)

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


div_m_1_spp <- get_spatial_preds(div_m_1, div_m_1$data, resolution, ndraws)
div_m_1_spp <- div_m_1_spp |> # group by x and y and get mean of estimate and se
    dplyr::group_by(x, y) |>
    dplyr::summarise(
        estimate = mean(estimate),
        se = mean(se)
    )

div_m_1_rast <- get_raster(div_m_1_spp, pop_contour, NULL, type, resolution,
    fact = 4
)
crange <- div_m_1_rast |>
    dplyr::select(!!type) |>
    range()
lims <- seq(crange[1] + 0.02, crange[2] - 0.02, length.out = 3)
limlabs <- lims |> round(2)



div_m_1_spat <- ggplot() +
    geom_raster(data = div_m_1_rast, aes(x = x, y = y, fill = !!sym(type))) +
    geom_sf(
        data = pop_contour_sf, fill = NA, linewidth = .5, colour = pal_centre
    ) +
    scale_fill_gradientn(
        colours = palette,
        breaks = lims,
        labels = limlabs
    ) +
    labs(
        fill = "Cultural\nDiversity",
        x = "Longitude", y = "Latitude"
    ) +
    titheme() +
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
        limits = c(min(div_m_1_rast$x) - 200, max(div_m_1_rast$x) + 800),
    ) +
    theme(
        aspect.ratio = NULL,
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.95, 0.95),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 10),
        legend.direction = "horizontal",
        legend.justification = c(1, 1),
        legend.box.just = "right",
        legend.spacing.y = unit(.2, "cm")
    )


# join plots
diversity_all <- nov_m_12_margeffs + nov_m_1_disp + div_m_1_age + div_m_1_spat +
    plot_layout(ncol = 4) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 12, face = "bold"))

ggsave(
    file.path(config$path$figures, "diversity_all.svg"),
    device = svglite::svglite,
    diversity_all,
    width = 10, height = 4, dpi = 300, bg = "white"
)


# Fourth plot: sampling effect on diversity and novelty

dicolors <- c(
    "nov_m_2" = "#9bb34d",
    "nov_m_2.1" = "#939c74"
)

nov_m_2_plot <- nov_m_2_mefs |>
    ggplot(aes(x = draw, fill = model, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_halfeye(alpha = 0.9) +
    scale_color_manual(
        values = sapply(dicolors, colorspace::darken, .2),
    ) +
    scale_fill_manual(
        values = dicolors,
        labels = c("Controls for\nsampling", "Does not control\nfor sampling"),
        limits = c("nov_m_2", "nov_m_2.1"),
        guide = guide_legend(
            byrow = TRUE, title.position = "top",
            keywidth = 0.8, keyheight = 0.8,
            title = "Model",
            override.aes = list(linetype = 0, shape = NA)
        )
    ) +
    labs(x = "Marginal Effect of Diversity on Novelty", y = "") +
    titheme() +
    scale_y_continuous(expand = c(0.2, 0)) +
    theme(
        aspect.ratio = .4,
        legend.spacing.y = unit(.2, "cm")
    ) +
    guides(color = "none") +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 12, face = "bold"))

ggsave(
    file.path(config$path$figures, "supp_nov_m_2_plot.svg"),
    device = svglite::svglite,
    nov_m_2_plot,
    width = 5, height = 3.5, dpi = 300, bg = "white"
)



# First plot again using absolute cultural diversity

# Calculate MEMs for model 4 and 5


# Calculate marginal effects of models 1 and 2 and bind them together
vars <- c(
    "mean_dispersal_distance",
    "prop_immigrant",
    "mean_age",
    "prop_same_birds"
)

div_m_25_margeffs_mefs <- dplyr::bind_rows(
    calc_avg_slopes(div_m_2, vars),
    calc_avg_slopes(div_m_2.1, vars)
)


div_m_25_margeffs <- div_m_25_margeffs_mefs |>
    ggplot(aes(x = draw, y = term, fill = term, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_halfeye(aes(slab_alpha = model)) +
    scale_fill_manual(
        values = fill_colors,
    ) +
    scale_color_manual(
        values = sapply(fill_colors, colorspace::darken, .2),
    ) +
    ggdist::scale_slab_alpha_discrete(
        range = c(.9, .4),
        labels = c("Absolute\nCultural Diversity", "Adjusting for\nRepertoire Size"),
        limits = c("div_m_2", "div_m_2.1"),
        guide = guide_legend(
            byrow = TRUE, title.position = "top",
            keywidth = 0.8, keyheight = 0.8,
            title = "Model",
            override.aes = list(linetype = 0, shape = NA)
        )
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
    # # set x axis limits to -0.03, 0.03
    # scale_x_continuous(
    #     limits = c(-0.025, 0.03),
    #     breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02)
    # ) +
    labs(x = "Marginal Effect", y = "") +
    guides(fill = "none", color = "none") +
    titheme() +
    theme(
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.position = c(0.999, 0.999),
        legend.justification = c(1, 1),
        legend.box.just = "right",
        aspect.ratio = 1,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(.2, "cm")
    )

# save plot as an r object
saveRDS(div_m_25_margeffs, file.path(config$path$figures, "div_m_25_margeffs.rds"))
