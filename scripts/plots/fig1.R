# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])

# LOAD DATA ──────────────────────────────────────────────────────────────── #

# Load neighbourhood data
neighbour_df <- read_csv_file(
    file.path(config$path$derived_data, "neighbour_data.csv")
) |>
    dplyr::mutate(
        turnover = 1 - prop_same_birds
    )



# distributions of age, dispersal, immigration, turnover, cultural turnover, diversity

box::use(R / rplot[titheme, titpalette, reds, blues, yellows, persian])

fill_colors <- c(
    "mean_dispersal_distance" = blues[1],
    "prop_immigrant" = yellows[1],
    "mean_age" = reds[1],
    "turnover" = persian[1]
)

plot_vars <- c("mean_dispersal_distance", "prop_immigrant", "mean_age", "turnover")
plot_titles <- c("Mean Dispersal Distance", "Proportion of Immigrants", "Mean Age", "Proportion of Different Birds")

plots <- list()

for (i in seq_along(plot_vars)) {
    p <- neighbour_df |>
        ggplot(aes(x = !!sym(plot_vars[i]))) +
        geom_histogram(aes(y = after_stat(density)),
            position = "identity",
            bins = 20, alpha = 0.5,
            fill = fill_colors[plot_vars[i]],
            color = NA
        ) +
        geom_density(aes(y = after_stat(density)),
            fill = fill_colors[plot_vars[i]],
            color = fill_colors[plot_vars[i]],
            alpha = 0.5,
            bw = switch(plot_vars[i],
                "mean_dispersal_distance" = 100,
                "prop_immigrant" = 0.04,
                "mean_age" = 0.15,
                "turnover" = 0.05
            )
        ) +
        titheme() +
        theme(
            aspect.ratio = .6,
            panel.grid.major.x = element_line(color = "grey", linewidth = 0.5)
        ) +
        labs(title = plot_titles[i], x = plot_vars[i], y = "Density")

    plots[[i]] <- p
}

# join using patchwork
all <- (plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]) + plot_layout(ncol = 2, nrow = 2)


ggsave(
    filename = file.path(config$path$figures, "fig1_dists.svg"),
    plot = all,
    bg = "transparent",
    width = 12,
    device = svglite::svglite,
    height = 10,
    units = "cm",
    dpi = 300
)


# bar plot with sample sizes
