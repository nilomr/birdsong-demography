# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid, get_spatial_preds, get_raster])
box::use(patchwork[...])
box::use(ggraph[...]) # #ggraph/issues/75
box::use(ggplot2[...])


# FUNCTION DEFINITIONS ───────────────────────────────────────────────────── #


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# Load song_sharing_data
pwise_df <- read_csv_file(
    file.path(config$path$derived_data, "pairwise_data.csv")
)

main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$derived_data, "wytham_map", "perimeter.shp")
) |>
    terra::project("+init=epsg:27700")
pop_contour_sf <- pop_contour |> sf::st_as_sf()




# MAIN ───────────────────────────────────────────────────────────────────── #

# MODEL RARE SONGS ────────────────────────────────────────────────────────── #


nsm3_data <- main_data |>
    dplyr::filter(n_vocalisations > 0, year %in% c(2020:2022)) |>
    dplyr::select(year, pnum, rare_n, repertoire_size, x, y) |>
    # remove where rare_n OR X OR Y is NA
    dplyr::filter(!is.na(rare_n), !is.na(x), !is.na(y)) |>
    dplyr::mutate(year = as.factor(year))

nsf3 <- brms::bf(
    rare_n ~ 1 +
        gp(x, y, by = year, k = 25, c = 5 / 4, scale = FALSE) + year,
    decomp = "QR"
)

nsm3 <- brms::brm(
    nsf3,
    data = nsm3_data,
    family = poisson(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(4),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "nsm3"),
    file_refit = "never",
)


# PLOT SPATIAL DISTRIBUTION OF RARE SONG FREQUENCY ───────────────────────── #


resolution <- 25
ndraws <- 1000
type <- "estimate" # or "se"

nsm3_preds <- get_spatial_preds(nsm3, nsm3_data, resolution, ndraws)


# Palette
magma_centre <- viridis::viridis_pal(option = "magma")(20)[10]
magma_centre <- colorspace::desaturate(magma_centre, 0.5)

plist <- list()

for (year in unique(nsm3_preds$year)) {
    nsm3_rast <- get_raster(
        nsm3_preds, pop_contour, year, type, resolution,
        fact = 4
    )
    crange <- nsm3_rast |>
        dplyr::select(!!type) |>
        range()
    lims <- seq(crange[1], crange[2], length.out = 4)
    limlabs <- lims |> round(3)

    plist[[year]] <-
        ggplot() +
        geom_raster(data = nsm3_rast, aes(x = x, y = y, fill = !!sym(type))) +
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
        title = "Distribution of Rare Songs",
        subtitle = paste(
            "Posterior spatial predictions of the number of rare songs in bird's repertoires"
        ),
        theme = titheme()
    )

ggsave(
    file.path(config$path$figures, "nsm3_map.png"),
    device = svglite::svglite,
    p,
    width = 10, height = 5, dpi = 300
)
