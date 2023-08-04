# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))

box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(mef = marginaleffects)
box::use(patchwork[...])
box::use(ggraph[...]) # ned to load this because of #ggraph/issues/75


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
# remove the 4 smallest polygons in the spatvector
pop_contour_sf <- pop_contour |> sf::st_as_sf()


# MAIN ───────────────────────────────────────────────────────────────────── #


# calculate sharing index for each pair of birds, add coordinates,
# and keep only relevant cols

pwise_df <- pwise_df |>
    dplyr::mutate(
        sharing_index = (2 * shared) / total
    ) |>
    dplyr::left_join(
        main_data |>
            dplyr::select(pnum, x, y),
        by = c("pnum" = "pnum")
    ) |>
    dplyr::filter(
        pnum != pnum2, father != father2, year == year2, nest_distance < 500
    )


# get the average sharing index for each pnum
pwise_df <- pwise_df |>
    dplyr::group_by(pnum) |>
    dplyr::summarise(
        sharing_index = mean(sharing_index, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    # remove duplicate pnums
    dplyr::distinct(pnum, .keep_all = TRUE)


# join this to main_data and plot this sharing index over the map
main_data_map <- main_data |>
    dplyr::left_join(
        pwise_df,
        by = c("pnum" = "pnum")
    ) |>
    dplyr::filter(!is.na(sharing_index))

# plot the variable 'sharing_index' from main_data over the pop_contour map
# with the nestboxes as points
pop_contour_sf |>
    ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(crs = 27700) +
    ggplot2::geom_point(
        data = main_data_map |>
            dplyr::filter(year == 2022),
        ggplot2::aes(x = x, y = y, fill = sharing_index),
        size = 6,
        shape = 21,
    ) +
    theme_frame(text_size = 12) +
    # add a diverging colour scale
    ggplot2::scale_fill_distiller(
        palette = "RdBu", direction = 1, name = "Sharing index"
    )

# Create graph object and layout
nodes <- pwise_df_year |>
    dplyr::select(pnum, x, y) |>
    dplyr::distinct() |>
    dplyr::rename(id = pnum)
edges <- pwise_df_year |>
    dplyr::select(pnum, pnum2, sharing_index) |>
    dplyr::distinct() |>
    dplyr::rename(from = pnum, to = pnum2)
graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
layout <- ggraph::create_layout(graph, "manual", x = nodes$x, y = nodes$y)
graph <- graph |>
    tidygraph::activate(edges)


# basic community detection
communities <- tidygraph::as_tbl_graph(graph) |>
    tidygraph::activate(nodes) |>
    igraph::cluster_louvain() |>
    igraph::membership()
dlay <- layout |>
    dplyr::as_tibble() |>
    dplyr::mutate(communities = communities)

rgb4 <- c("#f75435", "#684c41", "#4fa3a5", "#fdae38")
colouramp <- scales::colour_ramp(c("#ffffff", "#000000"))
edgecols <- colouramp(seq(0, 1, length = 10))


# plot the graph with the communities
networkmap <- ggraph::ggraph(layout) +
    ggplot2::geom_sf(data = pop_contour_sf, lwd = 0, fill = "#435c69") +
    ggraph::geom_edge_diagonal(ggplot2::aes(
        width = sharing_index, alpha = ggplot2::after_stat(index),
        color = sharing_index
    )) +
    ggraph::scale_edge_width_continuous(range = c(0.01, 1.5)) +
    ggraph::scale_edge_color_gradientn(
        colors = edgecols
    ) +
    ggraph::scale_edge_alpha(range = c(0, .15)) +
    ggplot2::geom_point(
        data = dlay,
        ggplot2::aes(x = x, y = y, color = as.factor(communities)),
        size = 2.5,
        alpha = 0.9,
    ) +
    titheme() +
    ggplot2::scale_color_manual(values = rgb4) +
    ggplot2::labs(
        color = "Community", y = "Northing", x = "Easting",
        edge_width = "Sharing Index"
    ) +
    ggplot2::guides(
        edge_alpha = "none", edge_color = "none",
        color = ggplot2::guide_legend(order = 1)
    )

ggplot2::ggsave(
    file.path(config$path$figures, "map_communities.png"),
    plot = networkmap,
    width = 6, height = 6, dpi = 300
)



# plot the variable 'repertoire_size' from main_data over the pop_contour map
# with the nestboxes as points

main_data_map <- main_data |>
    dplyr::filter(year == 2022) |>
    dplyr::select(pnum, x, y, repertoire_size) |>
    dplyr::filter(!is.na(repertoire_size))

pop_contour_sf |>
    ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(crs = 27700) +
    ggplot2::geom_point(
        data = main_data_map,
        ggplot2::aes(x = x, y = y, fill = repertoire_size),
        size = 6,
        shape = 21,
    ) +
    theme_frame(text_size = 12) +
    # add a diverging colour scale
    ggplot2::scale_fill_distiller(
        palette = "RdBu", direction = 1, name = "Repertoire size"
    )


# DEFINE AND FIT THE MODEL ───────────────────────────────────────────────── #


df1 <- brms::bf(
    mean_dist1 ~ 1 + natal_distance * nest_distance * year_born_diff +
        year + (1 | mm(father, father2))
)

dm1 <- brms::brm(
    df1,
    data = dm1_data_std,
    family = gaussian(),
    iter = 2000,
    warmup = 500,
    chains = 4,
    cores = 4,
    threads = brms::threading(4),
    backend = "cmdstanr",
    file = file.path(config$path$fits, "sacascascascas"),
    file_refit = "on_change",
)
