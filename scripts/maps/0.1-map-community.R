# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))

box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(patchwork[...])
box::use(ggraph[...]) # need bc/ #ggraph/issues/75
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

pop_contour_sf <- pop_contour |>
    sf::st_as_sf()


# MAIN ───────────────────────────────────────────────────────────────────── #


# calculate sharing index for each pair of birds, add coordinates,
# and keep only relevant cols

pwise_df_year <- pwise_df |>
    dplyr::mutate(
        sharing_index = (2 * shared) / total
    ) |>
    dplyr::left_join(
        main_data |>
            dplyr::select(pnum, x, y),
        by = c("pnum" = "pnum")
    ) |>
    dplyr::filter(
        sharing_index > 0, year == 2022, year2 == 2022
    ) |>
    dplyr::select(pnum, pnum2, sharing_index, x, y, nest_distance) |>
    # remove any rows in nest_distance that are NA
    dplyr::filter(!is.na(nest_distance))



# self-comparisons are missing in pwise_df_year; add them, with sharing_index = 1
pwise_df_year <- pwise_df_year |>
    dplyr::bind_rows(
        dplyr::tibble(
            pnum = pwise_df_year$pnum,
            pnum2 = pwise_df_year$pnum,
            sharing_index = 1,
            x = pwise_df_year$x,
            y = pwise_df_year$y
        )
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(pnum, pnum2) |>
    # if pnum == pnum2, then nest_distance = 0
    dplyr::mutate(
        nest_distance = ifelse(pnum == pnum2, 0, nest_distance)
    )

# Pivot the dataframe to wide format
pwise_mat <- pwise_df_year |>
    dplyr::select(pnum, pnum2, sharing_index) |>
    tidyr::pivot_wider(names_from = pnum2, values_from = sharing_index) |>
    dplyr::select(-pnum) |>
    as.matrix()


spat_mat <- pwise_df_year |>
    dplyr::select(pnum, pnum2, nest_distance) |>
    tidyr::pivot_wider(names_from = pnum2, values_from = nest_distance) |>
    dplyr::select(-pnum) |>
    as.matrix()

# NAs to 0
pwise_mat[is.na(pwise_mat)] <- 0
spat_mat[is.na(spat_mat)] <- 0

# Set the rownames and colnames of the matrices
names <- unique(pwise_df_year$pnum)
rownames(pwise_mat) <- names
colnames(pwise_mat) <- names
colnames(spat_mat) <- names
rownames(spat_mat) <- names


# Convert the matrices to a distance object
pwise_dist <- as.dist(1 - pwise_mat)
spat_dist <- as.dist(spat_mat)


K <- 3
range.alpha <- seq(0, 1, 0.1)
cr <- ClustGeo::choicealpha(pwise_dist, spat_dist, range.alpha,
    K,
    graph = FALSE
)
cr$Q # proportion of explained inertia
plot(cr)

tree <- ClustGeo::hclustgeo(pwise_dist, alpha = 0.4)
P5 <- cutree(tree, K)


plot(tree,
    hang = -1, label = FALSE,
    xlab = "", sub = ""
)

rect.hclust(tree, k = K, border = c(4, 5, 3, 2, 1))




# get the unique pnums and the cluster they belong to from P5,
# and add the x and y coordinates from pwise_df_year, all into one df
spat5k <- dplyr::tibble(
    pnum = names(P5),
    cluster = P5
) |>
    dplyr::left_join(
        pwise_df_year |>
            dplyr::select(pnum, x, y),
        by = c("pnum" = "pnum")
    ) |>
    dplyr::distinct()


# convert spat5k to a terra spatvector
spat5k_sf <- spat5k |>
    sf::st_as_sf(coords = c("x", "y"), crs = 27700) |>
    terra::vect()


# calculate the voronoi polygons
spat5k_voronoi <- terra::voronoi(spat5k_sf, bnd = pop_contour)
spat5k_voronoi <- terra::crop(spat5k_voronoi, pop_contour)
vmap <- terra::project(spat5k_voronoi, "+init=epsg:27700") |>
    sf::st_as_sf()


# plot using ggplot
vplot2022 <- ggplot() +
    geom_sf(data = vmap, aes(fill = as.factor(cluster)), alpha = .9, color = "white") +
    geom_sf(data = pop_contour_sf, fill = NA, color = "#b9b9b9", linewidth = .8) +
    geom_point(
        data = spat5k,
        aes(x = x, y = y, color = as.factor(cluster)),
        size = 1
    ) +
    theme_bw() +
    titheme() +
    scale_color_manual(values = colorspace::darken(titpalette(3,
        order = c(2, 1, 3)
    ), .3)) +
    scale_fill_manual(values = titpalette(3, order = c(2, 1, 3))) +
    ggplot2::labs(
        fill = "Community", y = "Northing", x = "Easting",
        title = "Song-sharing across the population",
        subtitle = "K = 3 communities, 2022",
        edge_width = "Sharing Index"
    ) +
    ggplot2::guides(
        color = "none",
    ) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(margin = margin(5, 0, 0, 0))
    )

ggsave(
    file.path(config$path$figures, "k=3_comms_2022.png"),
    plot = vplot2022,
    bg = "transparent",
    device = svglite::svglite,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)


# COMMUNITY DETECTION ON NETWORK ─────────────────────────────────────────── #


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
    device = svglite::svglite,
    bg = "transparent",
    plot = networkmap,
    width = 6, height = 6, dpi = 300
)
