# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))
box::use(R / io[read_csv_file])

# CALCULATE POST-NATAL DISPERSAL DISTANCES FOR RESIDENT BIRDS ────────────── #

# Read in the data
main_data <- read_csv_file(file.path(config$path$data, "main.csv")) |>
    dplyr::select(-c(
        laying_rate, incubation_started, num_broken_eggs, num_dead_chicks,
        incubation_duration, lay_date_visit_interval, num_eggs_weighed,
        num_live_chicks, missing_entire_brood, suspected_predation
    )) |>
    dplyr::mutate(year = as.factor(year))

# Read in nestbox information
nestbox_data <- read_csv_file(file.path(config$path$data, "nestboxes.csv"))

# Calculate postanatal dispersal distances (straight line)
natal_male <- main_data |>
    dplyr::select(nestbox, natal_box, father) |>
    dplyr::filter(!is.na(natal_box))

postn_disp <- natal_male |>
    dplyr::left_join(nestbox_data,
        by = c("nestbox" = "nestbox"),
        relationship = "many-to-many"
    ) |>
    dplyr::left_join(nestbox_data,
        by = c("natal_box" = "nestbox"),
        relationship = "many-to-many"
    ) |>
    dplyr::select(nestbox, natal_box, father, x.x, y.x, x.y, y.y) |>
    dplyr::mutate(dispersal_distance = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) |>
    dplyr::rename(x = x.x, y = y.x, natal_x = x.y, natal_y = y.y)

# Add the distance to the original data
main_data <- main_data |>
    dplyr::left_join(postn_disp,
        by = c("nestbox", "father", "natal_box", "x", "y"),
        relationship = "many-to-many"
    ) |>
    dplyr::distinct(pnum, .keep_all = TRUE)


# SAVE THE DATA TO A CSV FILE ────────────────────────────────────────────── #

readr::write_csv(main_data, file.path(config$path$derived_data, "main.csv"))


# CLEAN WYTHAM CONTOUR MAP ───────────────────────────────────────────────── #


# read shapefile
pop_contour <- terra::vect(
    file.path(config$path$data, "wytham_map", "perimeter.shp")
) |>
    terra::project("+init=epsg:27700")
# remove the 4 smallest polygons in the spatvector
pop_contour_sf <- pop_contour |> sf::st_as_sf()
pop_contour <- pop_contour_sf[which.max(sf::st_area(pop_contour_sf)), ]

# save the data to a shapefile
base_dir <- file.path(config$path$derived_data, "wytham_map")
# make base directory if it doesn't exist in a one liner
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
sf::st_write(pop_contour, file.path(
    config$path$derived_data, "wytham_map", "perimeter.shp"
), append = FALSE)



# PLOTS ──────────────────────────────────────────────────────────────────── #

### Plot the distribution of dispersal distances

main_data |>
    # plot a histogram with the distribution of distances
    ggplot2::ggplot(ggplot2::aes(x = dispersal_distance)) +
    ggplot2::geom_histogram(bins = 100) +
    ggplot2::labs(x = "Distance (m)", y = "Count") +
    # add more x-axis ticks
    ggplot2::scale_x_continuous(breaks = seq(0, 3500, 500)) +
    theme_frame(text_size = 12)

# Plot the centred distribution of distances for each father
main_data |>
    dplyr::group_by(father) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::filter(n > 1) |>
    # ungroup and select the father and dispersal distance columns
    dplyr::ungroup() |>
    dplyr::select(father, dispersal_distance, n) |>
    dplyr::filter(!is.na(dispersal_distance), n > 1) |>
    # 0-centre the dispersal distances for each father
    dplyr::group_by(father) |>
    dplyr::mutate(
        dispersal_distance = dispersal_distance - mean(dispersal_distance)
    ) |>
    dplyr::ungroup() |>
    # plot a histogram with the distribution of distances
    ggplot2::ggplot(ggplot2::aes(x = dispersal_distance)) +
    ggplot2::geom_histogram(bins = 100) +
    ggplot2::labs(x = "Distance (m)", y = "Count")



## Calculate voronoi neighbors for each nestbox, by year
### Plot map to check all is ok


# TODO: review - remove or integrate


# pop_contour |>
#     ggplot2::ggplot() +
#     ggplot2::geom_sf() +
#     ggplot2::coord_sf(crs = 27700) +
#     ggplot2::geom_point(
#         data = nestbox_data,
#         ggplot2::aes(x = x, y = y),
#         size = 0.5,
#         shape = 21,
#         fill = "black",
#         colour = "black"
#     ) +
#     theme_frame(text_size = 12)



# sf_main_data <- main_data |>
#     dplyr::filter(year %in% c(2020)) |>
#     dplyr::filter(!is.na(x) & !is.na(y)) |>
#     sf::st_as_sf(coords = c("x", "y"), remove = F, crs = 27700)

# territories <- sf::st_voronoi(sf::st_union(sf_main_data))
# territories <- sf::st_intersection(
#     sf::st_cast(territories),
#     sf::st_union(pop_contour)
# )

# # Add info on territory size
# territories_area <- sf::st_sf(
#     territory_area = as.numeric(sf::st_area(territories)),
#     geom = territories
# )
# birds.ids <- sf_main_data[, c(1, 2, 3, 4)]
# territories_area <- sf::st_join(territories_area, sf_main_data)


# # Find first order neighbors
# neighbors <- sf::st_touches(territories_area, territories_area)

# # Add neighbors to territories_area
# territories_area$neighbors <- apply(neighbors, 1, function(x) {
#     paste(territories_area$pnum[x], collapse = ", ")
# })
