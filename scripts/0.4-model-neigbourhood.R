# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
source(file.path(config$path$source, "rplot.R"))

box::use(R / io[read_csv_file])


# LOAD DATA ──────────────────────────────────────────────────────────────── #

sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "cont_pairwise_data.csv")
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

# convert main_data_map to a spatial object and plot it
main_data_map_sf <- sf::st_as_sf(main_data_map, coords = c("x", "y"), crs = 27700)

# draw polygons around each 20 points in main_data_map_sf, without overlapping
main_data_map_sf |>
    sf::st_make_grid(
        cellsize = c(500, 500), what = "polygons", square = TRUE
    ) |>
    # c
    sf::st_intersection(pop_contour_sf) |>
    # convert to a dataframe
    sf::st_as_sf() |>
    # plot the polygons
    ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(crs = 27700) +
    theme_frame(text_size = 12) +
    # add the points from main_data_map_sf
    ggplot2::geom_sf(
        data = main_data_map_sf,
        ggplot2::aes(fill = repertoire_size),
        size = 6,
        shape = 21,
    )



#------------------#


n_radius <- 200
# first, prepare main_data so we can get neighbourhood estimates
# from all birds, not only those with song data

# Read in nestbox information
nestbox_data <- read_csv_file(file.path(config$path$data, "nestboxes.csv"))
# calculate all the pairwise distances between nestboxes
nestbox_distances <- round(stats::dist(nestbox_data[, 2:3]), 2)
# convert to a matrix with rownames and colnames
nestbox_distances <- as.matrix(nestbox_distances)
rownames(nestbox_distances) <- nestbox_data$nestbox
colnames(nestbox_distances) <- nestbox_data$nestbox

nest_rm <- main_data$nestbox[!main_data$nestbox %in% nestbox_data$nestbox]

neigh_data <- main_data |>
    dplyr::filter(year %in% c(2020, 2021, 2022)) |>
    dplyr::select(pnum, year, x, y, resident, ) |>
    # create all pairwise combinations of pnums, except self-self
    tidyr::expand(pnum, pnum2 = pnum) |>
    # now add the data for each pnum
    dplyr::left_join(main_data, by = c("pnum" = "pnum")) |>
    dplyr::left_join(main_data,
        by = c("pnum2" = "pnum"),
        suffix = c("", "2")
    ) |>
    # remove rows where nestbox or nestbox2 is in nest_rm
    dplyr::filter(!nestbox %in% nest_rm, !nestbox2 %in% nest_rm) |>
    dplyr::mutate(
        nest_distance = nestbox_distances[cbind(nestbox, nestbox2)]
    ) |>
    # Do this within years
    dplyr::filter(year == year2, nest_distance < n_radius) |>
    # count how many birds are within 200m for each pnum
    dplyr::group_by(pnum) |>
    dplyr::mutate(
        # age and age2 to numeric
        age = as.numeric(age),
        age2 = as.numeric(age2),
        neighbours = dplyr::n(),
        # count how many of the neighbours have n_vocalisations > 0
        recorded = sum(n_vocalisations2 > 0, na.rm = TRUE),
        # calculate the mean distance with boxes within 200m:
        mean_spat_dist = mean(nest_distance, na.rm = TRUE),
        # calculate the proportion of neighbours that are resident
        prop_resident = sum(resident2 == TRUE, na.rm = TRUE) / dplyr::n(),
        # calculate the mean dispersal distance of neighbours
        mean_dispersal_distance = mean(dispersal_distance2, na.rm = TRUE),
        # calculate the mean age of neighbours
        mean_age = mean(age2, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
        pnum, pnum2, year, year2, neighbours, recorded,
        mean_spat_dist, prop_resident, mean_dispersal_distance, mean_age
    ) |>
    # remove duplicates
    dplyr::distinct(pnum, .keep_all = TRUE)


neigh_data

sharing_data |>
    dplyr::group_by(pnum) |>
    dplyr::filter(!is.na(father), !is.na(father2)) |>
    dplyr::filter(year == year2, nest_distance < n_radius) |>
    # calculate mean song similarity based on average of mean_dist1 and mean_dist2
    dplyr::mutate(
        song_similarity = mean(mean_dist1, na.rm = TRUE),
        sample = dplyr::n()
    ) |>
    dplyr::distinct(pnum, .keep_all = TRUE) |>
    # now add the data from neigh_data
    dplyr::left_join(neigh_data, by = c("pnum" = "pnum")) |>
    # plot song_similarity against mean_spat_dist
    ggplot2::ggplot(ggplot2::aes(x = mean_age, y = song_similarity)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess")

# calculate the mean distance with boxes within 200m:

# for each pnum, get the proportion of pnum2 that are resident (resident column)
dplyr::group_by(pnum) |>
    dplyr::filter(!is.na(father), !is.na(father2)) |>
    dplyr::mutate(
        resident = sum(resident_status2 == "resident") / dplyr::n(),
        # get the proportion of pnum2 that are the same as last year
        same_as_last_year = sum(labels1 == labels2) / dplyr::n()
    )
