# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
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
