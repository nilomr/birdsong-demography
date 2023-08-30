# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# load song_sharing_data
sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "similarity_data.csv")
)


main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

# PREPARE FULL DYADIC DATA ───────────────────────────────────────────────── #

# Need:
# - Distance between boxes
# - Distance in time
# - Distance between natal boxes
# - Distance in time between

# get all the rows in main_data with unique nestbox
nestbox_data <- main_data |>
    dplyr::select(nestbox, x, y) |>
    dplyr::distinct()


# Find pnums in sharing_data that are not in main_data
setdiff(sharing_data$pnum, main_data$pnum)

# calculate all the pairwise distances between nestboxes
nestbox_distances <- round(stats::dist(nestbox_data[, 2:3]), 2)
# convert to a matrix with rownames and colnames
nestbox_distances <- as.matrix(nestbox_distances)
rownames(nestbox_distances) <- nestbox_data$nestbox
colnames(nestbox_distances) <- nestbox_data$nestbox


# add nestboxes to sharing_data
sharing_data <- sharing_data |>
    dplyr::mutate(
        nestbox = stringr::str_sub(pnum, 6),
        nestbox2 = stringr::str_sub(pnum2, 6)
    )

# get the distance between each pair of (nestbox, nestbox2) from nestbox_distances
# that is present in sharing_data and add it to sharing_data
sharing_data <- sharing_data |>
    dplyr::mutate(
        nest_distance = nestbox_distances[cbind(nestbox, nestbox2)],
        year = as.numeric(stringr::str_sub(pnum, 1, 4)),
        year2 = as.numeric(stringr::str_sub(pnum2, 1, 4))
    )


# add the data from main_data to sharing_data, matching pnum to pnum and pnum2 to pnum:
sharing_data <- sharing_data |>
    dplyr::select(-c(year, nestbox)) |>
    dplyr::left_join(main_data,
        by = c("pnum" = "pnum"),
        relationship = "many-to-many"
    ) |>
    dplyr::left_join(main_data,
        by = c("pnum2" = "pnum"),
        suffix = c("", "2"),
        relationship = "many-to-many"
    )

# add the distance between natal boxes
sharing_data <- sharing_data |>
    dplyr::mutate(
        natal_distance = nestbox_distances[cbind(natal_box, natal_box2)]
    ) |>
    dplyr::select(natal_box, natal_box2, natal_distance, dplyr::everything())

# Add resident status of the dyad
sharing_data <- sharing_data |>
    dplyr::mutate(resident_status = dplyr::case_when(
        resident == TRUE & resident2 == TRUE ~ "Both",
        resident == TRUE & resident2 == FALSE ~ "One",
        resident == FALSE & resident2 == TRUE ~ "One",
        resident == FALSE & resident2 == FALSE ~ "Neither"
    ))

# Calculate the age difference between the two birds (within year, overall)
sharing_data <- sharing_data |>
    dplyr::mutate(
        age = as.numeric(age),
        age2 = as.numeric(age2)
    ) |>
    dplyr::mutate(age_difference = abs(age - age2)) |>
    dplyr::mutate(year_born_diff = abs(year_born - year_born2)) |>
    # Recode age difference
    dplyr::mutate(
        year_born_diff = dplyr::case_when(
            year_born_diff == 0 ~ "0",
            year_born_diff == 1 ~ "1",
            year_born_diff == 2 ~ "2",
            year_born_diff == 3 ~ "3",
            year_born_diff >= 4 ~ "4+",
        )
    )


# Clean and order columns
sharing_data <- sharing_data |>
    dplyr::select(
        pnum, pnum2, mean_dist1, mean_dist2, year, year2, nestbox, nestbox2,
        nest_distance, father, father2,
        natal_box, natal_box2, natal_distance, year_born, year_born2, year_born_diff,
        age, age2, age_difference, resident, resident2, resident_status,
        dispersal_distance, dispersal_distance2, n_vocalisations, n_vocalisations2,
        repertoire_size, repertoire_size2
    )

# SAVE TO CSV ────────────────────────────────────────────────────────────── #

readr::write_csv(sharing_data, file.path(
    config$path$derived_data,
    "cont_pairwise_data.csv"
))
