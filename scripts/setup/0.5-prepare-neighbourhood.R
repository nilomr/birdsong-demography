# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])


# LOAD DATA ──────────────────────────────────────────────────────────────── #


# Load song_sharing_data
sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "pairwise_data.csv")
) |>
    dplyr::mutate(
        labels1 = stringr::str_split(labels1, ", "),
        labels2 = stringr::str_split(labels2, ", ")
    )

main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)

# MAIN ───────────────────────────────────────────────────────────────────── #


n_radius <- 200
# first, prepare main_data so we can get neighbourhood estimates
# from all birds, not only those with song data

# Read in nestbox information
nestbox_data <- read_csv_file(file.path(config$path$data, "nestboxes.csv"))
nestbox_distances <- round(stats::dist(nestbox_data[, 2:3]), 2)
nestbox_distances <- as.matrix(nestbox_distances)
rownames(nestbox_distances) <- nestbox_data$nestbox
colnames(nestbox_distances) <- nestbox_data$nestbox

nest_rm <- main_data$nestbox[!main_data$nestbox %in% nestbox_data$nestbox]


main_data <- main_data |>
    dplyr::filter(year %in% c(2019, 2020, 2021, 2022))


fake_rows <- tidyr::expand_grid(
    nestbox = unique(main_data$nestbox),
    year = 2019:2022
) |>
    dplyr::anti_join(main_data, by = c("nestbox", "year")) |>
    dplyr::mutate(
        pnum = paste0("empty_", year, "1", nestbox)
    )

main_data <- main_data |>
    dplyr::bind_rows(fake_rows) |>
    dplyr::arrange(nestbox, year)


neigh_data <- main_data |>
    dplyr::select(pnum, nestbox, year, x, y, resident) |>
    # create all pairwise combinations of boxes per year, except self-self
    tidyr::expand(pnum, pnum2 = pnum) |>
    # now add the data for each pnum
    dplyr::full_join(main_data, by = c("pnum" = "pnum")) |>
    dplyr::full_join(main_data,
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
    dplyr::mutate(nest_distance = ifelse(
        stringr::str_detect(pnum, "empty"),
        NA_real_, # nolint: error.
        nest_distance
    )) |>
    # count how many birds are within 200m for each pnum
    dplyr::group_by(pnum) |>
    dplyr::mutate(
        # age and age2 to numeric
        age = as.numeric(age),
        age2 = as.numeric(age2),
        # create a variable neighbour that counts how many neighbours there
        # are that dont have 'empty' in pnum:
        neighbours = sum(!stringr::str_detect(pnum2, "empty"), na.rm = TRUE),
        # count how many of the neighbours have n_vocalisations > 0
        recorded = sum(n_vocalisations2 > 0, na.rm = TRUE),
        # calculate the mean distance with boxes within 200m:
        mean_spat_dist = mean(nest_distance, na.rm = TRUE),
        mean_spat_dist_sd = sd(nest_distance, na.rm = TRUE),
        mean_spat_dist_recorded = mean(nest_distance[n_vocalisations2 > 0], na.rm = TRUE),
        # calculate the proportion of neighbours that are resident
        # reverse resident2 so that TRUE = FALSE and FALSE = TRUE.
        # I find it more intuitive to think about immigration.
        resident2 = !resident2,
        prop_immigrant = mean(resident2, na.rm = TRUE),
        prop_immigrant_sd = sd(resident2, na.rm = TRUE),
        prop_immigrant_recorded = mean(resident2[n_vocalisations2 > 0], na.rm = TRUE),
        # calculate the mean dispersal distance of neighbours
        mean_dispersal_distance = mean(dispersal_distance2, na.rm = TRUE),
        mean_dispersal_distance_sd = sd(dispersal_distance2, na.rm = TRUE),
        mean_dispersal_distance_recorded = mean(dispersal_distance2[n_vocalisations2 > 0], na.rm = TRUE),
        # calculate the mean age of neighbours
        mean_age = mean(age2, na.rm = TRUE),
        mean_age_sd = sd(age2, na.rm = TRUE),
        mean_age_recorded = mean(age2[n_vocalisations2 > 0], na.rm = TRUE),
        # add a list of all father2 that are not NA
        father_list = list(father2[!is.na(father2)])
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
        pnum, pnum2, nestbox, year, year2, neighbours, recorded, mean_spat_dist,
        mean_spat_dist_sd, mean_spat_dist_recorded, prop_immigrant,
        prop_immigrant_sd, prop_immigrant_recorded,
        mean_dispersal_distance, mean_dispersal_distance_sd,
        mean_dispersal_distance_recorded,
        mean_age, mean_age_sd, mean_age_recorded, father_list
    ) |>
    # remove where pnum is duplicated
    dplyr::distinct(pnum, .keep_all = TRUE)


# Prepare data in longer format (I miss python so much)
neigh_data_filtered <- neigh_data |>
    dplyr::select(nestbox, year, father_list)

neigh_data_joined <- neigh_data_filtered |>
    dplyr::left_join(
        neigh_data_filtered |>
            dplyr::rename(year2 = year, father_list2 = father_list) |>
            dplyr::mutate(year = year2 - 1),
        by = c("nestbox", "year")
    )

# Calculate the proportion of strings that are also in the father_list
# for the same nestbox but in the previous year
neigh_data_joined <- neigh_data_joined |>
    dplyr::mutate(
        father_list = purrr::map(father_list, unique),
        father_list2 = purrr::map(father_list2, unique),
        prop_same_birds = purrr::map2_dbl(
            father_list, father_list2,
            ~ length(intersect(.x, .y)) / length(.y)
        )
    ) |>
    # change year2 to year
    dplyr::select(-year, -father_list, -father_list2) |>
    dplyr::rename(year = year2)


# Now, join the proportion of same birds to the original data
neigh_data <- neigh_data |>
    dplyr::left_join(neigh_data_joined, by = c("nestbox", "year"))



# Prepare sharing data
shframe <-
    main_data |>
    dplyr::select(pnum, nestbox, year, x, y, resident) |>
    # create all pairwise combinations of boxes per year, except self-self
    tidyr::expand(pnum, pnum2 = pnum) |>
    # now add the data for each pnum
    dplyr::full_join(main_data, by = c("pnum" = "pnum")) |>
    dplyr::full_join(main_data,
        by = c("pnum2" = "pnum"),
        suffix = c("", "2")
    ) |>
    # remove rows where nestbox or nestbox2 is in nest_rm
    dplyr::filter(!nestbox %in% nest_rm, !nestbox2 %in% nest_rm) |>
    dplyr::mutate(
        nest_distance = nestbox_distances[cbind(nestbox, nestbox2)]
    ) |>
    dplyr::filter(nest_distance < n_radius) |>
    dplyr::select(pnum, pnum2, nestbox, year, year2)


ssub <- sharing_data |>
    dplyr::filter(nest_distance < n_radius) |>
    dplyr::select(pnum, pnum2, labels1, labels2, nestbox, nestbox2)


nsharing <- shframe |>
    dplyr::full_join(ssub |> dplyr::select(pnum, labels1), by = c(
        "pnum" = "pnum"
    ), relationship = "many-to-many") |>
    dplyr::full_join(ssub |> dplyr::select(pnum2, labels2), by = c(
        "pnum2" = "pnum2"
    ), relationship = "many-to-many") |>
    # remove identical rows
    dplyr::distinct(pnum, pnum2, .keep_all = TRUE) |>
    dplyr::mutate(pnum2 = stringr::str_remove(pnum2, "empty_")) |>
    dplyr::mutate(pnum = stringr::str_remove(pnum, "empty_")) |>
    dplyr::mutate(nestbox2 = stringr::str_sub(pnum2, 6))


sharing_past <- nsharing |>
    dplyr::filter(year - 1 == year2) |>
    dplyr::group_by(pnum) |>
    dplyr::mutate(past_labels = if (any(!is.null(labels2))) list(labels2) else NA) |>
    dplyr::mutate(past_labels = purrr::map(past_labels, unlist)) |>
    dplyr::ungroup() |>
    dplyr::distinct(pnum, .keep_all = TRUE)


sharing_current <- nsharing |>
    dplyr::filter(year2 == year) |>
    dplyr::group_by(pnum) |>
    dplyr::mutate(current_labels = if (any(!is.null(labels2))) list(labels2) else NA) |>
    dplyr::mutate(current_labels = purrr::map(current_labels, unlist)) |>
    dplyr::ungroup() |>
    dplyr::distinct(pnum, .keep_all = TRUE)

# remove the nsharing data frame and any other data frames that are no longer needed
# need to do this to free up memory, pos laptop
rm(ssub, shframe, nsharing, neigh_data_filtered, neigh_data_joined, main_data)
gc()

classinfo <- manual_labels |>
    tidyr::separate(class_id,
        into = c("pnum"),
        sep = "_",
        remove = FALSE,
        extra = "drop"
    ) |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4)) |>
    dplyr::group_by(year) |>
    dplyr::mutate( # add count of total class_label (alread in the df) per year
        n_year = length(class_label)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(class_label, year) |>
    # for each year count how many pnums have a class_label
    dplyr::mutate(
        year_freq = length(unique(pnum)),
        rare = ifelse(year_freq < 5, TRUE, FALSE),
        # calculate the relative frequency within each year
        rel_freq = year_freq / n_year
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(class_label)


# get lists with the rare songs in each year
rare_songs <- list()
for (year in unique(classinfo$year)) {
    rare_songs[[year]] <- classinfo |>
        dplyr::filter(year == !!year, rare == TRUE) |>
        dplyr::pull(class_label) |>
        unique()
}


sharing_df <- dplyr::left_join(
    sharing_current, sharing_past,
    by = "pnum", suffix = c("", "2")
) |>
    dplyr::select(pnum, year, current_labels, past_labels) |>
    dplyr::mutate(
        unique_current_labels = purrr::map(current_labels, unique),
        unique_past_labels = purrr::map(past_labels, unique),
        n_current_songs = unlist(purrr::map(current_labels, length)),
        n_unique_current_songs = unlist(purrr::map(unique_current_labels, length)),
        n_songs_past = unlist(purrr::map(unique_past_labels, length)),
        n_shared = unlist(purrr::map2(
            unique_past_labels,
            unique_current_labels,
            ~ length(intersect(.x, .y))
        )),
        prop_shared = unlist(purrr::map2_dbl(
            n_shared, n_unique_current_songs, ~ .x / .y
        )),
        n_rare = unlist(purrr::map2_dbl(
            unique_current_labels, year, ~ {
                year_val <- eval(as.character(.y))
                sum(.x %in% rare_songs[[as.character(year_val)]])
            }
        )),
        prop_rare = unlist(purrr::map2_dbl(
            n_rare, n_unique_current_songs, ~ .x / .y
        )),
        diversity = unlist(purrr::map2_dbl(
            n_unique_current_songs, n_current_songs, ~ .x / .y
        )),
        novelty = scales::rescale(1 - log(unlist(purrr::map(
            unique_current_labels, ~ {
                mean(classinfo |>
                    dplyr::filter(class_label %in% .x) |>
                    dplyr::distinct(class_label, .keep_all = TRUE) |>
                    dplyr::pull(rel_freq))
            }
        ))))
    )

# add sharing_df to neigh_data
neighbour_data <- neigh_data |>
    dplyr::left_join(sharing_df, by = c("pnum", "year")) |>
    dplyr::left_join(nestbox_data, by = c("nestbox" = "nestbox")) |>
    dplyr::filter(!grepl("empty", pnum)) |>
    # remove all columns after the column named 'y'
    dplyr::select(pnum:y) |>
    # remove where year is 2019
    dplyr::filter(year != 2019)


# save neighbourhood_data to a csv
neighbour_data |>
    readr::write_csv(
        file.path(config$path$derived_data, "neighbour_data.csv")
    )
