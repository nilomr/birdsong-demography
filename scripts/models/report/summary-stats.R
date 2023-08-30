manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)


# calculate the average and sd prop_shared by year
neighbour_df |>
    dplyr::group_by(year) |>
    dplyr::filter(
        !is.na(mean_dispersal_distance), !grepl("empty", pnum)
    ) |>
    dplyr::summarise(
        mean_prop_shared = mean(prop_shared, na.rm = TRUE),
        sd_prop_shared = sd(prop_shared, na.rm = TRUE)
    )


# calculate the average and sd prop_shared by year
nm_data_std |>
    dplyr::group_by(year) |>
    dplyr::summarise(
        mean_prop_shared = mean(prop_shared, na.rm = TRUE),
        sd_prop_shared = sd(prop_shared, na.rm = TRUE)
    )

# Turnover

# Population-level change
myears <- manual_labels |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4))
myears_list <- lapply(unique(myears$year), function(x) myears$class_label[myears$year == x])
myears_list <- lapply(myears_list, unique)
dplyr::tibble(
    prop_shared_1_2 = length(intersect(myears_list[[1]], myears_list[[2]]))
    / length(myears_list[[2]]),
    prop_shared_2_3 = length(intersect(myears_list[[2]], myears_list[[3]]))
    / length(myears_list[[2]]),
    prop_shared_1_3 = length(intersect(myears_list[[1]], myears_list[[3]]))
    / length(myears_list[[3]])
)
