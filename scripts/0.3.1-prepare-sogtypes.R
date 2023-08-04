# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# load song_sharing_data
s_sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "song_sharing_data.csv")
)

manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)

main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

# PREPARE FULL DYADIC DATA ───────────────────────────────────────────────── #


# get all the rows in main_data with unique nestbox
nestbox_data <- main_data |>
    dplyr::select(nestbox, x, y) |>
    dplyr::distinct()


# Find pnums in sharing_data that are not in main_data
setdiff(s_sharing_data$pnum, main_data$pnum)

# calculate all the pairwise distances between nestboxes
nestbox_distances <- round(stats::dist(nestbox_data[, 2:3]), 2)
# convert to a matrix with rownames and colnames
nestbox_distances <- as.matrix(nestbox_distances)
rownames(nestbox_distances) <- nestbox_data$nestbox
colnames(nestbox_distances) <- nestbox_data$nestbox


# add nestboxes to sharing_data
sharing_data <- s_sharing_data |>
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
    dplyr::left_join(main_data, by = c("pnum" = "pnum")) |>
    dplyr::left_join(main_data,
        by = c("pnum2" = "pnum"),
        suffix = c("", "2")
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

# Calculate the age difference between the two birds
sharing_data <- sharing_data |>
    dplyr::mutate(
        age = as.numeric(age),
        age2 = as.numeric(age2)
    ) |>
    dplyr::mutate(age_difference = abs(age - age2))


# Clean and order columns
sharing_data <- sharing_data |>
    dplyr::select(
        pnum, pnum2, shared, total, year, year2, nestbox, nestbox2,
        nest_distance, father, father2,
        natal_box, natal_box2, natal_distance, year_born, year_born2,
        age, age2, age_difference, resident, resident2, resident_status,
        dispersal_distance, dispersal_distance2, labels1, labels2
    )

# SAVE TO CSV ────────────────────────────────────────────────────────────── #

readr::write_csv(sharing_data, file.path(
    config$path$derived_data,
    "pairwise_data.csv"
))





# calculate the frequency of each class_label in manual_labels
class_label_freq <- manual_labels |>
    dplyr::group_by(class_label) |>
    dplyr::summarise(
        n = dplyr::n(),
        freq = n / nrow(manual_labels)
    ) |>
    dplyr::arrange(desc(freq))

# Now get the average freq of the labels in labels1 for each pnum in pnum_df.


# in the s_sharing_data get only one row per unique pnum
pnum_df <- s_sharing_data |>
    dplyr::group_by(pnum) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    # remove any columns with a 2 in the end of the name
    dplyr::select(-ends_with("2"))

# split the labels1 column into a list of labels
pnum_df <- pnum_df |>
    dplyr::mutate(
        labels1 = stringr::str_split(labels1, ", "),
    )
# unnest the list of labels
pnum_df <- pnum_df |>
    tidyr::unnest(cols = labels1)

# get the frequency of each label
pnum_df <- pnum_df |>
    dplyr::left_join(class_label_freq, by = c("labels1" = "class_label")) |>
    dplyr::select(-n) |>
    dplyr::rename(label = labels1)

# calculate the average frequency of the labels by pnum and merge with main
# on pnum
songype_df <- pnum_df |>
    dplyr::left_join(main_data, by = "pnum")

# calculate the pairwise distances between all instances of the same label in main_data

# get the unique labels in main_data
unique_labels <- songype_df |>
    dplyr::select(label) |>
    dplyr::distinct()

# remove birds without id
songype_df <- songype_df |>
    dplyr::filter(!is.na(father))



calculate_distances <- function(songype_df, n, randomize) {
    unique_labels <- dplyr::distinct(songype_df, label)
    if (randomize) {
        # add progress bar
        pb <- progress::progress_bar$new(
            format = "[:bar] :percent :eta",
            total = n
        )
        # randomise the coordinates n times
        results <- list()
        for (i in 1:n) {
            coords_df <- dplyr::select(songype_df, x, y)
            perm <- sample(nrow(coords_df))
            randomized_df <- coords_df[perm, ]
            randomized_songype_df <- dplyr::mutate(
                songype_df,
                x = randomized_df$x, y = randomized_df$y
            )

            # calculate the pairwise distances between all instances of the same label
            label_coords <- unique_labels |>
                dplyr::group_split(label) |>
                purrr::map(~ dplyr::filter(randomized_songype_df, label == .x$label) |>
                    dplyr::select(x, y))
            label_distances <- label_coords |>
                purrr::map(~ round(stats::dist(.x), 2))
            unique_labels <- unique_labels |>
                dplyr::arrange(label)
            names(label_distances) <- unique_labels$label
            label_distances <- label_distances |>
                purrr::discard(purrr::is_empty)
            label_distances <- purrr::map(label_distances, as.vector)
            result_df <- tibble::enframe(label_distances, name = "label") |>
                tidyr::unnest(value, keep_empty = TRUE)
            results[[i]] <- result_df
            pb$tick()
        }

        result <- dplyr::bind_rows(results, .id = "randomised")
        return(result)
    } else {
        label_coords <- unique_labels |>
            dplyr::group_split(label) |>
            purrr::map(~ dplyr::filter(songype_df, label == .x$label) |>
                dplyr::select(x, y))
        label_distances <- label_coords |>
            purrr::map(~ round(stats::dist(.x), 2))
        unique_labels <- unique_labels |>
            dplyr::arrange(label)
        names(label_distances) <- unique_labels$label
        label_distances <- label_distances |>
            purrr::discard(purrr::is_empty)
        label_distances <- purrr::map(label_distances, as.vector)
        result_df <- tibble::enframe(label_distances, name = "label") |>
            tidyr::unnest(value, keep_empty = TRUE)
        return(result_df)
    }
}


randomised <- calculate_distances(songype_df, 100, T)
actual <- calculate_distances(songype_df, 1, F)


randomised <- randomised |>
    dplyr::mutate(type = "randomised")
actual <- actual |>
    dplyr::mutate(type = "actual")
# put the randomised and actual in one df with tidy long format
result_df <- dplyr::bind_rows(randomised, actual)


ggplot2::ggplot() +
    ggplot2::geom_histogram(
        data = randomised,
        ggplot2::aes(x = value, fill = label, y = stat(density * width)),
        binwidth = 100,
        alpha = 0.5,
        position = "identity"
    ) +
    ggplot2::geom_histogram(
        data = actual,
        ggplot2::aes(x = value, fill = label, y = stat(density * width)),
        binwidth = 100,
        alpha = 0.5,
        position = "identity"
    )


# find rows in songtype_df where label is 200-N6-R3
songype_df |>
    dplyr::filter(label == "200-N6-R3")





# filter by label 459-N2-UH-LH-R1
pnum_df |>
    dplyr::filter(label == "459-N2-UH-LH-R1")

# plot the avg_freq vs. resident as factor
main_data |>
    ggplot2::ggplot(ggplot2::aes(x = avg_freq, y = as.factor(resident))) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Average frequency of labels", y = "Resident status")
