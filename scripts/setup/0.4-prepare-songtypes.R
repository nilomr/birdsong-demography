# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


# LOAD DATA ──────────────────────────────────────────────────────────────── #

# Load song_sharing_data
sharing_data <- read_csv_file(
    file.path(config$path$derived_data, "pairwise_data.csv")
) |>
    dplyr::mutate(year = as.factor(year), year2 = as.factor(year2))


manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)

main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

# PREPARE SONG-TYPE DATA ─────────────────────────────────────────────────── #


# calculate the frequency of each class_label in manual_labels for each year
class_label_freq <- manual_labels |>
    tidyr::separate(class_id,
        into = c("pnum"),
        sep = "_",
        remove = F,
        extra = "drop"
    ) |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4)) |>
    dplyr::group_by(year) |>
    # count unique class_labels in each year
    dplyr::mutate(
        year_type_count = dplyr::n_distinct(class_label)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(year, class_label) |>
    dplyr::mutate(
        n_year = dplyr::n(),
        class_freq = n_year / year_type_count,
    ) |>
    dplyr::arrange(desc(n_year)) |>
    # for each pnum count how many class_label it has with n_year <3
    dplyr::group_by(pnum) |>
    dplyr::mutate(
        rare_n = sum(n_year < 4),
        singletons = sum(n_year == 1),
        average_frequency = mean(class_freq)
    )


class_label_freq <- class_label_freq |>
    # remove duplicate pnums
    dplyr::distinct(pnum, .keep_all = TRUE) |>
    dplyr::select(pnum, rare_n, singletons, average_frequency)


# merge this with the pairwise data
sharing_data <- sharing_data |>
    dplyr::left_join(class_label_freq, by = c("pnum"), relationship = "many-to-many") |>
    dplyr::left_join(class_label_freq, by = c("pnum2" = "pnum"), suffix = c("", "2"))


# do the same with main_data
main_data <- main_data |>
    dplyr::left_join(class_label_freq, by = c("pnum"))


# save updated sharing data and main data
sharing_data |>
    readr::write_csv(
        file.path(config$path$derived_data, "pairwise_data.csv")
    )

main_data |>
    readr::write_csv(
        file.path(config$path$derived_data, "main.csv")
    )


# calculate mean and sd for rare_n to compare with McGregor & Krebs 1982
main_data |>
    # get where nestbox starts with B
    dplyr::group_by(resident) |>
    dplyr::summarise(
        mean = mean(rare_n, na.rm = T),
        sd = sd(rare_n, na.rm = T)
    )



# END




# # Now get the average freq of the labels in labels1 for each pnum in pnum_df.


# # in the s_sharing_data get only one row per unique pnum
# pnum_df <- s_sharing_data |>
#     dplyr::group_by(pnum) |>
#     dplyr::slice(1) |>
#     dplyr::ungroup() |>
#     # remove any columns with a 2 in the end of the name
#     dplyr::select(-ends_with("2"))

# # split the labels1 column into a list of labels
# pnum_df <- pnum_df |>
#     dplyr::mutate(
#         labels1 = stringr::str_split(labels1, ", "),
#     )
# # unnest the list of labels
# pnum_df <- pnum_df |>
#     tidyr::unnest(cols = labels1)

# # get the frequency of each label
# pnum_df <- pnum_df |>
#     dplyr::left_join(class_label_freq, by = c("labels1" = "class_label")) |>
#     dplyr::select(-n) |>
#     dplyr::rename(label = labels1)

# # calculate the average frequency of the labels by pnum and merge with main
# # on pnum
# songype_df <- pnum_df |>
#     dplyr::left_join(main_data, by = "pnum")

# # calculate the pairwise distances between all instances of the same label in main_data

# # get the unique labels in main_data
# unique_labels <- songype_df |>
#     dplyr::select(label) |>
#     dplyr::distinct()

# # remove birds without id
# songype_df <- songype_df |>
#     dplyr::filter(!is.na(father))



# calculate_distances <- function(songype_df, n, randomize) {
#     unique_labels <- dplyr::distinct(songype_df, label)
#     if (randomize) {
#         # add progress bar
#         pb <- progress::progress_bar$new(
#             format = "[:bar] :percent :eta",
#             total = n
#         )
#         # randomise the coordinates n times
#         results <- list()
#         for (i in 1:n) {
#             coords_df <- dplyr::select(songype_df, x, y)
#             perm <- sample(nrow(coords_df))
#             randomized_df <- coords_df[perm, ]
#             randomized_songype_df <- dplyr::mutate(
#                 songype_df,
#                 x = randomized_df$x, y = randomized_df$y
#             )

#             # calculate the pairwise distances between all instances of the same label
#             label_coords <- unique_labels |>
#                 dplyr::group_split(label) |>
#                 purrr::map(~ dplyr::filter(randomized_songype_df, label == .x$label) |>
#                     dplyr::select(x, y))
#             label_distances <- label_coords |>
#                 purrr::map(~ round(stats::dist(.x), 2))
#             unique_labels <- unique_labels |>
#                 dplyr::arrange(label)
#             names(label_distances) <- unique_labels$label
#             label_distances <- label_distances |>
#                 purrr::discard(purrr::is_empty)
#             label_distances <- purrr::map(label_distances, as.vector)
#             result_df <- tibble::enframe(label_distances, name = "label") |>
#                 tidyr::unnest(value, keep_empty = TRUE)
#             results[[i]] <- result_df
#             pb$tick()
#         }

#         result <- dplyr::bind_rows(results, .id = "randomised")
#         return(result)
#     } else {
#         label_coords <- unique_labels |>
#             dplyr::group_split(label) |>
#             purrr::map(~ dplyr::filter(songype_df, label == .x$label) |>
#                 dplyr::select(x, y))
#         label_distances <- label_coords |>
#             purrr::map(~ round(stats::dist(.x), 2))
#         unique_labels <- unique_labels |>
#             dplyr::arrange(label)
#         names(label_distances) <- unique_labels$label
#         label_distances <- label_distances |>
#             purrr::discard(purrr::is_empty)
#         label_distances <- purrr::map(label_distances, as.vector)
#         result_df <- tibble::enframe(label_distances, name = "label") |>
#             tidyr::unnest(value, keep_empty = TRUE)
#         return(result_df)
#     }
# }


# randomised <- calculate_distances(songype_df, 100, T)
# actual <- calculate_distances(songype_df, 1, F)


# randomised <- randomised |>
#     dplyr::mutate(type = "randomised")
# actual <- actual |>
#     dplyr::mutate(type = "actual")
# # put the randomised and actual in one df with tidy long format
# result_df <- dplyr::bind_rows(randomised, actual)


# ggplot2::ggplot() +
#     ggplot2::geom_histogram(
#         data = randomised,
#         ggplot2::aes(x = value, fill = label, y = stat(density * width)),
#         binwidth = 100,
#         alpha = 0.5,
#         position = "identity"
#     ) +
#     ggplot2::geom_histogram(
#         data = actual,
#         ggplot2::aes(x = value, fill = label, y = stat(density * width)),
#         binwidth = 100,
#         alpha = 0.5,
#         position = "identity"
#     )


# # find rows in songtype_df where label is 200-N6-R3
# songype_df |>
#     dplyr::filter(label == "200-N6-R3")





# # filter by label 459-N2-UH-LH-R1
# pnum_df |>
#     dplyr::filter(label == "459-N2-UH-LH-R1")

# # plot the avg_freq vs. resident as factor
# main_data |>
#     ggplot2::ggplot(ggplot2::aes(x = avg_freq, y = as.factor(resident))) +
#     ggplot2::geom_point() +
#     ggplot2::labs(x = "Average frequency of labels", y = "Resident status")
