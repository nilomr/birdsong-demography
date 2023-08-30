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

# END
