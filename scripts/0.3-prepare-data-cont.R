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

# Calculate the age difference between the two birds (within year, overall)
sharing_data <- sharing_data |>
    dplyr::mutate(
        age = as.numeric(age),
        age2 = as.numeric(age2)
    ) |>
    dplyr::mutate(age_difference = abs(age - age2)) |>
    dplyr::mutate(year_born_diff = abs(year_born - year_born2))

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


# plot average of mean_dist1 and mean_dist2 for each pair of pnums vs nest_distance

sharing_data |>
    dplyr::filter(pnum < pnum2) |>
    # remove rows where father and father2 are NA
    dplyr::filter(!is.na(father), !is.na(father2)) |>
    # remove where (mean_dist1 + mean_dist2) / 2) is < 0.9
    dplyr::filter((mean_dist1 + mean_dist2) / 2 > 0.9) |>
    # calculate difference in years between year_born and year_born2
    dplyr::mutate(year_born_diff = abs(year_born - year_born2)) |>
    # convert the age_differencecolumn to 0, 1 and 2+
    dplyr::mutate(year_born_diff = dplyr::case_when(
        age_difference == 0 ~ "0",
        age_difference == 1 ~ "1",
        age_difference >= 2 ~ "2+"
    )) |>
    # remove distance between nestboxes > 3000
    ggplot2::ggplot(ggplot2::aes(
        x = nest_distance,
        y = mean_dist1,
        color = resident_status
    )) +
    # ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::labs(x = "Distance between nestboxes (m)", y = "Average of mean_dist1 and mean_dist2")

# from sharing data subset all distances <500 and bin them into 100m bins, then plot the mean and SE for each bin

sharing_data |>
    dplyr::filter(year == year2) |>
    dplyr::filter(nest_distance < 500) |>
    # remove rows where pnum  and pnum2 are the same but in reverse order
    dplyr::filter(pnum < pnum2) |>
    dplyr::mutate(nest_distance_bin = cut(nest_distance, breaks = seq(0, 500, 100))) |>
    dplyr::group_by(nest_distance_bin) |>
    # get the inverse of mean_dist1 and mean_dist2
    dplyr::mutate(
        mean_dist1 = 1 / mean_dist1,
        mean_dist2 = 1 / mean_dist2
    ) |>
    dplyr::summarize(
        mean_dist = mean((mean_dist1 + mean_dist2) / 2),
        se = stats::sd((mean_dist1 + mean_dist2) / 2) / sqrt(dplyr::n())
    ) |>
    dplyr::filter(!is.na(nest_distance_bin)) |>
    ggplot2::ggplot(ggplot2::aes(x = nest_distance_bin, y = mean_dist)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_dist - se, ymax = mean_dist + se), width = .05) +
    ggplot2::labs(x = "Distance between nestboxes (m)", y = "Average acoustic distance") +
    # remove grid lines and only leave left and bottom axis lines
    ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
        # add space between axis title and axis labels
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10))
    ) +
    # change x axis labels to 100, 200, 300, 400, 500
    ggplot2::scale_x_discrete(
        labels = c("100", "200", "300", "400", "500")
    )
