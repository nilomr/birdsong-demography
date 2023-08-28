# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme])
box::use(patchwork[...])
box::use(ggplot2[...])


# READ IN THE DATA ───────────────────────────────────────────────────────── #

# Load feature vector data
songfeats <- read_csv_file(
    file.path(config$path$data, "feature_vectors.csv")
)
# Load metadata
songmeta <- read_csv_file(
    file.path(config$path$data, "great-tit-hits.csv")
)

main_data <- read_csv_file(file.path(config$path$derived_data, "main.csv"))

# Load feature vector data
labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
) |>
    tidyr::extract(class_id, into = "pnum", regex = "^(\\w+)_", remove = FALSE)

# add the class_id column from songmeta as the first column of songfeats:
songfeats <- dplyr::bind_cols(songmeta |> dplyr::select(class_id), songfeats)

# remove all rows where class_id contains SW115 (broken bird) and
# 20221MP32 (no bird info)
songfeats <- songfeats |>
    dplyr::filter(!grepl("20211O115|20221MP32", class_id))
songmeta <- songmeta |>
    dplyr::filter(!grepl("20211O115|20221MP32", class_id))


# ──── MAIN ───────────────────────────────────────────────────────────────────

# Calculate distances

# Find median feature vector for each label
songfeats_median <- songfeats |>
    dplyr::group_by(class_id) |>
    dplyr::summarize(dplyr::across(-1, median))


songfeats_median <- songfeats_median |>
    dplyr::mutate(pnum = stringr::str_split(
        class_id, "_",
        simplify = TRUE
    )[, 1]) |>
    dplyr::select(pnum, class_id, dplyr::everything())


# Get unique pnums
unique_pnums <- unique(songfeats_median$pnum)

# Initialize empty data frame to store results
mean_dists <- data.frame(
    pnum = character(),
    pnum2 = character(),
    mean_dist1 = numeric(),
    mean_dist2 = numeric()
)

# Create a matrix of all pairwise distances between songs
song_dists <- as.matrix(dist(songfeats_median[, -c(1, 2)]))
colnames(song_dists) <- songfeats_median$class_id
rownames(song_dists) <- songfeats_median$class_id


# convert song_dists to a long dataframe
song_dists_long <- song_dists |>
    as.data.frame() |>
    tidyr::pivot_longer(
        cols = everything(),
        names_to = "class_id",
        values_to = "dist"
    ) |>
    # now add the class_id, repeating as necessary
    dplyr::mutate(
        class_id2 = rep(colnames(song_dists), each = nrow(song_dists)),
        class_id = rep(colnames(song_dists), nrow(song_dists))
    ) |>
    # remove rows where class_id and class_id2 are the same in reverse order
    dplyr::filter(class_id < class_id2) |>
    dplyr::arrange(class_id, class_id2) |>
    dplyr::mutate(
        pnum = stringr::str_split(class_id, "_", simplify = TRUE)[, 1],
        pnum2 = stringr::str_split(class_id2, "_", simplify = TRUE)[, 1],
        year = as.numeric(stringr::str_sub(pnum, 1, 4)),
        year2 = as.numeric(stringr::str_sub(pnum2, 1, 4))
    )


main_data_r <- main_data |>
    dplyr::select(pnum, father)

song_dists_long <- song_dists_long |>
    dplyr::left_join(main_data_r, by = c("pnum" = "pnum")) |>
    dplyr::rename(bird_id = father) |>
    dplyr::left_join(main_data_r, by = c("pnum2" = "pnum")) |>
    dplyr::rename(bird_id2 = father)


# add distance between birds

# get all the rows in main_data with unique nestbox
nestbox_data <- main_data |>
    dplyr::select(nestbox, x, y) |>
    dplyr::distinct()


# calculate all the pairwise distances between nestboxes
nestbox_distances <- round(stats::dist(nestbox_data[, 2:3]), 2)
# convert to a matrix with rownames and colnames
nestbox_distances <- as.matrix(nestbox_distances)
rownames(nestbox_distances) <- nestbox_data$nestbox
colnames(nestbox_distances) <- nestbox_data$nestbox


# add nestboxes to sharing_data
song_dists_long <- song_dists_long |>
    dplyr::mutate(
        nestbox = stringr::str_sub(pnum, 6),
        nestbox2 = stringr::str_sub(pnum2, 6)
    )

# get the distance between each pair of (nestbox, nestbox2) from nestbox_distances
# that is present in song_dists_long and add it to song_dists_long
song_dists_long <- song_dists_long |>
    dplyr::mutate(
        nest_distance = nestbox_distances[cbind(nestbox, nestbox2)]
    )

# Select and match where dist is less than 0.9 and nest_distance is less than 100m
m <- song_dists_long |>
    dplyr::filter(
        (is.na(bird_id) & !is.na(bird_id2)) |
            (!is.na(bird_id) & is.na(bird_id2)),
        dist < 0.9, nest_distance < 100,
        year != year2,
    ) |>
    # remove where there are less than two rows per pnum and pnum2 combinations
    dplyr::group_by(pnum, pnum2) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    # now only keep unique combinations of pnum and pnum2
    dplyr::distinct(pnum, pnum2, .keep_all = TRUE) |>
    # remove class_id and class_id2
    dplyr::select(-c(class_id, class_id2)) |>
    # if one bird_id or bird_id2 is known and the other is not, use the known one
    dplyr::mutate(
        bird_id = dplyr::case_when(
            is.na(bird_id) & !is.na(bird_id2) ~ bird_id2,
            !is.na(bird_id) & is.na(bird_id2) ~ bird_id,
            TRUE ~ bird_id
        ),
        bird_id2 = dplyr::case_when(
            is.na(bird_id) & !is.na(bird_id2) ~ bird_id2,
            !is.na(bird_id) & is.na(bird_id2) ~ bird_id,
            TRUE ~ bird_id2
        )
    )

ids <- dplyr::tibble(
    pnum = c(m$pnum, m$pnum2),
    bird_id = c(m$bird_id, m$bird_id2)
)

# Test join
main_data_s <- main_data |>
    dplyr::filter(pnum %in% labels$pnum)

before <- sum(!is.na(main_data_s[main_data_s$year %in%
    c(2020, 2021, 2022), ]$father))

main_data_s <- main_data_s |>
    dplyr::left_join(ids, by = c("pnum" = "pnum")) |>
    dplyr::mutate(
        father = dplyr::case_when(
            is.na(father) ~ bird_id,
            TRUE ~ father
        )
    ) |>
    dplyr::select(-bird_id)


# count NA values in father column in main_data
after <- sum(!is.na(main_data_s[main_data_s$year %in% c(2020, 2021, 2022), ]$father))

cat(
    "Number of values in father column before:",
    before,
    "\nNumber of values in father column after:",
    after,
    "\nDifference:",
    after - before
)

# count how many unique fathers there are in main_data_s
nid <- length(unique(main_data_s$father[!is.na(main_data_s$father) &
    main_data_s$year %in% c(2020, 2021, 2022)]))

cat("Number of unique fathers:", nid, "\n")


# count how many appear more than once
main_data_s |>
    dplyr::filter(year %in% c(2020, 2021, 2022)) |>
    dplyr::filter(!is.na(father)) |>
    dplyr::group_by(father) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::distinct(father) |>
    nrow()

# from 49 to 70

# All ok, do it to main_data and save

main_data <- main_data |>
    dplyr::left_join(ids, by = c("pnum" = "pnum")) |>
    dplyr::mutate(
        father = dplyr::case_when(
            is.na(father) ~ bird_id,
            TRUE ~ father
        )
    ) |>
    dplyr::select(-bird_id)



# SAVE THE DATA TO A CSV FILE ────────────────────────────────────────────── #

readr::write_csv(main_data, file.path(config$path$derived_data, "main.csv"))


# ──── BUILD PLOTS ────────────────────────────────────────────────────────────

# using OLD data to define thresholds

# for each pair of class_id and class_id2, get those where bird_id and bird_id2 are the same:
song_dists_long_same_bird <- song_dists_long |>
    dplyr::filter(bird_id == bird_id2) |>
    dplyr::filter(year != year2) |>
    # now find for each pnum and pnum2 the row with the minimum dist
    dplyr::group_by(pnum, pnum2) |>
    dplyr::filter(dist == min(dist)) |>
    dplyr::ungroup() |>
    dplyr::select(pnum, pnum2, dist)

# now do the same but for different birds
song_dists_long_diff_bird <- song_dists_long |>
    dplyr::filter(bird_id != bird_id2) |>
    dplyr::filter(year != year2) |>
    # now find for each pnum and pnum2 the row with the minimum dist
    dplyr::group_by(pnum, pnum2) |>
    dplyr::filter(dist == min(dist)) |>
    dplyr::ungroup() |>
    dplyr::select(pnum, pnum2, dist)

colors <- c("Same" = "#c9710d", "Different" = "#318392")
# now plot the distributions of distances for same and different birds in the same plot
dist_dists <- song_dists_long_same_bird |>
    dplyr::mutate(bird_type = "Same") |>
    dplyr::bind_rows(
        song_dists_long_diff_bird |>
            dplyr::mutate(bird_type = "Different")
    ) |>
    ggplot(aes(x = dist, fill = bird_type)) +
    geom_histogram(aes(y = ..density..),
        position = "identity",
        bins = 50, alpha = 0.5
    ) +
    geom_density(aes(y = ..density.., fill = bird_type, color = bird_type),
        alpha = 0.5
    ) +
    # vline at 0.7
    geom_vline(
        xintercept = 0.9, linetype = "dashed",
        color = "#858585"
    ) +
    titheme() +
    theme(aspect.ratio = 1) +
    scale_color_manual(
        values = colors,
        labels = rev(c("Same\nbirds", "Different\nbirds")),
        name = "Comparison"
    ) +
    scale_fill_manual(
        values = colors,
        labels = rev(c("Same\nbirds", "Different\nbirds")),
        name = "Comparison"
    ) +
    guides(
        fill = guide_legend(
            byrow = TRUE, title.position = "top",
        ),
        color = guide_legend(
            byrow = TRUE, title.position = "top",
        )
    ) +
    theme(legend.spacing.y = unit(.1, "cm")) +
    labs(x = "Acoustic Distance", y = "Density")


# Plot the distribution of postbreeeding movements

main_data |>
    # plot a histogram with the distribution of distances
    ggplot(aes(x = dispersal_distance)) +
    geom_histogram(bins = 100) +
    labs(x = "Distance (m)", y = "Count") +
    # add more x-axis ticks
    scale_x_continuous(breaks = seq(0, 3500, 500))

# Plot the centred distribution of distances for each father
dispplot <- main_data |>
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
    ggplot(aes(x = dispersal_distance)) +
    geom_histogram(aes(y = ..density..),
        position = "identity",
        bins = 50, alpha = 0.5,
        fill = "#8a3333",
    ) +
    # vlines in -100 and 100
    geom_vline(
        xintercept = c(-100, 100), linetype = "dashed",
        color = "#858585"
    ) +
    geom_density(aes(y = ..density..),
        alpha = 0.5,
        color = "#8a3333",
        fill = "#8a3333"
    ) +
    titheme() +
    theme(aspect.ratio = 1) +
    labs(x = "Distance (m)", y = "Density")


both <- dist_dists + dispplot

ggsave(
    file.path(config$path$figures, "supp_acc_bird_distances.svg"),
    plot = both,
    width = 16,
    height = 8,
    units = "cm",
    dpi = 300
)
