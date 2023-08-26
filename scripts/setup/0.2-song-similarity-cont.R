# CONFIGURATION ──────────────────────────────────────────────────────────── #
config <- config::get()
source(file.path(config$path$source, "rplot.R"))
box::use(R / io[read_csv_file])

# READ IN THE DATA ───────────────────────────────────────────────────────── #

# Load feature vector data
songfeats <- read_csv_file(
    file.path(config$path$data, "feature_vectors.csv")
)
# Load metadata
songmeta <- read_csv_file(
    file.path(config$path$data, "great-tit-hits.csv")
)

# add the class_id column from songmeta as the first column of songfeats:
songfeats <- dplyr::bind_cols(songmeta |> dplyr::select(class_id), songfeats)

# remove all rows where class_id contains SW115 (broken bird) and
# 20221MP32 (no bird info)
songfeats <- songfeats |>
    dplyr::filter(!grepl("20211O115|20221MP32", class_id))
songmeta <- songmeta |>
    dplyr::filter(!grepl("20211O115|20221MP32", class_id))


# CALCULATE AMED -------------------------------------------------------------

# Find median feature vector for each label
songfeats_median <- songfeats |>
    dplyr::group_by(class_id) |>
    dplyr::summarize(dplyr::across(-1, median))


songfeats_median <- songfeats_median |>
    dplyr::mutate(pnum = stringr::str_split(class_id, "_", simplify = TRUE)[, 1]) |>
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

# Initialize progress bar
pb <- progress::progress_bar$new(
    format = "[:bar] :percent ETA: :eta",
    total = choose(length(unique_pnums), 2),
    clear = FALSE
)

# Create a matrix of all pairwise distances between songs
song_dists <- as.matrix(dist(songfeats_median[, -c(1, 2)]))

# Loop through all pairs of unique pnums
for (i in 1:(length(unique_pnums) - 1)) {
    for (j in (i + 1):length(unique_pnums)) {
        # Get indices of songs with pnum i and pnum j
        idx1 <- which(songfeats_median$pnum == unique_pnums[i])
        idx2 <- which(songfeats_median$pnum == unique_pnums[j])

        # Compute minimum distances between songs with pnum i and
        # pnum j (one per song)
        min_dist1 <- apply(
            song_dists[idx1, idx2, drop = FALSE], 1,
            function(x) min(x)
        )
        min_dist2 <- apply(
            song_dists[idx2, idx1, drop = FALSE], 1,
            function(x) min(x)
        )

        # find which of min_dist1 and min_dist2 has lower length,
        # arrange its dists from lower to higher and then remove the last
        # dists until both have the same length
        if (length(min_dist1) > length(min_dist2)) {
            min_dist1 <- sort(min_dist1)[1:length(min_dist2)]
        } else if (length(min_dist2) > length(min_dist1)) {
            min_dist2 <- sort(min_dist2)[1:length(min_dist1)]
        }

        # Add result to mean_dists
        mean_dists <- mean_dists |>
            dplyr::add_row(
                pnum = unique_pnums[i],
                pnum2 = unique_pnums[j],
                mean_dist1 = mean(min_dist1),
                mean_dist2 = mean(min_dist2)
            )

        # Update progress bar
        pb$tick()
    }
}

# Convert mean_dists to a tibble
mean_dists <- dplyr::as_tibble(mean_dists)
# take the inverse of the mean_dists (similarity is more intuitive than distance)
mean_dists <- mean_dists |>
    dplyr::mutate(
        mean_dist1 = 1 / mean_dist1,
        mean_dist2 = 1 / mean_dist2
    )

# save mean_dists to csv
readr::write_csv(mean_dists, file.path(
    config$path$derived_data, "similarity_data.csv"
))
