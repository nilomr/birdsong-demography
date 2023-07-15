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


# CLUSTER SONGS ──────────────────────────────────────────────────────────── #

deepsplit <- .5

# Find median feature vector for each label
songfeats_median <- songfeats |>
    dplyr::group_by(class_id) |>
    dplyr::summarize(dplyr::across(-1, median))

# Perform hierarchical clustering & cut the dendrogram dynamically
dist_matrix <- dist(songfeats_median[-1])
hclust_result <- hclust(dist_matrix, method = "average")
clusters <- dynamicTreeCut::cutreeHybrid(
    hclust_result, as.matrix(dist_matrix),
    minClusterSize = 1,
    deepSplit = deepsplit,
    respectSmallClusters = TRUE
)

cluster_assignment <- dplyr::tibble(
    labels = clusters$labels, class_id = songfeats_median$class_id
)

# find n uniqye labels in cluster_assignment
print(paste("Number of unique labels:", max(cluster_assignment$labels)))

# Separate class_id into pnum and song_id
cluster_assignment <- cluster_assignment |>
    dplyr::mutate(pnum = stringr::str_split(
        class_id, "_",
        simplify = TRUE
    )[, 1]) |>
    # labvels as factor
    dplyr::mutate(labels = as.factor(labels))

# group by pnum and calculate how many labels are shared by each pair of pnums
pnumlabs <- cluster_assignment |>
    # summarise so that each pnum has a list of labels
    dplyr::group_by(pnum) |>
    dplyr::summarise(labels = toString(labels))

# now create a long format table with all combinations of pnums
pnumlabs <- pnumlabs |>
    dplyr::mutate(pnum = as.factor(pnum)) |>
    tidyr::expand(pnum, pnum2 = pnum) |>
    # remove self-combinations
    dplyr::filter(pnum != pnum2) |>
    # add labels for each pnum
    dplyr::left_join(pnumlabs, by = c("pnum" = "pnum")) |>
    dplyr::rename(labels1 = labels) |>
    dplyr::left_join(pnumlabs, by = c("pnum2" = "pnum")) |>
    dplyr::rename(labels2 = labels)
# remove combinations that are the same but in reverse order
# dplyr::filter(pnum < pnum2)

# and now calculate the number of total, combined total and shared
# labels for each pair of pnums
sharing_data <- pnumlabs |>
    dplyr::mutate(
        total1 = stringr::str_count(labels1, ",") + 1,
        total2 = stringr::str_count(labels2, ",") + 1,
        shared = purrr::map2_dbl(labels1, labels2, function(x, y) {
            length(intersect(
                stringr::str_split(x, ",")[[1]],
                stringr::str_split(y, ",")[[1]]
            ))
        })
    ) |>
    dplyr::mutate(total = total1 + total2)

# count how many pnums have more than 0 shared labels
print(paste(
    "Number of pnums with shared labels:",
    sum(sharing_data$shared > 0) / 2
))


# SAVE TO CSV ────────────────────────────────────────────────────────────── #

readr::write_csv(sharing_data, file.path(
    config$path$derived_data,
    "song_sharing_data.csv"
))
