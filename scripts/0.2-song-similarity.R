# CONFIGURATION ──────────────────────────────────────────────────────────── #
config <- config::get()
source(file.path(config$path$source, "rplot.R"))
box::use(R / io[read_csv_file])

# READ IN THE DATA ───────────────────────────────────────────────────────── #

# Load feature vector data
labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)

labels <- labels |>
    tidyr::extract(class_id, into = "pnum", regex = "^(\\w+)_", remove = FALSE)


# group by pnum and calculate how many labels are shared by each pair of pnums
pnumlabs <- labels |>
    # summarise so that each pnum has a list of labels
    dplyr::group_by(pnum) |>
    dplyr::summarise(class_label = toString(class_label))

# now create a long format table with all combinations of pnums
pnumlabs <- pnumlabs |>
    dplyr::mutate(pnum = as.factor(pnum)) |>
    tidyr::expand(pnum, pnum2 = pnum) |>
    # remove self-combinations
    dplyr::filter(pnum != pnum2) |>
    # add labels for each pnum
    dplyr::left_join(pnumlabs, by = c("pnum" = "pnum")) |>
    dplyr::rename(labels1 = class_label) |>
    dplyr::left_join(pnumlabs, by = c("pnum2" = "pnum")) |>
    dplyr::rename(labels2 = class_label)


# and now calculate the number of total, combined total and shared
# labels for each pair of pnums
sharing_data <- pnumlabs |>
    dplyr::mutate(
        labels1 = stringr::str_split(labels1, ", "),
        labels2 = stringr::str_split(labels2, ", "),
        total1 = purrr::map_int(labels1, ~ length(.x)),
        total2 = purrr::map_int(labels2, ~ length(.x)),
        shared = purrr::map2_int(
            labels1, labels2, ~ length(intersect(.x, .y))
        )
    ) |>
    dplyr::mutate(total = total1 + total2)

# count how many pnums have more than 0 shared labels
print(paste(
    "Number of pnums with shared labels:",
    sum(sharing_data$shared > 0) / 2
))



# SAVE TO CSV ────────────────────────────────────────────────────────────── #


# lists to string sso they can be saved as csv
sharing_data <- sharing_data |>
    dplyr::mutate(
        labels1 = purrr::map_chr(labels1, ~ toString(.x)),
        labels2 = purrr::map_chr(labels2, ~ toString(.x))
    )
readr::write_csv(sharing_data, file.path(
    config$path$derived_data,
    "song_sharing_data.csv"
))
