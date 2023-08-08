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
    file.path(config$path$derived_data, "cont_pairwise_data.csv")
) |>
    dplyr::mutate(year = as.factor(year), year2 = as.factor(year2))
main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)


# MAIN ───────────────────────────────────────────────────────────────────── #




# calculate the frequency of each class label in each year (year is the first
# four characters of the class_id column):


freq_by_year <- manual_labels |>
    # split class_id by the underscore:
    tidyr::separate(class_id,
        into = c("pnum"),
        sep = "_",
        remove = F
    ) |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4)) |>
    dplyr::group_by(year) |>
    # count unique class_labels in each year
    dplyr::mutate(
        year_type_count = dplyr::n_distinct(class_label),
        year_pnum_count = dplyr::n_distinct(pnum)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(year, class_label) |>
    dplyr::mutate(num_pnums = dplyr::n_distinct(pnum)) |>
    # now calculate the frequency of each class_label each year
    dplyr::reframe(
        n = dplyr::n(),
        stype_freq = n / year_type_count,
        bird_freq = num_pnums / year_pnum_count
    ) |>
    # remove duplicate rows
    dplyr::distinct() |>
    # arrange by class_label and freq
    dplyr::arrange(year)

# for each class_label add the freq in the next year in the same row
freq_by_year <- freq_by_year |>
    dplyr::group_by(class_label) |>
    dplyr::mutate(
        stype_freq_next_year = dplyr::lead(stype_freq),
        year_next = dplyr::lead(year),
        bird_freq_next_year = dplyr::lead(bird_freq)
    ) |>
    dplyr::ungroup()


# plot freq vs freq_next_year
freqplot <- freq_by_year |>
    dplyr::arrange(class_label) |>
    ggplot(aes(x = stype_freq, y = stype_freq_next_year)) +
    # add points with some jitter
    geom_point(
        position = position_jitter(width = 0.05, height = 0.05),
        size = .9, alpha = .8
    ) +
    geom_ribbon(stat = "smooth", fill = titpalette(2)[2], alpha = .1) +
    geom_smooth(
        method = "loess",
        se = FALSE,
        color = titpalette(2)[2],
        alpha = .7,
    ) +
    geom_abline(
        slope = 1, intercept = 0, linetype = "dashed",
        color = "#858585"
    ) +
    titheme() +
    scale_x_log10(limits = c(0.003, 0.35)) +
    scale_y_log10(limits = c(0.003, 0.35)) +
    theme(
        aspect.ratio = 1,
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "none"
    ) +
    labs(
        x = expression(log[10] ~ " Frequency in Year"),
        y = expression(log[10] ~ " Frequency in Year + 1"),
        title = "Frequency of Song Types Across Years",
    )


ggsave(
    file.path(config$path$figures, "song_frequency.png"),
    plot = freqplot,
    width = 10,
    height = 10,
    units = "cm",
    dpi = 300
)
