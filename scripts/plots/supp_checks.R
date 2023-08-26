# CONFIGURATION ------------------------------------------------------------

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, blues, yellows])
box::use(R / utils[og_scale, match_grid, get_spatial_preds, get_raster])
box::use(patchwork[...])
box::use(ggplot2[...])


# LOAD DATA ----------------------------------------------------------------


# Load neighbourhood data
neighbour_df <- read_csv_file(
    file.path(config$path$derived_data, "neighbour_data.csv")
)

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

# BUILD PLOTS ---------------------------------------------------------------

# Plot 1: Neighbourhood data

colors <- c("#FFA500", "#FF8C00", "#FF7F50", "#FF6347")

plots <- list()
y_labels <- c("Recorded", "Prop. Resident (Recorded)", "Mean Dispersal (Recorded)", "Mean Age (Recorded)")
x_labels <- c("Neighbours", "Prop. Resident", "Mean Dispersal", "Mean Age")

print_xgrid <- theme(
    panel.grid.major.x = element_line(
        colour = "#ececec", linewidth = 0.5,
        linetype = 1
    ),
    panel.grid.minor.x = element_blank()
)

for (i in 1:4) {
    y_col <- c("recorded", "prop_immigrant_recorded", "mean_dispersal_distance_recorded", "mean_age_recorded")[i]
    x_col <- c("neighbours", "prop_immigrant", "mean_dispersal_distance", "mean_age")[i]

    p <- neigh_data |>
        ggplot(aes_string(y = y_col, x = x_col)) +
        geom_point(fill = colors[i], alpha = .3, shape = 21, stroke = NA, size = 2) +
        geom_smooth(method = "lm", se = FALSE, color = colorspace::darken(colors[i], .2), linetype = 1) +
        titheme() +
        print_xgrid +
        coord_fixed(ratio = 1) +
        labs(x = x_labels[i], y = y_labels[i])

    plots[[i]] <- p
}

# Combine plots using patchwork
all = plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]]

ggsave(
    filename = file.path(config$path$figures, "supp_neighbour_data.svg"),
    plot = all,
    width = 15,
    height = 13,
    units = "cm",
    dpi = 300
)


# Plot 2: Song frequency ----------------------------------------------------


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


classinfo <- manual_labels |>
    tidyr::separate(class_id,
        into = c("pnum"),
        sep = "_",
        remove = FALSE,
        extra = "drop"
    ) |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4)) |>
    dplyr::group_by(year) |>
    dplyr::mutate( # add count of total class_label (alread in the df) per year
        n_year = length(class_label)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(class_label, year) |>
    # for each year count how many pnums have a class_label
    dplyr::mutate(
        year_freq =
            length(unique(pnum)), rare = ifelse(year_freq < 5, TRUE, FALSE),
        # calculate the relative frequency within each year
        rel_freq = year_freq / n_year
    ) |>
    dplyr::ungroup()



# plot freq vs freq_next_year
freqplot <- freq_by_year |>
    dplyr::arrange(class_label) |>
    ggplot(aes(x = stype_freq, y = stype_freq_next_year)) +
    # add points with some jitter
    geom_point(
        position = position_jitter(width = 0.03, height = 0.03),
        alpha = .35, shape = 21, stroke = NA, size = 2, fill = titpalette(2)[2]
    ) +
    geom_ribbon(stat = "smooth", fill = titpalette(2)[2], alpha = .1) +
    geom_smooth(
        method = "loess",
        se = FALSE,
        color = colorspace::darken(titpalette(2)[2], .2),
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
        aspect.ratio = 1
    ) +
    labs(
        x = expression(log[10] ~ " Frequency in Year"),
        y = expression(log[10] ~ " Frequency in Year + 1")
    )



# plot distribution of relative frequency of each unique class_label
class_dist <- classinfo |>
    ggplot(aes(x = reorder(class_label, rel_freq), y = log10(rel_freq), fill = year)) +
    geom_bar(stat = "identity") +
    # reverse y axis and disable scientific notation
    scale_fill_manual(values = titpalette(3)) +
    # reverse both axes
    scale_x_discrete(limits = rev, expand = c(0.01, 0)) +
    titheme() +
    # aspect ratio to .4
    theme(aspect.ratio = 1) +
    # remove x axis tick labels
    theme(axis.text.x = element_blank()) +
    # limit y axis between -160 and 0
    scale_y_continuous(
        limits = c(-160, 0),
        labels = scales::math_format(10^.x), expand = c(0, 0)
    ) +
    labs(
        x = "Class label",
        y = expression(log[10] ~ " Frequency"),
        fill = "Year"
    )


fullfreqs = freqplot + class_dist

ggplot2::ggsave(
    filename = file.path(config$path$figures, "supp_song_frequencies.svg"),
    plot = fullfreqs,
    width = 15,
    height = 9,
    units = "cm",
    dpi = 300
)







# Plot 3: Measuring cultural diversity --------------------------------------



# plot n_current_songs vs n_unique_current_songs
neighbour_data |>
    ggplot(aes(x = prop_rare, y = novelty)) +
    geom_point(alpha = .35) +
    geom_smooth(method = "lm", se = FALSE) +
    titheme() +
    theme(
        aspect.ratio = 1,
    )

# plot novelty vs prop_rare
neighbour_data |>
    ggplot(aes(x = novelty, y = prop_rare)) +
    geom_point(alpha = .35) +
    geom_smooth(method = "lm", se = FALSE) +
    titheme()


colors <- c("#FFA500", "#FF8C00")

# Plot 1: Novelty vs Proportion Rare
p1 <- neighbour_data |>
    ggplot(aes(x = prop_rare, y = novelty)) +
    geom_point(fill = colors[1], alpha = .35, shape = 21, stroke = NA, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = colorspace::darken(colors[1], .2), linetype = 1) +
    titheme() +
    theme(
        panel.grid.major.x = element_line(
            colour = "#ececec", linewidth = 0.5,
            linetype = 1
        ),
        panel.grid.minor.x = element_blank()
    ) +
    coord_fixed(ratio = 1) +
    labs(x = "Proportion Rare", y = "Novelty")

# Plot 2: Number of Song Types vs Diversity
p2 <- neighbour_data |>
    ggplot(aes(x = n_unique_current_songs, y = diversity)) +
    geom_point(fill = colors[2], alpha = .35, shape = 21, stroke = NA, size = 2) +
    geom_smooth(method = "loess", se = FALSE, color = colorspace::darken(colors[2], .2), linetype = 1) +
    titheme() +
    theme(
        panel.grid.major.x = element_line(
            colour = "#ececec", linewidth = 0.5,
            linetype = 1
        ),
        panel.grid.minor.x = element_blank()
    ) +
    coord_fixed(ratio = 1) +
    labs(x = "Number of Song Types", y = "Diversity")

# Plot 3: Number of song types vs novelty
p3 <- neighbour_data |>
    ggplot(aes(x = n_unique_current_songs, y = novelty)) +
    geom_point(fill = colors[2], alpha = .35, shape = 21, stroke = NA, size = 2) +
    geom_smooth(method = "loess", se = FALSE, color = colorspace::darken(colors[2], .2), linetype = 1) +
    titheme() +
    theme(
        panel.grid.major.x = element_line(
            colour = "#ececec", linewidth = 0.5,
            linetype = 1
        ),
        panel.grid.minor.x = element_blank()
    ) +
    coord_fixed(ratio = 1) +
    labs(x = "Number of Song Types", y = "Novelty")

# Combine plots using patchwork
all <- p1 + p2 + p3

ggsave(
    filename = file.path(config$path$figures, "supp_cultural_mets.svg"),
    plot = all,
    width = 17,
    height = 13,
    units = "cm",
    dpi = 300
)
