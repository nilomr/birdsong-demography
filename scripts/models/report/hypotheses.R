# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds, blues, yellows])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD DATA ──────────────────────────────────────────────────────────────

# Individual (dyad) models
age_m_1 <- readRDS(file.path(config$path$fits, "age_m_1.rds"))
disp_m_1 <- readRDS(file.path(config$path$fits, "disp_m_1.rds"))
imm_m_1 <- readRDS(file.path(config$path$fits, "imm_m_1.rds"))


# Turnover
turn_m_1 <- readRDS(file.path(config$path$fits, "turn_m_1.rds"))



# Are immigrant birds dissimilar to resident birds? ──────────────────────── #

print(brms::hypothesis(
    imm_m_1,
    "resident_statusOne - resident_statusBoth < 0"
)$hypothesis, digits = 2)


# Effect of territorial distance // best get this from separate model, although
# estimate is very close
print(brms::hypothesis(
    disp_m_1,
    "nest_distance  < 0"
)$hypothesis, digits = 1)


# Is the repertoire of immigrants larger?
print(brms::hypothesis(rep_m_1, "immigrantTRUE > 0")$hypothesis, digits = 2)



# ──── TURNOVER ───────────────────────────────────────────────────────────────

# Load neighbourhood data
neighbour_df <- read_csv_file(
    file.path(config$path$derived_data, "neighbour_data.csv")
)

# calculate the average and sd prop_shared by year
neighbour_df |>
    dplyr::group_by(year) |>
    dplyr::summarise(
        mean_prop_shared = mean(prop_shared, na.rm = TRUE),
        sd_prop_shared = sd(prop_shared, na.rm = TRUE)
    )

# Population-level change
manual_labels <- read_csv_file(
    file.path(config$path$derived_data, "manual_labels.csv")
)

myears <- manual_labels |>
    dplyr::mutate(year = stringr::str_sub(class_id, 1, 4))
myears_list <- lapply(unique(myears$year), function(x) myears$class_label[myears$year == x])
myears_list <- lapply(myears_list, unique)
dplyr::tibble(
    prop_shared_1_2 = 1 - length(intersect(myears_list[[1]], myears_list[[2]]))
    / length(myears_list[[2]]),
    prop_shared_2_3 = 1 - length(intersect(myears_list[[2]], myears_list[[3]]))
    / length(myears_list[[3]]),
    prop_shared_1_3 = 1 - length(intersect(myears_list[[1]], myears_list[[3]]))
    / length(myears_list[[3]])
) |> round(2)

main_data <- read_csv_file(
    file.path(config$path$derived_data, "main.csv")
)

prop_shared <- sapply(unique(main_data$year), function(y) {
    fathers <- main_data$father[main_data$year == y]
    prev_year_fathers <- main_data$father[main_data$year == y - 3]
    1 - sum(fathers %in% prev_year_fathers) / length(fathers)
})

prop_shared_df <- data.frame(year = unique(main_data$year), prop_shared = prop_shared)


# Marginal estimate for the effect of individual turnover
print(brms::hypothesis(turn_m_1, "prop_same_birds = 0")$hypothesis, digits = 2)


# Hypothesis test that immigrant birds are more dissimilar than resident birds
# when birds are in the same area and are one year apart
hy1m1 <- marginaleffects::predictions(
    imm_m_1,
    newdata = marginaleffects::datagrid(
        nest_distance = min(imm_m_1_data_std$nest_distance),
        resident_status = unique(imm_m_1_data_std$resident_status),
        year_born_diff = "1",
        year = "2020"
    )
) |>
    marginaleffects::posterior_draws(shape = "DxP") |>
    brms::hypothesis("b3 - b1 < 0")

print(hy1m1$hypothesis, digits = 2)
