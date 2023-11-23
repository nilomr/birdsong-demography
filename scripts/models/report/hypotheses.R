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

# Repertoire size and novelty
rep_m_1 <- readRDS(file.path(config$path$fits, "rep_m_1.rds"))
rep_m_2 <- readRDS(file.path(config$path$fits, "rep_m_2.rds"))
rep_m_1.1 <- readRDS(file.path(config$path$fits, "rep_m_1.1.rds"))
rep_m_2.1 <- readRDS(file.path(config$path$fits, "rep_m_2.1.rds"))


# Diversity
div_data <- readRDS(file.path(config$path$fits, "div_data.rds"))
nov_m_1 <- readRDS(file.path(config$path$fits, "nov_m_1.rds"))
div_m_1 <- readRDS(file.path(config$path$fits, "div_m_1.rds"))

# Turnover
turn_m_1 <- readRDS(file.path(config$path$fits, "turn_m_1.rds"))



# Are immigrant birds dissimilar to resident birds? ──────────────────────── #

print(brms::hypothesis(
    imm_m_1,
    "resident_statusOne - resident_statusBoth < 0"
)$hypothesis, digits = 2)




# ──── REPERTOIRE SIZE AND NOVELTY AT THE INDIVIDUAL LEVEL ────────────────────

# Does dispersal reduce the size of a bird's repertoire?
print(brms::hypothesis(rep_m_1.1, "dispersal_distance < 0")$hypothesis, digits = 2)

# Does dispersal reduce the novelty of a bird's repertoire,
# adjusting for repertoire size?
print(brms::hypothesis(rep_m_2.1, "- dispersal_distance < 0")$hypothesis, digits = 2)

# Does immigration increase the size of a bird's repertoire?
print(brms::hypothesis(rep_m_1, "immigrantTRUE > 0")$hypothesis, digits = 2)

# Does immigration increase the novelty of a bird's repertoire,
# adjusting for repertoire size?
print(brms::hypothesis(rep_m_2, "immigrantTRUE > 0")$hypothesis, digits = 2)


# ──── DIVERSITY ──────────────────────────────────────────────────────────────


# Novelty
print(brms::hypothesis(nov_m_1, "mean_dispersal_distance < 0")$hypothesis, digits = 2)

marginaleffects::avg_slopes(nov_m_1, re_formula = NULL, ndraws = 1000, type = "response") |> dplyr::mutate_if(is.numeric, round, 3)



# How much is 1 SD in the original scale?
og_scale(div_data, dplyr::tibble(mean_age = 1), v = "mean_age")

mu <- mean(div_data$mean_age)
sigma <- sd(div_data$mean_age)

v <- numeric(2)
for (i in c(min(div_data$mean_age), min(div_data$mean_age) + sigma)) {
    x <- i
    z <- (x - mu) / sigma
    v[i / 100 + 1] <- z
}

abs(v[1] - v[2])

# odds-ratio-scale posterior
marginaleffects::avg_comparisons(
    div_m_1,
    re_formula = NULL, ndraws = 2000,
    variable = "mean_age",
    type = "link"
) |>
    marginaleffects::posteriordraws() |>
    dplyr::mutate(draw = exp(draw) - 1) |>
    ggplot(aes(x = draw)) +
    ggdist::stat_halfeye() +
    geom_vline(xintercept = 0) +
    scale_x_continuous(labels = scales::label_percent()) +
    labs(x = "Percent change in outcome", y = "Density") +
    titheme()



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
