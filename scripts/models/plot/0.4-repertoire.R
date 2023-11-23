# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds, blues, yellows])
box::use(R / utils[og_scale, match_grid, partial_residuals])
box::use(patchwork[...])
box::use(ggplot2[...])

# ──── LOAD DATA ──────────────────────────────────────────────────────────────


rep_m_1 <- readRDS(file.path(config$path$fits, "rep_m_1.rds"))
rep_m_1.1 <- readRDS(file.path(config$path$fits, "rep_m_1.1.rds"))
rep_m_1.2 <- readRDS(file.path(config$path$fits, "rep_m_1.2.rds"))

repnov_m_1 <- readRDS(file.path(config$path$fits, "repnov_m_1.rds"))
repnov_m_1.1 <- readRDS(file.path(config$path$fits, "repnov_m_1.1.rds"))
repnov_m_1.2 <- readRDS(file.path(config$path$fits, "repnov_m_1.2.rds"))



# ──── MARGINAL EFFECTS OF IMMIGRATION AND DISPERSAL ─────────────────────────


# Does dispersal reduce the size of a bird's repertoire?
rep_m_1.1_draws <- brms::hypothesis(rep_m_1.1, "dispersal_distance < 0")$samples |>
    dplyr::as_tibble() |>
    dplyr::mutate(term = "dispersal_distance", model = "repertoire-size")

# Does immigration increase the size of a bird's repertoire?
rep_m_1_draws <- brms::hypothesis(rep_m_1, "immigrantTRUE > 0")$samples |>
    dplyr::as_tibble() |>
    dplyr::mutate(term = "immigrant", model = "repertoire-size")

# Does age affect the size of a bird's repertoire?
rep_m_1.2_draws <- brms::hypothesis(rep_m_1.2, "age > 0")$samples |>
    dplyr::as_tibble() |>
    dplyr::mutate(term = "age", model = "repertoire-size")


# Does dispersal reduce the novelty of a bird's repertoire?
repnov_m_1.1_draws <- brms::hypothesis(repnov_m_1.1, "dispersal_distance > 0")$samples |>
    dplyr::as_tibble() |>
    dplyr::mutate(
        H1 = -H1,
        term = "dispersal_distance",
        model = "repertoire-novelty"
    )

# using marginaleffects instead

# Does imigration increase the novelty of a bird's repertoire?
repnov_m_1_draws <- brms::hypothesis(repnov_m_1, "immigrantTRUE > 0")$samples |>
    dplyr::as_tibble() |>
    dplyr::mutate(
        H1 = -H1,
        term = "immigrant", model = "repertoire-novelty"
    )


# Does imigration increase the novelty of a bird's repertoire?
repnov_m_1.2_draws <- brms::hypothesis(repnov_m_1.2, "age > 0")$samples |>
    dplyr::as_tibble() |>
    dplyr::mutate(
        H1 = -H1,
        term = "age", model = "repertoire-novelty"
    )


# ──── JOIN AND PLOT ─────────────────────────────────────────────────────────

all_draws <- dplyr::bind_rows(
    rep_m_1.1_draws,
    rep_m_1_draws,
    rep_m_1.2_draws,
    repnov_m_1.1_draws,
    repnov_m_1_draws,
    repnov_m_1.2_draws
) |>
    dplyr::rename(draw = H1)

fill_colors <- c(
    "dispersal_distance" = blues[1],
    "immigrant" = yellows[1],
    "age" = reds[1]
)

prepertoire_plot <-
    all_draws |>
    ggplot(aes(x = draw, y = term, fill = term, color = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_halfeye(aes(slab_alpha = model)) +
    scale_fill_manual(
        values = fill_colors
    ) +
    ggdist::scale_slab_alpha_discrete(
        range = c(.9, .4),
        labels = c("Repertoire\nNovelty", "Repertoire\nSize"),
        limits = c("repertoire-novelty", "repertoire-size"),
        guide = guide_legend(
            byrow = TRUE, title.position = "top",
            keywidth = 0.8, keyheight = 0.8,
            title = "Outcome",
            override.aes = list(linetype = 0, shape = NA)
        )
    ) +
    scale_color_manual(
        values = sapply(fill_colors, colorspace::darken, .2),
    ) +
    scale_y_discrete(
        limits = rev(c("dispersal_distance", "immigrant", "age")),
        expand = c(0.1, 0.5),
        labels = c(
            "dispersal_distance" = "Dispersal\nDistance",
            "immigrant" = "Immigration",
            "age" = "Age"
        )
    ) +
    scale_x_continuous(
        limits = c(-1, 1),
        expand = c(0, 0),
        breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    labs(x = "Marginal Effect", y = "") +
    guides(color = "none", fill = "none") +
    titheme() +
    theme(
        legend.box.just = "right",
        aspect.ratio = 1,
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.spacing.y = unit(.2, "cm")
    )

# save to file
saveRDS(prepertoire_plot, file.path(config$path$figures, "repertoire_plot.rds"))
