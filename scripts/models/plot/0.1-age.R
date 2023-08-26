# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD DATA ──────────────────────────────────────────────────────────────


age_m_1 <- readRDS(file.path(config$path$fits, "age_m_1.rds"))



# ──── MARGINAL EFFECT OF AGE DIFFERENCE ──────────────────────────────────────

age_m_1_agedraws <- marginaleffects::avg_slopes(age_m_1,
    variables = "year_born_diff",
    type = "response",
    re_formula = NULL,
    newdata = marginaleffects::datagrid(
        year_born_diff = c("0", "1", "2", "3", "4+"),
        nest_distance = mean(age_m_1$data$nest_distance),
        natal_distance =
            mean(age_m_1$data$natal_distance)
    )
) |>
    marginaleffects::posterior_draws() |>
    dplyr::as_tibble()


p1 <- age_m_1_agedraws |>
    ggplot(aes(
        x = draw,
        fill = rev(contrast)
    )) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_slab(alpha = .9) +
    scale_fill_manual(
        labels = c("3+ years", "3 years", "2 years", "1 year"),
        values = reds
    ) +
    titheme() +
    theme(
        aspect.ratio = .4,
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        y = "Density",
        title = "Age Difference",
    )

p2 <- age_m_1_agedraws |>
    ggplot(aes(x = draw, color = rev(contrast))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#858585") +
    ggdist::stat_pointinterval(
        alpha = .7,
        point_size = 2,
        interval_size_range = c(0.7, 1.5),
        position = position_dodge(width = .2, preserve = "single"),
        show.legend = FALSE
    ) +
    titheme() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(
            margin =
                margin(t = 10, r = 0, b = 0, l = 0)
        ),
        aspect.ratio = .1, # remove y axis text:
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    labs(
        x = "Change in Cultural Similarity"
    ) +
    scale_color_manual(values = reds)

fullplot <- (p1 / p2) +
    scale_x_continuous(
        breaks = c(-0.015, -0.010, -0.005, 0, 0.005),
        labels = c(-0.015, -0.010, -0.005, 0, 0.005)
    ) &
    coord_cartesian(xlim = c(-0.015, 0.003)) &
    guides(
        colour = "none",
        fill = guide_legend(
            title = "Difference",
            byrow = TRUE, title.position = "top",
            reverse = TRUE
        )
    ) &
    theme(legend.spacing.y = unit(.1, "cm"))

# save this plot to a r data file so I can open it later to create a combined figure
saveRDS(fullplot, file.path(config$path$figures, "individual_age.rds"))

ggsave(
    file.path(config$path$figures, "individual_age.png"),
    plot = fullplot,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 300
)
