# MODEL CHECKS ───────────────────────────────────────────────────────────── #


c2 <- titpalette(2)[2]
bayesplot::color_scheme_set(c(
    c2, c2, c2, c2,
    "#292929", "#292929"
))

age_m_1_ppcheck <- brms::pp_check(age_m_1, type = "stat_2d", ndraws = 1000) +
    titheme() +
    labs(
        y = "SD", x = "Mean",
        title = "Posterior predictive check",
        subtitle = "Model: age_m_1"
    ) +
    theme(
        legend.justification = c(1, 1),
        aspect.ratio = 1,
        panel.grid.major = element_line(
            color = "#ececec",
            linewidth = 0.5,
            linetype = 1
        ),
        legend.spacing.y = unit(0.3, "lines"),
        # make plot background white
        panel.background = element_rect(fill = "white")
    )

ggsave(
    file.path(config$path$figures, "age_m_1_ppcheck.png"),
    plot = age_m_1_ppcheck,
    device = svglite::svglite,
    width = 10,
    height = 10,
    units = "cm",
    dpi = 300,
    # add a white background
    bg = "white",
    scale = 1.3
)
