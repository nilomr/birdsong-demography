# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, blues])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD MODELS ────────────────────────────────────────────────────────────


model_names <- c(
    "rep_m_1", "rep_m_1.1", "rep_m_1.2", "repnov_m_1", "repnov_m_1.1",
    "repnov_m_1.2",
    "disp_m_1", "imm_m_1", "age_m_1",
    "nov_m_1", "div_m_1", "nov_m_2", "nov_m_2.1", "div_m_2", "div_m_2.1",
    "turn_m_1", "turn_m_2"
)

models <- lapply(model_names, function(x) {
    readRDS(file.path(
        config$path$fits,
        paste0(x, ".rds")
    ))
})


models <- list(
    age_m_1 = age_m_1,
    disp_m_1 = disp_m_1,
    imm_m_1 = imm_m_1,
    nov_m_1 = nov_m_1,
    div_m_1 = div_m_1,
    nov_m_2 = nov_m_2,
    nov_m_2.1 = nov_m_2.1,
    div_m_2 = div_m_2,
    div_m_2.1 = div_m_2.1,
    turn_m_1 = turn_m_1,
    turn_m_2 = turn_m_2,
    rep_m_1 = rep_m_1,
    rep_m_1.1 = rep_m_1.1,
    rep_m_1.2 = rep_m_1.2,
    repnov_m_1 = repnov_m_1,
    repnov_m_1.1 = repnov_m_1.1,
    repnov_m_1.2 = repnov_m_1.2
)

models <- models[model_names]
model_names <- names(models)


# MODEL CHECKS ───────────────────────────────────────────────────────────── #


c2 <- titpalette(2)[2]
bayesplot::color_scheme_set(c(
    c2, c2, c2, c2,
    "#292929", "#292929"
))


pp_checks <- list()

for (model_name in model_names) {
    model <- models[[model_name]]
    pp_check <- brms::pp_check(model, ndraws = 20) +
        titheme() +
        labs(
            y = "Density", x = "Response",
            subtitle = paste0("Model: ", model_name)
        ) +
        theme(
            legend.justification = c(1, 1),
            aspect.ratio = 1,
            panel.grid.major = element_line(
                color = "#ececec",
                linewidth = 0.5,
                linetype = 1
            ),
            legend.spacing.y = unit(0.3, "lines")
        )

    pp_checks[[model_name]] <- pp_check
}

# plot together using patchwork
all_ppchecks <- pp_checks$rep_m_1 + pp_checks$rep_m_1.1 + pp_checks$rep_m_1.2 +
    pp_checks$repnov_m_1 + pp_checks$repnov_m_1.1 + pp_checks$repnov_m_1.2 +
    pp_checks$disp_m_1 + pp_checks$imm_m_1 + pp_checks$age_m_1 +
    pp_checks$nov_m_1 + pp_checks$div_m_1 + pp_checks$nov_m_2 +
    pp_checks$nov_m_2.1 + pp_checks$div_m_2 + pp_checks$div_m_2.1 +
    pp_checks$turn_m_1 + pp_checks$turn_m_2 +
    plot_layout(ncol = 4, nrow = 5, guides = "collect") +
    plot_annotation(tag_levels = "A", title = "Posterior Predictive Checks") & titheme()


ggsave(
    file.path(config$path$figures, "ppchecks.svg"),
    plot = all_ppchecks,
    device = svglite::svglite,
    width = 17,
    height = 20,
    units = "cm",
    dpi = 300,
    # add a white background
    bg = "white",
    scale = 1.3
)
