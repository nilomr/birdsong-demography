# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette, reds, blues, yellows])
box::use(R / utils[og_scale, match_grid, partial_residuals])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── READ IN DATA ───────────────────────────────────────────────────────────

div_m_25_margeffs <- readRDS(file.path(config$path$figures, "div_m_25_margeffs.rds"))
repertoire_plot <- readRDS(file.path(config$path$figures, "repertoire_plot.rds"))


# ──── MAIN ───────────────────────────────────────────────────────────────────


# Join using patchwork

supdiv <- (div_m_25_margeffs +
    labs(x = "Marginal Effect\non Cultural Diversity")) +
    (repertoire_plot +
        labs(x = "Marginal Effect\non Repertoire Size")) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 12, face = "bold"))


ggsave(
    filename = file.path(config$path$figures, "supp_abs_diversity.svg"),
    plot = supdiv,
    bg = "transparent",
    device = svglite::svglite,
    width = 19,
    height = 12,
    units = "cm",
    dpi = 300
)
