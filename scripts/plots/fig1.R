# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])

# LOAD DATA ──────────────────────────────────────────────────────────────── #

age_effect <- readRDS(file.path(config$path$figures, "age_m_1.rds"))
dispersal_effect <- readRDS(file.path(config$path$figures, "disp_m_1.rds"))
immigration_effect <- readRDS(file.path(config$path$figures, "imm_m_1.rds"))


# BUILD THE PLOT ─────────────────────────────────────────────────────────── #
fig1 <- age_effect + dispersal_effect + immigration_effect



# SAVE THE PLOT ──────────────────────────────────────────────────────────── #
ggsave(
    file.path(config$path$figures, "fig1.svg"),
    plot = fig1,
    bg = "transparent",
    device = svglite::svglite,
    width = 15,
    height = 18,
    units = "cm",
    dpi = 300
)
