# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid])
box::use(patchwork[...])
box::use(ggplot2[...])

# LOAD DATA ──────────────────────────────────────────────────────────────── #

age_effect <- readRDS(file.path(config$path$figures, "individual_age.rds"))
dispersal_effect <- readRDS(file.path(config$path$figures, "individual_dispersal.rds"))
immigration_effect <- readRDS(file.path(config$path$figures, "individual_immigration.rds"))


# BUILD THE PLOT ─────────────────────────────────────────────────────────── #
fig1 <- age_effect + dispersal_effect + immigration_effect



# SAVE THE PLOT ──────────────────────────────────────────────────────────── #
ggsave(
    file.path(config$path$figures, "fig1.svg"),
    plot = fig1,
    width = 15,
    height = 18,
    units = "cm",
    dpi = 300
)
