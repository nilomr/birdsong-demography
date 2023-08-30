# ──── CONFIGURATION ──────────────────────────────────────────────────────────

config <- config::get()
box::use(R / io[read_csv_file])
box::use(R / rplot[titheme, titpalette])
box::use(R / utils[og_scale, match_grid, get_spatial_preds, get_raster])
box::use(patchwork[...])
box::use(ggplot2[...])


# ──── LOAD DATA ──────────────────────────────────────────────────────────────


# Check that we are indeed getting rid of the spatial autocorrelation with the
# gaussian process

# Load the models
turn_m_0 <- readRDS(file.path(config$path$fits, "turn_m_0.rds"))
turn_m_1 <- readRDS(file.path(config$path$fits, "turn_m_1.rds"))


turn_m_0_check <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(turn_m_0)),
    observedResponse = turn_m_0$data$prop_shared,
    fittedPredictedResponse = apply(t(brms::posterior_epred(turn_m_0)), 1, mean),
    integerResponse = FALSE
)

resds <- DHARMa::recalculateResiduals(turn_m_0_check, sel = turn_m_0$data$year == 2022)
DHARMa::testSpatialAutocorrelation(resds,
    x = turn_m_1$data[turn_m_1$data$year == 2022, ]$x,
    y = turn_m_1$data[turn_m_1$data$year == 2022, ]$y,
    plot = FALSE
)

turn_m_1_check <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(turn_m_1)),
    observedResponse = turn_m_1$data$prop_shared,
    fittedPredictedResponse = apply(t(brms::posterior_epred(turn_m_1)), 1, mean),
    integerResponse = FALSE
)

resds <- DHARMa::recalculateResiduals(turn_m_1_check, sel = turn_m_1$data$year == 2022)
DHARMa::testSpatialAutocorrelation(resds,
    x = turn_m_1$data[turn_m_1$data$year == 2022, ]$x,
    y = turn_m_1$data[turn_m_1$data$year == 2022, ]$y,
    plot = FALSE
)
