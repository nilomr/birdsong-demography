# CONFIGURATION ──────────────────────────────────────────────────────────── #

config <- config::get()
box::use(modelsummary[modelsummary])
box::use(kableExtra[...])
box::use(purrr[imap, reduce])

dir.create(config$path$reports, recursive = TRUE, showWarnings = FALSE)

# LOAD DATA ──────────────────────────────────────────────────────────────── #


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

# ──── MODEL DEFINITIONS ──────────────────────────────────────────────────────


model_info <- data.frame(
    model = model_names,
    formula = sapply(models, function(x) as.character(x$formula)[[1]]),
    family = sapply(models, function(x) as.character(x$family[[1]])),
    n_obs = sapply(models, function(x) nrow(x$data)),
    n_groups = sapply(models, function(x) {
        ngrps <- summary(x)$ngrps[[1]]
        if (is.null(ngrps)) {
            return("GP")
        } else {
            return(ngrps)
        }
    })
)

# Convert to a latex table
model_info |>
    kable(
        format = "latex",
        booktabs = T,
        caption = "Model information",
        col.names = c("Model", "Formula", "Family", "Observations", "Groups")
    ) |>
    kable_styling(
        latex_options = c("striped"),
        font_size = 11
    ) |>
    save_kable(file.path(config$path$reports, "model_info.tex"))


# ──── PARAMETER ESTIMATES / HYPOTHESES ───────────────────────────────────────


hypotheses <- list(
    c("natal_distance > 0", "nest_distance < 0"),
    c(
        "resident_statusBoth < resident_statusNeither",
        "resident_statusBoth < resident_statusOne"
    ),
    c(
        "year_born_diff0 > year_born_diff1", "year_born_diff0 > year_born_diff2",
        "year_born_diff0 > year_born_diff3", "year_born_diff0 > year_born_diff4P"
    ),
    c("immigrantTRUE > 0"),
    c("dispersal_distance < 0"),
    c("age > 0"),
    c("-immigrantTRUE < 0"),
    c("dispersal_distance > 0"),
    c("age > 0"),
    c(
        "mean_dispersal_distance < 0", "prop_immigrant < 0", "mean_age > 0",
        "-prop_same_birds > 0"
    ),
    c(
        "mean_dispersal_distance < 0", "prop_immigrant < 0", "mean_age > 0",
        "-prop_same_birds > 0"
    ),
    c("diversity > 0"),
    c("diversity > 0"),
    c(
        "mean_dispersal_distance < 0", "prop_immigrant > 0", "mean_age > 0",
        "-prop_same_birds > 0"
    ),
    c(
        "mean_dispersal_distance < 0", "prop_immigrant > 0", "mean_age > 0",
        "-prop_same_birds > 0"
    ),
    c("prop_same_birds > 0"),
    c(
        "-mean_dispersal_distance < 0", "-prop_immigrant < 0", "-mean_age < 0",
        "prop_same_birds > 0"
    )
)


model_names <- c(
    "disp_m_1", "imm_m_1", "age_m_1",
    "rep_m_1", "rep_m_1.1", "rep_m_1.2", "repnov_m_1", "repnov_m_1.1", "repnov_m_1.2",
    "nov_m_1", "div_m_1",
    "nov_m_2", "nov_m_2.1", "div_m_2", "div_m_2.1", "turn_m_1", "turn_m_2"
)


tab_list <- list()

for (i in seq_along(model_names)) {
    model <- model_names[i]
    h <- hypotheses[[i]]
    tab <- brms::hypothesis(get(model), h, robust = TRUE)$hypothesis |>
        dplyr::select(-Star)
    tab_list[[model]] <- tab
}

# Define readable names
readable_names <- c(
    "natal_distance" = "natal distance",
    "nest_distance" = "nest distance",
    "resident_statusBoth-resident_statusNeither" = "both resident-both immigrant",
    "resident_statusBoth-resident_statusOne" = "both resident-one immigrant",
    "year_born_diff0-year_born_diff1" = "age difference 0-1",
    "year_born_diff0-year_born_diff2" = "age difference 0-2",
    "year_born_diff0-year_born_diff3" = "age difference 0-3",
    "year_born_diff0-year_born_diff4P" = "age difference 0-4+",
    "immigrantTRUE" = "immigrant",
    "dispersal_distance" = "dispersal distance",
    "age" = "age",
    "-immigrantTRUE" = "non-immigrant",
    "mean_dispersal_distance" = "mean dispersal distance",
    "prop_immigrant" = "proportion immigrant",
    "-prop_immigrant" = "proportion immigrant",
    "-mean_dispersal_distance" = "mean dispersal distance",
    "mean_age" = "mean age",
    "-mean_age" = "mean age",
    "prop_same_birds" = "individual turnover",
    "-prop_same_birds" = "individual turnover",
    "diversity" = "diversity",
    "0" = "0"
)

combined_tab <- tab_list |>
    imap(function(x, y) {
        x |>
            mutate(across(where(is.numeric), ~ round(., digits = 3))) |>
            rename_all(~ gsub("\\.", ". ", .)) |>
            select(
                Hypothesis, Estimate, `CI. Lower`, `CI. Upper`,
                `Evid. Ratio`, `Post. Prob`
            ) |>
            mutate(Model = y, Hypothesis = gsub("\\(|\\)", "", Hypothesis)) |>
            select(Model, everything()) |>
            mutate(Hypothesis = sapply(
                strsplit(Hypothesis, " > | < "),
                function(z) {
                    paste(sapply(z, function(w) {
                        readable_names[w]
                    }), collapse = ifelse(grepl(">", Hypothesis), " > ", " < "))
                }
            )) |>
            mutate(Hypothesis = gsub("_", " ", Hypothesis)) |>
            mutate(Estimate = paste0(
                Estimate, " [", `CI. Lower`, ", ",
                `CI. Upper`, "]"
            ))
    }) |>
    reduce(rbind) |>
    # remove all parentheses
    mutate(Hypothesis = gsub("\\(|\\)", "", Hypothesis)) |>
    # escape any < or > symbols, to avoid latex errors
    mutate(Hypothesis = gsub("<", "$<$", Hypothesis)) |>
    mutate(Hypothesis = gsub(">", "$>$", Hypothesis)) |>
    # change "NA" to an infinity symbol
    mutate(Estimate = gsub("NA", "$\\infty$", Estimate)) |>
    select(-c(`CI. Lower`, `CI. Upper`)) |>
    # scape underscores in the model names
    mutate(Model = gsub("_", "\\\\_", Model))


# Add the model name as row headers
combined_tab_with_headers <- combined_tab |>
    kable(format = "latex", booktabs = TRUE, escape = FALSE) |>
    kable_styling(latex_options = c("striped"), font_size = 9) |>
    pack_rows(index = table(factor(combined_tab$Model,
        levels = unique(combined_tab$Model)
    )), escape = FALSE) |>
    # chnage any m_ to m\_ in this string
    gsub("m_", "m\\\\_", x = _) |>
    save_kable(file.path(config$path$reports, "combined_hypotheses.tex"))
