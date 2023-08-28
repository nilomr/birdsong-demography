modelsummary::modelsummary(nm2,
    estimate = "{estimate} [{conf.low}, {conf.high}]",
    title = "Model n2",
    output = file.path(config$path$reports, "modelsummary-nm2.md")
)
