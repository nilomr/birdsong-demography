# Hypothesis test that immigrant birds are more dissimilar than resident birds
# when birds are in the same area and are one year apart
hy1m1 <- marginaleffects::predictions(
    imm_m_1,
    newdata = marginaleffects::datagrid(
        nest_distance = min(imm_m_1_data_std$nest_distance),
        resident_status = unique(imm_m_1_data_std$resident_status),
        year_born_diff = "1",
        year = "2020"
    )
) |>
    marginaleffects::posterior_draws(shape = "DxP") |>
    brms::hypothesis("b3 - b1 < 0")

print(hy1m1$hypothesis, digits = 2)
