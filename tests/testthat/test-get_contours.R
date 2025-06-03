test_that("Returns error because of invalid geometries", {
  df <- example_df
  expect_error(get_contours(df, id_var = name, progress = FALSE))
})

test_that("Returns an sf data.frame", {
  df <- example_df
  expect_contains(
    get_contours(df, id_var = name,
                 invalid_geom = "fix", progress = FALSE) |>
      class(), c("sf", "data.frame")
    )
})

test_that("Returns as sf data.frame when by_period is TRUE", {
  df <- example_df
  expect_contains(
    get_contours(df, id_var = name, by_period = TRUE, period_var = year,
                 invalid_geom = "exclude", progress = FALSE) |>
      class(), c("sf", "data.frame")
  )
})
