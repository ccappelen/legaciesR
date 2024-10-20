
test_that("Returns valid geometries", {
  df <- example_df
  expect_true(all(st_is_valid(fix_invalid(df, progress = FALSE, report = FALSE))))
})


