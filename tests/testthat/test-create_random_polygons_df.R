
test_that("Returns class 'data.frame' and 'sf'", {
  expect_contains(
    create_random_polygons_df(n = 2, nmap_min = 5, nmap_max = 50, progress = FALSE) |>
      class(),
    c("data.frame", "sf")
  )
})


