#
# test_that("Returns error with no id_vars selected", {
#   df <- example_df[example_df$id == sample(unique(example_df$id), 1), ]
#   expect_error(contour_polygons(df))
# })
#
# test_that("Returns an sf data.frame", {
#   df <- example_df[example_df$id == sample(unique(example_df$id), 1), ]
#   expect_contains(contour_polygons(df, id_vars = id) |> class(), c("sf", "data.frame"))
#   expect_contains(contour_polygons(df, id_vars = c(id, year)) |> class(), c("sf", "data.frame"))
# })
#
# test_that("Returns error if nmap_threshold is too high", {
#   df <- example_df[example_df$id == sample(unique(example_df$id), 1), ][1:4, ]
#   expect_error(contour_polygons(df, id_vars = id, nmap_threshold = 5))
# })
#
# test_that("Returns error if invalid geometry and invalid_geom = 'none'", {
#   df <- example_df
#   df <- df |>
#     filter(id == df$id[which(!st_is_valid(df))])
#   expect_error(contour_polygons(df, id_vars = name, invalid_geom = "none"))
# })
#
#
#
#
#
