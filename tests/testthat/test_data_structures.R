# Tests for data structures
#
# Test data
dom <- structure(
  list(
    projection = list(
      proj  = "lcc",
      lon_0 = 10.74,
      lat_1 = 59.91,
      lat_2 = 59.91,
      R     = 6371229
    ),
    nx = 5,
    ny = 5,
    dx = 50000,
    dy = 50000,
    clonlat = c(10.74, 59.91)
  ),
  class = "geodomain"
)
make_geolist <- function(len) {
  as_geolist(lapply(
    1:len,
    function(x) meteogrid::as.geofield(array(runif(25), c(5, 5)), dom)
  ))
}
test_df <- data.frame(
  validdate = as_datetime(seq_dates(2021010100, 2021010123)),
  point_det    = runif(24),
  point_mbr000 = runif(24),
  point_mbr001 = runif(24)
)
test_df[["grid_det"]]    <- make_geolist(24)
test_df[["grid_mbr000"]] <- make_geolist(24)
test_df[["grid_mbr001"]] <- make_geolist(24)

main_class <- c("harp_df", "tbl_df", "tbl", "data.frame")

# Tests
test_that("as_harp_df throws errors when needed", {
  # No validdate column
  expect_error(
    as_harp_df(test_df[-1]),
    "Data frame must have `validdate` column."
  )
  # Not a data frame
  expect_error(as_harp_df(2), "no applicable method")
})

test_that("as_harp_df adds the correct classes", {
  expect_equal(
    as_harp_df(test_df[c(1, 2)]),
    structure(test_df[c(1, 2)], class = c("harp_det_point_df", "harp_point_df", main_class))
  )
  expect_equal(
    as_harp_df(test_df[c(1, 3, 4)]),
    structure(test_df[c(1, 3, 4)], class = c("harp_ens_point_df", "harp_point_df", main_class))
  )
  expect_equal(
    as_harp_df(test_df[c(1, 5)]),
    structure(test_df[c(1, 5)], class = c("harp_det_grid_df", "harp_grid_df", main_class))
  )
  expect_equal(
    as_harp_df(test_df[c(1, 6, 7)]),
    structure(test_df[c(1, 6, 7)], class = c("harp_ens_grid_df", "harp_grid_df", main_class))
  )
})

# Tests for harp_list
test_that("as_harp_list throws error for non harp_df", {
  expect_error(as_harp_list(a = 2), "All ... must be `harp_df` data frames")
  expect_error(as_harp_list(a = 2, b = 2), "All ... must be `harp_df` data frames")
  expect_error(as_harp_list(a = test_df, b = test_df), "All ... must be `harp_df` data frames")
  expect_error(as_harp_list(a = test_df, b = 2), "All ... must be `harp_df` data frames")
})

test_that("as_harp_list throws error when arguments aren't named", {
  expect_error(as_harp_list(2), "All ... must be named")
  expect_error(as_harp_list(2, 2), "All ... must be named")
  expect_error(as_harp_list(list(2)), "All ... must be named")
  expect_error(as_harp_list(list(2, 2)), "All ... must be named")
  expect_error(as_harp_list(as_harp_df(test_df)), "All ... must be named")
  expect_error(as_harp_list(list(as_harp_df(test_df))), "All ... must be named")
})

test_that("as_harp_list returns a harp_list", {
  expect_equal(
    as_harp_list(a = as_harp_df(test_df)),
    structure(list(a = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(list(a = as_harp_df(test_df))),
    structure(list(a = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(a = as_harp_df(test_df), b = as_harp_df(test_df)),
    structure(list(a = as_harp_df(test_df), b = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(list(a = as_harp_df(test_df), b = as_harp_df(test_df))),
    structure(list(a = as_harp_df(test_df), b = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
})

test_that("Concatenation works for harp_lists", {
  expect_equal(
    c(as_harp_list(a = as_harp_df(test_df)), as_harp_list(b = as_harp_df(test_df))),
    structure(list(a = as_harp_df(test_df), b = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    c(
      as_harp_list(a = as_harp_df(test_df), b = as_harp_df(test_df)),
      as_harp_list(c = as_harp_df(test_df), d = as_harp_df(test_df))
    ),
    structure(
      list(
        a = as_harp_df(test_df), b = as_harp_df(test_df),
        c = as_harp_df(test_df), d = as_harp_df(test_df)
      ),
      class = c("harp_list", "list")
    )
  )
})

test_that("Single square bracket extracts a harp_list", {
  expect_equal(
    as_harp_list(a = as_harp_df(test_df))[1],
    structure(list(a = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(a = as_harp_df(test_df), b = as_harp_df(test_df))[1],
    structure(list(a = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(a = as_harp_df(test_df), b = as_harp_df(test_df))[2],
    structure(list(b = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(a = as_harp_df(test_df), b = as_harp_df(test_df), c = as_harp_df(test_df))[1:2],
    structure(list(a = as_harp_df(test_df), b = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
  expect_equal(
    as_harp_list(a = as_harp_df(test_df), b = as_harp_df(test_df), c = as_harp_df(test_df))[c(1, 3)],
    structure(list(a = as_harp_df(test_df), c = as_harp_df(test_df)), class = c("harp_list", "list"))
  )
})
