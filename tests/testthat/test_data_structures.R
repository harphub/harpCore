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
  geolist(lapply(
    1:len,
    function(x) meteogrid::as.geofield(array(runif(25), c(5, 5)), dom)
  ))
}
test_df <- data.frame(
  valid_dttm   = as_dttm(seq_dttm(2021010100, 2021010123)),
  point_det    = runif(24),
  point_mbr000 = runif(24),
  point_mbr001 = runif(24)
)
test_df[["grid_det"]]    <- make_geolist(24)
test_df[["grid_mbr000"]] <- make_geolist(24)
test_df[["grid_mbr001"]] <- make_geolist(24)

point_df <- data.frame(fcst_model = rep("point", 24))
grid_df <- data.frame(fcst_model = rep("grid", 24))

main_class <- c("harp_df", "tbl_df", "tbl", "data.frame")

# Tests
test_that("as_harp_df throws errors when needed", {
  # No valid_dttm column
  expect_error(
    as_harp_df(test_df[-1]),
    "Data frame must have `valid_dttm` column."
  )
  # Not a data frame
  expect_error(as_harp_df(2), "no applicable method")
})

test_that("as_harp_df adds the correct classes", {
  test_df[["fcst"]] <- test_df[["point_det"]]
  expect_equal(
    as_harp_df(test_df[c(1, 2)]),
    structure(cbind(point_df, test_df[c(1, 8)]), class = c("harp_det_point_df", "harp_point_df", main_class))
  )
  expect_equal(
    as_harp_df(test_df[c(1, 3, 4)]),
    structure(test_df[c(1, 3, 4)], class = c("harp_ens_point_df", "harp_point_df", main_class))
  )
  test_df[["fcst"]] <- test_df[["grid_det"]]
  expect_equal(
    as_harp_df(test_df[c(1, 5)]),
    structure(cbind(grid_df, test_df[c(1, 8)]), class = c("harp_det_grid_df", "harp_grid_df", main_class))
  )
  expect_equal(
    as_harp_df(test_df[c(1, 6, 7)]),
    structure(test_df[c(1, 6, 7)], class = c("harp_ens_grid_df", "harp_grid_df", main_class))
  )
})

test_df <- test_df[c(1, 6, 7)]

# Tests for harp_list
test_that("as_harp_list throws error for non harp_df", {
  expect_error(as_harp_list(a = 2), "No data to convert to a <harp_list>")
  expect_error(as_harp_list(a = 2, b = 2), "No data to convert to a <harp_list>")
  expect_error(as_harp_list(a = test_df, b = test_df), "Not all elements are valid")
  expect_error(as_harp_list(a = test_df, b = 2), "Not all elements are valid")
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

test_that("as_det returns the correct member", {
  expect_equal(
    as_det(ens_point_df, 1),
    structure(
      dplyr::transmute(
        ens_point_df,
        fcst_model = "point",
        .data$fcst_dttm,
        .data$lead_time,
        .data$valid_dttm,
        .data$SID,
        fcst = .data$point_mbr001
      ),
      class = gsub("harp_ens_point_df", "harp_det_point_df", class(ens_point_df))
    )
  )
  expect_equal(
    as_det(ens_grid_df, 0),
    structure(
      dplyr::transmute(
        ens_grid_df,
        fcst_model = "grid",
        .data$fcst_dttm,
        .data$lead_time,
        .data$valid_dttm,
        fcst = .data$grid_mbr000
      ),
      class = gsub("harp_ens_grid_df", "harp_det_grid_df", class(ens_grid_df))
    )
  )
  expect_equal(
    as_det(ens_point_df, "point_mbr000"),
    structure(
      dplyr::transmute(
        ens_point_df,
        fcst_model = "point",
        .data$fcst_dttm,
        .data$lead_time,
        .data$valid_dttm,
        .data$SID,
        fcst = .data$point_mbr000
      ),
      class = gsub("harp_ens_point_df", "harp_det_point_df", class(ens_point_df))
    )
  )
  expect_equal(
    as_det(ens_point_list, 0),
    structure(
      mapply(
        function(x, y) {
          mbr_name <- paste0(y, "_mbr000")
          structure(
            tibble::tibble(
              fcst_model = y,
              fcst_dttm = x$fcst_dttm,
              lead_time = x$lead_time,
              valid_dttm = x$valid_dttm,
              SID = x$SID,
              fcst = x[[mbr_name]]
            ),
            class = gsub("harp_ens_point_df", "harp_det_point_df", class(x))
          )
        },
        ens_point_list,
        names(ens_point_list),
        SIMPLIFY = FALSE
      ),
      class = c("harp_list", "list")
    )
  )
  expect_equal(
    as_det(ens_grid_list, 0),
    structure(
      mapply(
        function(x, y) {
          mbr_name <- paste0(y, "_mbr000")
          structure(
            tibble::tibble(
              fcst_model = y,
              fcst_dttm = x$fcst_dttm,
              lead_time = x$lead_time,
              valid_dttm = x$valid_dttm,
              fcst = x[[mbr_name]]
            ),
            class = gsub("harp_ens_grid_df", "harp_det_grid_df", class(x))
          )
        },
        ens_grid_list,
        names(ens_point_list),
        SIMPLIFY = FALSE
      ),
      class = c("harp_list", "list")
    )
  )

})

