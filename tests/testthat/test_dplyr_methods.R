# test dplyr methods

add_first_col <- function(df, name, value) {
  df[[name]] <- value
  df[c(ncol(df), 1:(ncol(df) - 1))]
}

mutate_col <- function(df, col, func) {
  df[[col]] <- func(df[[col]])
  df
}

rename_cols <- function(df, pattern, replace = "") {
  colnames(df) <- gsub(pattern, replace, colnames(df))
  df
}

## bind
test_that("bind gives the correct data frame", {
  expect_equal(
    bind(det_point_list),
    rbind(det_point_list[[1]], det_point_list[[2]])
  )
  expect_equal(
    bind(det_point_list, "mname"),
    rbind(
      add_first_col(det_point_list[[1]], "mname", names(det_point_list)[1]),
      add_first_col(det_point_list[[2]], "mname", names(det_point_list)[2])
    )
  )
  expect_equal(
    bind(ens_point_list),
    rbind(
      pivot_members(ens_point_list[[1]]), pivot_members(ens_point_list[[2]])
    )
  )
  expect_equal(
    bind(det_grid_list),
    mutate_col(
      rbind(
        mutate_col(det_grid_list[[1]], "fcst", unclass),
        mutate_col(det_grid_list[[2]], "fcst", unclass)
      ),
      "fcst", geolist
    )
  )
  expect_equal(
    bind(ens_grid_list),
    mutate_col(
      rbind(
        mutate_col(pivot_members(ens_grid_list[[1]]), "fcst", unclass),
        mutate_col(pivot_members(ens_grid_list[[2]]), "fcst", unclass)
      ),
      "fcst", geolist
    )
  )
})


