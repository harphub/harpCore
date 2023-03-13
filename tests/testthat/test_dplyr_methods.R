# test dplyr methods

add_first_col <- function(df, name, value) {
  df[[name]] <- value
  df[c(ncol(df), 1:(ncol(df) - 1))]
}

rename_cols <- function(df, pattern, replace = "") {
  colnames(df) <- gsub(pattern, replace, colnames(df))
  df
}

## bind
test_that("bind_dfr gives the correct data frame", {
  expect_equal(
    bind_dfr(det_point_list),
    rbind(
      rename_cols(add_first_col(det_point_list[[1]], "fcst_model", names(det_point_list)[1]), "point_"),
      rename_cols(add_first_col(det_point_list[[2]], "fcst_model", names(det_point_list)[2]), "point_")
    )
  )
  expect_equal(
    bind_dfr(det_point_list, "mname"),
    rbind(
      rename_cols(add_first_col(det_point_list[[1]], "mname", names(det_point_list)[1]), "point_"),
      rename_cols(add_first_col(det_point_list[[2]], "mname", names(det_point_list)[2]), "point_")
    )
  )
  expect_equal(
    bind_dfr(ens_point_list),
    rbind(
      rename_cols(add_first_col(ens_point_list[[1]], "fcst_model", names(ens_point_list)[1]), "point_"),
      rename_cols(add_first_col(ens_point_list[[2]], "fcst_model", names(ens_point_list)[2]), "point_")
    )
  )
  expect_equal(
    bind_dfr(det_grid_list),
    rbind(
      rename_cols(add_first_col(det_grid_list[[1]], "fcst_model", names(det_grid_list)[1]), "grid_"),
      rename_cols(add_first_col(det_grid_list[[2]], "fcst_model", names(det_grid_list)[2]), "grid_")
    )
  )
  expect_equal(
    bind_dfr(ens_grid_list),
    rbind(
      rename_cols(add_first_col(ens_grid_list[[1]], "fcst_model", names(ens_grid_list)[1]), "grid_"),
      rename_cols(add_first_col(ens_grid_list[[2]], "fcst_model", names(ens_grid_list)[2]), "grid_")
    )
  )
})

test_that("bind throws error when geofields in the same column are on different domains", {
  expect_error(
    bind_dfr(
      as_harp_list(
        a = det_grid_df,
        b = dplyr::mutate(det_grid_df, grid_det = as_geolist(lapply(grid_det, meteogrid::subgrid, 1, 4, 1, 4)))
      )
    ),
    "Domain mismatch"
  )
})

test_that("bind can combine columns that don't exist in all data frames", {
  expect_equal(
    bind_dfr(as_harp_list(a = ens_point_df, b = dplyr::mutate(ens_point_df, point_mbr003 = point_mbr001))),
    rename_with(
      as_harp_df(tibble::tibble(
        fcst_model = c(rep("a", nrow(ens_point_df)), rep("b", nrow(ens_point_df))),
        fcst_dttm = rep(ens_point_df$fcst_dttm, 2),
        lead_time = rep(ens_point_df$lead_time, 2),
        valid_dttm = rep(ens_point_df$valid_dttm, 2),
        SID = rep(ens_point_df$SID, 2),
        point_mbr000 = rep(ens_point_df$point_mbr000, 2),
        point_mbr001 = rep(ens_point_df$point_mbr001, 2),
        point_mbr003 = c(rep(NA, nrow(ens_point_df)), ens_point_df$point_mbr001)
      )),
      ~gsub("point_", "", .x)
    )
  )

  expect_equal(
    bind_dfr(as_harp_list(a = ens_grid_df, b = dplyr::mutate(ens_grid_df, grid_mbr003 = grid_mbr001))),
    rename_with(
      as_harp_df(tibble::tibble(
        fcst_model = c(rep("a", nrow(ens_grid_df)), rep("b", nrow(ens_grid_df))),
        fcst_dttm = rep(ens_grid_df$fcst_dttm, 2),
        lead_time = rep(ens_grid_df$lead_time, 2),
        valid_dttm = rep(ens_grid_df$valid_dttm, 2),
        grid_mbr000 = as_geolist(rep(ens_grid_df$grid_mbr000, 2)),
        grid_mbr001 = as_geolist(rep(ens_grid_df$grid_mbr001, 2)),
        grid_mbr003 = as_geolist(c(lapply(1:nrow(ens_grid_df), function(x) NULL), ens_grid_df$grid_mbr001))
      )),
      ~gsub("grid_", "", .x)
    )
  )
})
