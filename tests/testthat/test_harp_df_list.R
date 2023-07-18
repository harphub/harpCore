# test functions for manipulating harp_df data frames and harp_list objects

## select_members
test_that("select_members selects the correct members", {
  expect_equal(
    select_members(ens_point_df, 0),
    ens_point_df[c("fcst_dttm", "lead_time", "valid_dttm", "SID", "point_mbr000")]
  )
  expect_equal(
    select_members(ens_grid_df, 1),
    ens_grid_df[c("fcst_dttm", "lead_time", "valid_dttm", "grid_mbr001")]
  )
  expect_equal(
    select_members(ens_point_list, 0),
    structure(
      list(
        a = ens_point_list[[1]][c("fcst_model", "fcst_dttm", "lead_time", "valid_dttm", "SID", "a_mbr000")],
        b = ens_point_list[[2]][c("fcst_model", "fcst_dttm", "lead_time", "valid_dttm", "SID", "b_mbr000")]
      ),
      class = c("harp_list", "list")
    )
  )
  expect_equal(
    suppressWarnings(select_members(ens_point_list, list(0, 1))),
    structure(
      list(
        a = ens_point_list[[1]][c("fcst_model", "fcst_dttm", "lead_time", "valid_dttm", "SID", "a_mbr000")],
        b = ens_point_list[[2]][c("fcst_model", "fcst_dttm", "lead_time", "valid_dttm", "SID", "b_mbr001")]
      ),
      class = c("harp_list", "list")
    )
  )
  expect_equal(
    select_members(ens_grid_list, list(a = 1, b = 0)),
    structure(
      list(
        a = ens_grid_list[[1]][c("fcst_model", "fcst_dttm", "lead_time", "valid_dttm", "a_mbr001")],
        b = ens_grid_list[[2]][c("fcst_model", "fcst_dttm", "lead_time", "valid_dttm", "b_mbr000")]
      ),
      class = c("harp_list", "list")
    )
  )
})

test_that("select_members throws error for wrong names in list", {
  expect_error(
    select_members(ens_grid_list, list(a_fcst = 0, b_fcst = 0)),
    "a_fcst, b_fcst not found in .data"
  )
  expect_error(
    select_members(ens_grid_list, list(a_fcst = 0, b = 0)),
    "a_fcst not found in .data"
  )
})

## pivot_members
test_that("pivot_members returns the correct data frame", {
  expect_equal(
    pivot_members(ens_point_df),
    structure(
      tibble::tibble(
        sub_model = "point",
        fcst_dttm = rep(ens_point_df$fcst_dttm, 2),
        lead_time = rep(sort(ens_point_df$lead_time), 2),
        valid_dttm = rep(sort(ens_point_df$valid_dttm), 2),
        SID = sort(rep(ens_point_df$SID, 2)),
        member = rep(c("mbr000", "mbr001"), nrow(ens_point_df)),
        fcst = unlist(lapply(
          1:nrow(ens_point_df),
          function(i) c(ens_point_df$point_mbr000[i], ens_point_df$point_mbr001[i])
        ))
      ),
      class = c("harp_ens_point_df_long", "harp_point_df", "harp_df", "tbl_df", "tbl", "data.frame")
    )
  )
  expect_equal(
    pivot_members(ens_grid_df),
    structure(
      tibble::tibble(
        sub_model = "grid",
        fcst_dttm = rep(ens_grid_df$fcst_dttm, 2),
        lead_time = sort(rep(ens_grid_df$lead_time, 2)),
        valid_dttm = sort(rep(ens_grid_df$valid_dttm, 2)),
        member = rep(c("mbr000", "mbr001"), nrow(ens_grid_df)),
        fcst = geolist(Reduce(c, lapply(
          1:nrow(ens_grid_df),
          function(i) c(ens_grid_df$grid_mbr000[i], ens_grid_df$grid_mbr001[i])
        ))
      )),
      class = c("harp_ens_grid_df_long", "harp_grid_df", "harp_df", "tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("pivot_members works in both directions", {
  expect_equal(pivot_members(pivot_members(ens_point_df)), ens_point_df)
  expect_equal(pivot_members(pivot_members(ens_grid_df)), ens_grid_df)
})

## expand_date
test_that("expand_date expands the date correctly", {
  expect_equal(
    expand_date(det_point_df, valid_dttm),
    structure(
        tibble::tibble(
        fcst_model = "point",
        fcst_dttm = det_point_df$fcst_dttm,
        lead_time = det_point_df$lead_time,
        valid_dttm = det_point_df$valid_dttm,
        SID = det_point_df$SID,
        fcst = det_point_df$fcst,
        valid_year = 2021L,
        valid_month = 1L,
        valid_day = 1L,
        valid_hour = rep(seq.int(0, 23), 2),
        valid_minute = 0L
      ),
      class = class(det_point_df)
    )
  )
  expect_equal(
    expand_date(ens_point_df, fcst_dttm),
    structure(
      tibble::tibble(
        fcst_dttm = ens_point_df$fcst_dttm,
        lead_time = ens_point_df$lead_time,
        valid_dttm = ens_point_df$valid_dttm,
        SID = ens_point_df$SID,
        point_mbr000 = ens_point_df$point_mbr000,
        point_mbr001 = ens_point_df$point_mbr001,
        fcst_year = 2021L,
        fcst_month = 1L,
        fcst_day = 1L,
        fcst_hour = 0L,
        fcst_minute = 0L
      ),
      class = class(ens_point_df)
    )
  )
  old_lctime <- Sys.getlocale("LC_TIME")
  invisible(Sys.setlocale("LC_TIME", "en_GB.UTF-8"))
  expect_equal(
    expand_date(det_grid_df, fcst_dttm, text_months = TRUE),
    structure(
      tibble::tibble(
        fcst_model = "grid",
        fcst_dttm = det_grid_df$fcst_dttm,
        lead_time = det_grid_df$lead_time,
        valid_dttm = det_grid_df$valid_dttm,
        fcst = det_grid_df$fcst,
        fcst_year = 2021L,
        fcst_month = "Jan",
        fcst_day = 1L,
        fcst_hour = 0L,
        fcst_minute = 0L
      ),
      class = class(det_grid_df)
    )
  )
  invisible(Sys.setlocale("LC_TIME", old_lctime))
  expect_equal(
    expand_date(det_point_list, valid_dttm),
    structure(
      list(
        a = structure(
          tibble::tibble(
            fcst_model = "a",
            fcst_dttm = det_point_list[[1]]$fcst_dttm,
            lead_time = det_point_list[[1]]$lead_time,
            valid_dttm = det_point_list[[1]]$valid_dttm,
            SID = det_point_list[[1]]$SID,
            fcst = det_point_list[[1]]$fcst,
            valid_year = 2021L,
            valid_month = 1L,
            valid_day = 1L,
            valid_hour = rep(seq.int(0, 23), 2),
            valid_minute = 0L
          ),
          class = class(det_point_list[[1]])
        ),
        b = structure(
          tibble::tibble(
            fcst_model = "b",
            fcst_dttm = det_point_list[[2]]$fcst_dttm,
            lead_time = det_point_list[[2]]$lead_time,
            valid_dttm = det_point_list[[2]]$valid_dttm,
            SID = det_point_list[[2]]$SID,
            fcst = det_point_list[[2]]$fcst,
            valid_year = 2021L,
            valid_month = 1L,
            valid_day = 1L,
            valid_hour = rep(seq.int(0, 23), 2),
            valid_minute = 0L
          ),
          class = class(det_point_list[[2]])
        )
      ),
      class = class(det_point_list)
    )
  )
})

test_that("expand_date throws warning for bad column selection", {
  expect_warning(
    expand_date(det_point_df, SID), "SID is not a date-time column"
  )
  expect_warning(
    expand_date(det_point_df, col), "col not found"
  )
})


