dttm_ymd     <- as.POSIXct("2022-03-08 00:00:00", tz = "UTC")
dttm_ymd_h   <- as.POSIXct("2022-03-08 10:00:00", tz = "UTC")
dttm_ymd_hm  <- as.POSIXct("2022-03-08 10:30:00", tz = "UTC")
dttm_ymd_hms <- as.POSIXct("2022-03-08 10:30:30", tz = "UTC")

str_ymd     <- "20220308"
str_ymd_h   <- "2022030810"
str_ymd_hm  <- "202203081030"
str_ymd_hms <- "20220308103030"

num_ymd     <- as.numeric(as.POSIXct("2022-03-08 00:00:00", tz = "UTC"))
num_ymd_h   <- as.numeric(as.POSIXct("2022-03-08 10:00:00", tz = "UTC"))
num_ymd_hm  <- as.numeric(as.POSIXct("2022-03-08 10:30:00", tz = "UTC"))
num_ymd_hms <- as.numeric(as.POSIXct("2022-03-08 10:30:30", tz = "UTC"))

seq_30s <- c("20220308000000", "20220308000030", "20220308000100", "20220308000130", "20220308000200")
seq_30m <- c("202203080000", "202203080030", "202203080100", "202203080130", "202203080200")
seq_1h  <- c("2022030800", "2022030801", "2022030802")
seq_1d  <- c("2022030812", "2022030912", "2022031012")

test_that("as_str_dttm truncates correctly", {
  expect_equal(as_str_dttm(dttm_ymd), str_ymd)
  expect_equal(as_str_dttm(dttm_ymd_h), str_ymd_h)
  expect_equal(as_str_dttm(dttm_ymd_hm), str_ymd_hm)
  expect_equal(as_str_dttm(dttm_ymd_hms), str_ymd_hms)
})

test_that("as_YMD*** functions truncate correctly", {
  expect_equal(as_YMD(dttm_ymd_hms), str_ymd)
  expect_equal(as_YMDh(dttm_ymd), paste0(str_ymd, "00"))
  expect_equal(as_YMDhm(dttm_ymd), paste0(str_ymd, "0000"))
  expect_equal(as_YMDhms(dttm_ymd), paste0(str_ymd, "000000"))
  expect_equal(as_YMDh(dttm_ymd_h), str_ymd_h)
  expect_equal(as_YMDhm(dttm_ymd_hm), str_ymd_hm)
  expect_equal(as_YMDhms(dttm_ymd_hms), str_ymd_hms)
})

test_that("as_str_dttm works with unix time", {
  expect_equal(as_str_dttm(num_ymd), str_ymd)
  expect_equal(as_str_dttm(num_ymd_h), str_ymd_h)
  expect_equal(as_str_dttm(num_ymd_hm), str_ymd_hm)
  expect_equal(as_str_dttm(num_ymd_hms), str_ymd_hms)
})

test_that("as_unixtime works with strings", {
  expect_equal(as_unixtime(str_ymd), num_ymd)
  expect_equal(as_unixtime(str_ymd_h), num_ymd_h)
  expect_equal(as_unixtime(str_ymd_hm), num_ymd_hm)
  expect_equal(as_unixtime(str_ymd_hms), num_ymd_hms)
})

test_that("as_dttm works with strings", {
  expect_equal(as_dttm(str_ymd), dttm_ymd)
  expect_equal(as_dttm(str_ymd_h), dttm_ymd_h)
  expect_equal(as_dttm(str_ymd_hm), dttm_ymd_hm)
  expect_equal(as_dttm(str_ymd_hms), dttm_ymd_hms)
})

test_that("as_unixtime throws error with wrong length", {
  expect_error(as_unixtime(2))
  expect_error(as_unixtime(20))
  expect_error(as_unixtime(202))
  expect_error(as_unixtime(2022))
  expect_error(as_unixtime(20220))
  expect_error(as_unixtime(202201))
  expect_error(as_unixtime(2022010))
  expect_error(as_unixtime(202201000))
  expect_error(as_unixtime(20220100000))
  expect_error(as_unixtime(2022010000000))
  expect_error(as_unixtime(202201000000000))
})

test_that("as_unixtime returns NA with badly formed date", {
  expect_true(is.na(as_unixtime(20221302)))
  expect_true(is.na(as_unixtime(20221034)))
  expect_true(is.na(as_unixtime(2022100234)))
  expect_true(is.na(as_unixtime(202210020060)))
  expect_true(is.na(as_unixtime(20221302000060)))
})

test_that("seq_dttm works for all intervals", {
  expect_equal(seq_dttm(202203080000, 202203080002, by = "30s"), seq_30s)
  expect_equal(seq_dttm(2022030800, 2022030802, by = "30m"), seq_30m)
  expect_equal(seq_dttm(2022030800, 2022030802, by = "1h"), seq_1h)
  expect_equal(seq_dttm(2022030812, 2022031012, by = "1d"), seq_1d)
})

test_that("seq_dttm throws error with unknown interval units", {
  expect_error(seq_dttm(202203080000, 202203080002, by = "30a"))
  expect_error(seq_dttm(2022030800, 2022030802, by = "30b"))
  expect_error(seq_dttm(2022030800, 2022030802, by = "1l"))
  expect_error(seq_dttm(2022030812, 2022031012, by = "1q"))
  expect_error(seq_dttm(2022030812, 2022031012, by = "d"))
})
