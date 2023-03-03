## Tests for utility functions

# seq_pwr2
test_that("seq_pwr2 throws error for bad inputs", {
  expect_error(seq_pwr2(0.25, 5), "`from` must be an integer when powers = TRUE")
  expect_error(seq_pwr2(5.25, 5), "`from` must be an integer when powers = TRUE")
  expect_error(seq_pwr2(0, 5.5), "`len` must be a positive integer")
  expect_error(seq_pwr2(0, -5), "`len` must be a positive integer")
  expect_error(seq_pwr2(0, 5, powers = 10), "`powers` must be logical")
  expect_error(seq_pwr2(0, 5, powers = "TRUE"), "`powers` must be logical")
  expect_error(seq_pwr2(0, 5, powers = FALSE), "`from` must be an integer power of 2")
  expect_error(seq_pwr2(0.12, 5, powers = FALSE), "`from` must be an integer power of 2")
  expect_error(seq_pwr2(-3, 5, powers = FALSE), "`from` must be an integer power of 2")
})

test_that("seq_pwr2 gives the correct sequences", {
  expect_equal(seq_pwr2(0, 5), c(1, 2, 4, 8, 16))
  expect_equal(seq_pwr2(1, 5, powers = FALSE), c(1, 2, 4, 8, 16))
  expect_equal(seq_pwr2(-3, 5), c(0.125, 0.25, 0.5, 1, 2))
  expect_equal(seq_pwr2(0.125, 5, powers = FALSE), c(0.125, 0.25, 0.5, 1, 2))
  expect_equal(seq_pwr2(4, 3), c(16, 32, 64))
  expect_equal(seq_pwr2(16, 3, powers = FALSE), c(16, 32, 64))
})

# psub
test_that("psub makes the correct replacements", {
  expect_equal(psub(c("a", "b", "c"), c("a", "c"), c("A", "C")), c("A", "b", "C"))
  expect_equal(
    psub(c("aa_mbr000", "mbr001", "aa_mbr002"), c("[a-z]+_mbr[0-9]{3}", "mbr[0-9]{3}"), c("aa", "bb")),
    c("aa", "bb", "aa")
  )
})

test_that("exact pattern matching in psub", {
  expect_warning(psub("ace", "a", "A"), "\"a\" not found")
  expect_equal(
    psub(c("one", "two", "three"), c("o", "t"), c("O", "T"), exact = FALSE),
    c("One", "TwO", "Three")
  )
})

# extract numeric
test_that("extract_numeric extracts the correct numbers for a single string", {
  expect_equal(extract_numeric("23mnmn56mmnm788m"), c(23, 56, 788))
  expect_equal(extract_numeric("mnmn56mmnm788m44"), c(56, 788, 44))
  expect_equal(extract_numeric("23.321897mnmn56mmnm788.44332m"), c(23.321897, 56, 788.44332))
  expect_equal(extract_numeric("-23mnmn56mmnm-788m"), c(-23, 56, -788))
  expect_equal(extract_numeric("-23.443mnmn56mmnm-788.292m"), c(-23.443, 56, -788.292))
})

test_that("extract_numeric extracts the correct numbers for a vector of strings", {
  expect_equal(extract_numeric(c("23mn", "mn56mm", "cs788")), c(23, 56, 788))
  expect_equal(extract_numeric(c("mnmn56", "mmnm788", "m44")), c(56, 788, 44))
  expect_equal(extract_numeric(c("23.321897mn", "mn56mm", "nm788.44332m")), c(23.321897, 56, 788.44332))
  expect_equal(extract_numeric(c("-23mn", "mn56mm", "nm-788m")), c(-23, 56, -788))
  expect_equal(extract_numeric(c("-23.443mn", "mn56mmn", "m-788.292m")), c(-23.443, 56, -788.292))
  expect_equal(extract_numeric(c("1h", "2h d4h, 3", "f4.5f4.6", "d-4-8.7d")), c(1, 2, 4, 3, 4.5, 4.6, -4, -8.7))
  expect_equal(
    extract_numeric(c("1h", "2h d4h, 3", "f4.5f4.6", "d-4-8.7d"), as_list = TRUE),
    list(1, c(2, 4, 3), c(4.5, 4.6), c(-4, -8.7))
  )
})

test_that("extract_numeric throws error for non character or numeric", {
  expect_error(extract_numeric(TRUE))
  expect_error(extract_numeric(complex(real = 4, imaginary = 5)))
  expect_error(extract_numeric(Sys.time()))
  expect_equal(extract_numeric(c(1, 2, -3.4)), c(1, 2, -3.4))
})
