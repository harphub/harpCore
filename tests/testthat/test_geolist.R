# Tests for geolist methods

# Create test data
dom1 <- structure(
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

dom2 <- structure(
  list(
    projection = list(
      proj  = "lcc",
      lon_0 = 10.74,
      lat_1 = 59.92,
      lat_2 = 59.92,
      R     = 6371229
    ),
    nx = 5,
    ny = 5,
    dx = 50000,
    dy = 50000,
    clonlat = c(10.75, 59.90)
  ),
  class = "geodomain"
)

geol1 <- lapply(
  1:10,
  function(x) {
    set.seed(x)
    structure(
      array(runif(dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1,
      info   = list(name = "", time = list()),
      class  = "geofield"
    )
  }
)

geo1 <- geol1[[1]]

geol2 <- lapply(
  11:20,
  function(x) {
    set.seed(x)
    structure(
      array(runif(dom2$nx * dom2$ny), c(x = dom2$nx, y = dom2$ny)),
      domain = dom2,
      info   = list(name = "", time = list()),
      class  = "geofield"
    )
  }
)

geo2 <- geol2[[1]]

test_that("as_geolist can create geolists from lists and geofields", {
  expect_equal(
    as_geolist(geol1),
    structure(geol1, class = c("geolist", "list"), domain = dom1)
  )
  expect_equal(
    as_geolist(geol2),
    structure(geol2, class = c("geolist", "list"), domain = dom2)
  )
  expect_equal(
    as_geolist(geo1, geo1),
    structure(list(geo1, geo1), class = c("geolist", "list"), domain = dom1)
  )
  expect_equal(
    as_geolist(geo2, geo2),
    structure(list(geo2, geo2), class = c("geolist", "list"), domain = dom2)
  )
})

test_that("as_geolist output has correct length", {
  expect_equal(length(as_geolist(geol1)), 10)
  expect_equal(length(as_geolist(geol2)), 10)
  expect_equal(length(as_geolist(c(geol1, geol1))), 20)
  expect_equal(length(as_geolist(c(geol2, geol2))), 20)
  expect_equal(length(as_geolist(geo1, geo1)), 2)
  expect_equal(length(as_geolist(geo2, geo2)), 2)
})

test_that("as_geolist throws error when domains are different", {
  expect_error(
    as_geolist(geo1, geo2),
    "All geofields must be on the same domain"
  )
  expect_error(
    as_geolist(c(geol1, geol2)),
    "All geofields must be on the same domain"
  )
})

test_that("as_geolist throws error when inputs are not all geofields", {
  expect_error(as_geolist(list(1, 2, 3)), "All ... must be geofields")
  expect_error(as_geolist(1, 2, 3), "All ... must be geofields")
  expect_error(as_geolist(list(geo1, 1)), "All ... must be geofields")
  expect_error(as_geolist(geo1, 1), "All ... must be geofields")
  expect_error(as_geolist(geo1, unclass(geo1)), "All ... must be geofields")
})

# function to make data to test cumulative math for geolists
test_cum <- function(.geolist, .d, .f) {
  res <- .geolist[1]
  for (i in 2:length(.geolist)) {
    res[[i]] <- .f(.geolist[[i]], res[[(i - 1)]])
  }
  structure(res, class = c("geolist", "list"), domain = .d)
}

# test for cumulative math
test_that("cumulative math methods work for geolists", {
  expect_equal(cumsum(as_geolist(geol1)), test_cum(geol1, dom1, `+`))
  expect_equal(cumprod(as_geolist(geol1)), test_cum(geol1, dom1, `*`))
  expect_equal(cummin(as_geolist(geol1)), test_cum(geol1, dom1, pmin))
  expect_equal(cummax(as_geolist(geol1)), test_cum(geol1, dom1, pmax))
  expect_equal(cumsum(as_geolist(geol2)), test_cum(geol2, dom2, `+`))
  expect_equal(cumprod(as_geolist(geol2)), test_cum(geol2, dom2, `*`))
  expect_equal(cummin(as_geolist(geol2)), test_cum(geol2, dom2, pmin))
  expect_equal(cummax(as_geolist(geol2)), test_cum(geol2, dom2, pmax))
})

# Special Ops methods for geofields
geof_data <- list(
  array(seq_len(dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
  array(seq_len(dom1$nx * dom1$ny) + 5, c(x = dom1$nx, y = dom1$ny)),
  array(seq_len(dom2$nx * dom2$ny) + 3, c(x = dom2$nx, y = dom2$ny)),
  array(seq_len(dom2$nx * dom2$ny) + 7, c(x = dom2$nx, y = dom2$ny))
)

test_geof <- c(
  lapply(
    geof_data[1:2],
    function(x) structure(
      x, domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  ),
  lapply(
    geof_data[3:4],
    function(x) structure(
      x, domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
)

test_that("test basic ops for geofields", {
  expect_equal(
    test_geof[[1]] + test_geof[[2]],
    structure(
      geof_data[[1]] + geof_data[[2]],
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] - test_geof[[2]],
    structure(
      geof_data[[1]] - geof_data[[2]],
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] * test_geof[[2]],
    structure(
      geof_data[[1]] * geof_data[[2]],
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] / test_geof[[2]],
    structure(
      geof_data[[1]] / geof_data[[2]],
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] + test_geof[[4]],
    structure(
      geof_data[[3]] + geof_data[[4]],
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] - test_geof[[4]],
    structure(
      geof_data[[3]] - geof_data[[4]],
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] * test_geof[[4]],
    structure(
      geof_data[[3]] * geof_data[[4]],
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] / test_geof[[4]],
    structure(
      geof_data[[3]] / geof_data[[4]],
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
})

test_that("test basic ops for geofield with scalar as first or second arg", {
  expect_equal(
    test_geof[[1]] + 6,
    structure(
      geof_data[[1]] + 6,
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] - 6,
    structure(
      geof_data[[1]] - 6,
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] * 6,
    structure(
      geof_data[[1]] * 6,
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] / 6,
    structure(
      geof_data[[1]] / 6,
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] + 9,
    structure(
      geof_data[[3]] + 9,
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] - 9,
    structure(
      geof_data[[3]] - 9,
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] * 9,
    structure(
      geof_data[[3]] * 9,
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[3]] / 9,
    structure(
      geof_data[[3]] / 9,
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )

  expect_equal(
    6 + test_geof[[1]],
    structure(
      geof_data[[1]] + 6,
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    6 - test_geof[[1]],
    structure(
      6 - geof_data[[1]],
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    6 * test_geof[[1]],
    structure(
      geof_data[[1]] * 6,
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    6 / test_geof[[1]],
    structure(
      6 / geof_data[[1]],
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    9 + test_geof[[3]],
    structure(
      geof_data[[3]] + 9,
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    9 - test_geof[[3]],
    structure(
      9 - geof_data[[3]],
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    9 * test_geof[[3]],
    structure(
      geof_data[[3]] * 9,
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    9 / test_geof[[3]],
    structure(
      9 / geof_data[[3]],
      domain = dom2, info = list(name = "", time = list()), class  = "geofield"
    )
  )

})

test_that("check geofield ops for logicals", {
  expect_equal(
    test_geof[[1]] == test_geof[[1]],
    structure(
      array(rep(TRUE, dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] == test_geof[[2]],
    structure(
      array(rep(FALSE, dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] < test_geof[[2]],
    structure(
      array(rep(TRUE, dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] > test_geof[[2]],
    structure(
      array(rep(FALSE, dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    test_geof[[1]] != test_geof[[2]],
    structure(
      array(rep(TRUE, dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
  expect_equal(
    !(test_geof[[1]] > test_geof[[2]]),
    structure(
      array(rep(TRUE, dom1$nx * dom1$ny), c(x = dom1$nx, y = dom1$ny)),
      domain = dom1, info = list(name = "", time = list()), class  = "geofield"
    )
  )
})

test_that("geofield ops for different domains", {
  expect_error(
    test_geof[[1]] + test_geof[[3]],
    "geofields must be on the same domain"
  )
  expect_error(
    geo1 + geo2,
    "geofields must be on the same domain"
  )
})

test_that("combining geofields with arrays", {
  expect_error(
    geo1 + unclass(geo1),
    "geofields can only be combined with other geofields or scalars"
  )
  expect_error(
    unclass(geo1) + geo1,
    "geofields can only be combined with other geofields or scalars"
  )
})

# Ops methods for geolists
test_that("geolists on different domains throw an error", {
  expect_error(
    as_geolist(geol1) + as_geolist(geol2),
    "geolists must have the same domain"
  )
  expect_error(
    as_geolist(geol1) * as_geolist(geol2),
    "geolists must have the same domain"
  )
  expect_error(
    as_geolist(geol1) / as_geolist(geol2),
    "geolists must have the same domain"
  )
  expect_error(
    as_geolist(geol1) - as_geolist(geol2),
    "geolists must have the same domain"
  )
})

test_that("geolists of different length throw error", {
  expect_error(
    as_geolist(geol1) + c(as_geolist(geol1), as_geolist(geol1)),
    "geolists must have the same length"
  )
  expect_error(
    as_geolist(geol2) + c(as_geolist(geol2), as_geolist(geol2)),
    "geolists must have the same length"
  )
})

geol3 <- lapply(geol1, function(x) x * runif(1))
geol4 <- lapply(geol2, function(x) x * runif(1))

test_that("basic math with two geolists", {
  expect_equal(
    as_geolist(geol1) + as_geolist(geol1),
    structure(
      mapply(`+`, geol1, geol1, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom1
    )
  )
  expect_equal(
    as_geolist(geol2) + as_geolist(geol2),
    structure(
      mapply(`+`, geol2, geol2, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom2
    )
  )
  expect_equal(
    as_geolist(geol1) + as_geolist(geol1),
    as_geolist(geol1) * 2
  )
  expect_equal(
    as_geolist(geol2) + as_geolist(geol2),
    as_geolist(geol2) * 2
  )
  expect_equal(
    as_geolist(geol1) + as_geolist(geol1) + as_geolist(geol1),
    as_geolist(geol1) * 3
  )
  expect_equal(
    as_geolist(geol1) - as_geolist(geol3),
    structure(
      mapply(`-`, geol1, geol3, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom1
    )
  )
  expect_equal(
    as_geolist(geol2) - as_geolist(geol4),
    structure(
      mapply(`-`, geol2, geol4, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom2
    )
  )
  expect_equal(
    as_geolist(geol1) * as_geolist(geol3),
    structure(
      mapply(`*`, geol1, geol3, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom1
    )
  )
  expect_equal(
    as_geolist(geol2) * as_geolist(geol4),
    structure(
      mapply(`*`, geol2, geol4, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom2
    )
  )
  expect_equal(
    as_geolist(geol1) * as_geolist(geol1),
    as_geolist(geol1) ^ 2
  )
  expect_equal(
    as_geolist(geol2) * as_geolist(geol2),
    as_geolist(geol2) ^ 2
  )
  expect_equal(
    as_geolist(geol1) / as_geolist(geol3),
    structure(
      mapply(`/`, geol1, geol3, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom1
    )
  )
  expect_equal(
    as_geolist(geol2) / as_geolist(geol4),
    structure(
      mapply(`/`, geol2, geol4, SIMPLIFY = FALSE),
      class = c("geolist", "list"), domain = dom2
    )
  )
})

# Summary methods for geolists
sum_geol1 <- geol1[[1]]
for (i in 2:length(geol1)) {
  sum_geol1 <- sum_geol1 + geol1[[i]]
}
sum_geol2 <- geol2[[1]]
for (i in 2:length(geol2)) {
  sum_geol2 <- sum_geol2 + geol2[[i]]
}
prod_geol1 <- geol1[[1]]
for (i in 2:length(geol1)) {
  prod_geol1 <- prod_geol1 * geol1[[i]]
}
prod_geol2 <- geol2[[1]]
for (i in 2:length(geol2)) {
  prod_geol2 <- prod_geol2 * geol2[[i]]
}
min_geol1 <- geol1[[1]]
for (i in 2:length(geol1)) {
  min_geol1 <- pmin(min_geol1, geol1[[i]])
}
min_geol2 <- geol2[[1]]
for (i in 2:length(geol2)) {
  min_geol2 <- pmin(min_geol2, geol2[[i]])
}
max_geol1 <- geol1[[1]]
for (i in 2:length(geol1)) {
  max_geol1 <- pmax(max_geol1, geol1[[i]])
}
max_geol2 <- geol2[[1]]
for (i in 2:length(geol2)) {
  max_geol2 <- pmax(max_geol2, geol2[[i]])
}

ldom <- structure(
  list(
    projection = list(
      proj  = "lcc",
      lon_0 = 10.74,
      lat_1 = 59.91,
      lat_2 = 59.91,
      R     = 6371229
    ),
    nx = 2,
    ny = 2,
    dx = 50000,
    dy = 50000,
    clonlat = c(10.74, 59.91)
  ),
  class = "geodomain"
)
lgeol <- list(
  array(c(TRUE, FALSE, TRUE, TRUE), c(2, 2)),
  array(c(FALSE, FALSE, TRUE, TRUE), c(2, 2)),
  array(c(TRUE, FALSE, FALSE, TRUE), c(2, 2))
)
lgeol <- lapply(lgeol, function(x) meteogrid::as.geofield(x, domain = ldom))
lgeol_any <- meteogrid::as.geofield(
  array(c(TRUE, FALSE, TRUE, TRUE), c(2, 2)), domain = ldom
)
lgeol_all <- meteogrid::as.geofield(
  array(c(FALSE, FALSE, FALSE, TRUE), c(2, 2)), domain = ldom
)

test_that("Summary methods for geolists", {
  expect_equal(sum(as_geolist(geol1)), sum_geol1)
  expect_equal(sum(as_geolist(geol2)), sum_geol2)
  expect_equal(prod(as_geolist(geol1)), prod_geol1)
  expect_equal(prod(as_geolist(geol2)), prod_geol2)
  expect_equal(min(as_geolist(geol1)), min_geol1)
  expect_equal(min(as_geolist(geol2)), min_geol2)
  expect_equal(max(as_geolist(geol1)), max_geol1)
  expect_equal(max(as_geolist(geol2)), max_geol2)
  expect_equal(any(as_geolist(lgeol)), lgeol_any)
  expect_equal(all(as_geolist(lgeol)), lgeol_all)
})

geol1m <- geol1
geol1m[[4]][2, 3] <- NA
geol1m[[6]][1, 5] <- NA

geol2m <- geol2
geol2m[[3]][3, 2] <- NA
geol2m[[7]][5, 1] <- NA

sum_geol1_na <- geol1m[[1]]
for (i in 2:length(geol1m)) {
  sum_geol1_na <- sum_geol1_na + geol1m[[i]]
}
sum_geol2_na <- geol2m[[1]]
for (i in 2:length(geol2m)) {
  sum_geol2_na <- sum_geol2_na + geol2m[[i]]
}
prod_geol1_na <- geol1m[[1]]
for (i in 2:length(geol1m)) {
  prod_geol1_na <- prod_geol1_na * geol1m[[i]]
}
prod_geol2_na <- geol2m[[1]]
for (i in 2:length(geol2m)) {
  prod_geol2_na <- prod_geol2_na * geol2m[[i]]
}
min_geol1_na <- geol1m[[1]]
for (i in 2:length(geol1m)) {
  min_geol1_na <- pmin(min_geol1_na, geol1m[[i]])
}
min_geol2_na <- geol2m[[1]]
for (i in 2:length(geol2m)) {
  min_geol2_na <- pmin(min_geol2_na, geol2m[[i]])
}
max_geol1_na <- geol1m[[1]]
for (i in 2:length(geol1m)) {
  max_geol1_na <- pmax(max_geol1_na, geol1m[[i]])
}
max_geol2_na <- geol2m[[1]]
for (i in 2:length(geol2m)) {
  max_geol2_na <- pmax(max_geol2_na, geol2m[[i]])
}

sum_geol1_na.rm <- geol1m[[1]]
sum_geol1_na.rm[is.na(sum_geol1_na.rm)] <- 0
for (i in 2:length(geol1m)) {
  new_geo <- geol1m[[i]]
  new_geo[is.na(new_geo)] <- 0
  sum_geol1_na.rm <- sum_geol1_na.rm + new_geo
}
sum_geol2_na.rm <- geol2m[[1]]
sum_geol2_na.rm[is.na(sum_geol2_na.rm)] <- 0
for (i in 2:length(geol2m)) {
  new_geo <- geol2m[[i]]
  new_geo[is.na(new_geo)] <- 0
  sum_geol2_na.rm <- sum_geol2_na.rm + new_geo
}
prod_geol1_na.rm <- geol1m[[1]]
prod_geol1_na.rm[is.na(prod_geol1_na.rm)] <- 1
for (i in 2:length(geol1m)) {
  new_geo <- geol1m[[i]]
  new_geo[is.na(new_geo)] <- 1
  prod_geol1_na.rm <- prod_geol1_na.rm * new_geo
}
prod_geol2_na.rm <- geol2m[[1]]
prod_geol2_na.rm[is.na(prod_geol2_na.rm)] <- 1
for (i in 2:length(geol2m)) {
  new_geo <- geol2m[[i]]
  new_geo[is.na(new_geo)] <- 1
  prod_geol2_na.rm <- prod_geol2_na.rm * new_geo
}
min_geol1_na.rm <- geol1m[[1]]
for (i in 2:length(geol1m)) {
  min_geol1_na.rm <- pmin(min_geol1_na.rm, geol1m[[i]], na.rm = TRUE)
}
min_geol2_na.rm <- geol2m[[1]]
for (i in 2:length(geol2m)) {
  min_geol2_na.rm <- pmin(min_geol2_na.rm, geol2m[[i]], na.rm = TRUE)
}
max_geol1_na.rm <- geol1m[[1]]
for (i in 2:length(geol1m)) {
  max_geol1_na.rm <- pmax(max_geol1_na.rm, geol1m[[i]], na.rm = TRUE)
}
max_geol2_na.rm <- geol2m[[1]]
for (i in 2:length(geol2m)) {
  max_geol2_na.rm <- pmax(max_geol2_na.rm, geol2m[[i]], na.rm = TRUE)
}

lgeolm <- list(
  array(c(TRUE, NA, TRUE, FALSE), c(2, 2)),
  array(c(FALSE, NA, TRUE, FALSE), c(2, 2)),
  array(c(TRUE, NA, TRUE, NA), c(2, 2))
)
lgeolm <- lapply(lgeolm, function(x) meteogrid::as.geofield(x, domain = ldom))
lgeol_any_na <- meteogrid::as.geofield(
  array(c(TRUE, NA, TRUE, NA), c(2, 2)), domain = ldom
)
lgeol_all_na <- meteogrid::as.geofield(
  array(c(FALSE, NA, TRUE, FALSE), c(2, 2)), domain = ldom
)
lgeol_any_na.rm <- meteogrid::as.geofield(
  array(c(TRUE, NA, TRUE, FALSE), c(2, 2)), domain = ldom
)
lgeol_all_na.rm <- meteogrid::as.geofield(
  array(c(FALSE, NA, TRUE, FALSE), c(2, 2)), domain = ldom
)


test_that("Summary methods for geolists with missing values", {
  expect_equal(sum(as_geolist(geol1m)), sum_geol1_na)
  expect_equal(sum(as_geolist(geol2m)), sum_geol2_na)
  expect_equal(prod(as_geolist(geol1m)), prod_geol1_na)
  expect_equal(prod(as_geolist(geol2m)), prod_geol2_na)
  expect_equal(min(as_geolist(geol1m)), min_geol1_na)
  expect_equal(min(as_geolist(geol2m)), min_geol2_na)
  expect_equal(max(as_geolist(geol1m)), max_geol1_na)
  expect_equal(max(as_geolist(geol2m)), max_geol2_na)
  expect_equal(any(as_geolist(lgeolm)), lgeol_any_na)
  expect_equal(all(as_geolist(lgeolm)), lgeol_all_na)

  expect_equal(sum(as_geolist(geol1m), na.rm = TRUE), sum_geol1_na.rm)
  expect_equal(sum(as_geolist(geol2m), na.rm = TRUE), sum_geol2_na.rm)
  expect_equal(prod(as_geolist(geol1m), na.rm = TRUE), prod_geol1_na.rm)
  expect_equal(prod(as_geolist(geol2m), na.rm = TRUE), prod_geol2_na.rm)
  expect_equal(min(as_geolist(geol1m), na.rm = TRUE), min_geol1_na.rm)
  expect_equal(min(as_geolist(geol2m), na.rm = TRUE), min_geol2_na.rm)
  expect_equal(max(as_geolist(geol1m), na.rm = TRUE), max_geol1_na.rm)
  expect_equal(max(as_geolist(geol2m), na.rm = TRUE), max_geol2_na.rm)
  expect_equal(any(as_geolist(lgeolm), na.rm = TRUE), lgeol_any_na.rm)
  expect_equal(all(as_geolist(lgeolm), na.rm = TRUE), lgeol_all_na.rm)
})

test_that("check for NAs in geofields and geolists", {
  expect_equal(
    is.na(lgeol_all_na),
    meteogrid::as.geofield(array(c(FALSE, TRUE, FALSE, FALSE), c(2, 2)), domain = ldom)
  )
  expect_equal(
    is.na(as_geolist(lgeolm)),
    structure(
      list(
        meteogrid::as.geofield(array(c(FALSE, TRUE, FALSE, FALSE), c(2, 2)), domain = ldom),
        meteogrid::as.geofield(array(c(FALSE, TRUE, FALSE, FALSE), c(2, 2)), domain = ldom),
        meteogrid::as.geofield(array(c(FALSE, TRUE, FALSE, TRUE), c(2, 2)), domain = ldom)
      ),
      class = c("geolist", "list"), domain = ldom
    )
  )
})

# Stats methods for geolists
geols <- list(
  meteogrid::as.geofield(array(c(1, 2, 3, 4), c(2, 2)), dom = ldom),
  meteogrid::as.geofield(array(c(3, 4, 5, 6), c(2, 2)), dom = ldom),
  meteogrid::as.geofield(array(c(5, 6, 7, 8), c(2, 2)), dom = ldom),
  meteogrid::as.geofield(array(c(7, 8, 9, 10), c(2, 2)), dom = ldom)
)

geols_mean <- meteogrid::as.geofield(
  array(
    c(mean(c(1, 3, 5, 7)), mean(c(2, 4, 6, 8)), mean(c(3, 5, 7, 9)), mean(c(4, 6, 8, 10))),
    c(2, 2)
  ),
  domain = ldom
)

geols_sd <- meteogrid::as.geofield(
  array(
    c(sd(c(1, 3, 5, 7)), sd(c(2, 4, 6, 8)), sd(c(3, 5, 7, 9)), sd(c(4, 6, 8, 10))),
    c(2, 2)
  ),
  domain = ldom
)

geols_var <- meteogrid::as.geofield(
  array(
    c(var(c(1, 3, 5, 7)), var(c(2, 4, 6, 8)), var(c(3, 5, 7, 9)), var(c(4, 6, 8, 10))),
    c(2, 2)
  ),
  domain = ldom
)

geols_na <- list(
  meteogrid::as.geofield(array(c(NA, 2, 3, 4), c(2, 2)), dom = ldom),
  meteogrid::as.geofield(array(c(NA, 4, 5, 6), c(2, 2)), dom = ldom),
  meteogrid::as.geofield(array(c(5, 6, NA, 8), c(2, 2)), dom = ldom),
  meteogrid::as.geofield(array(c(7, 8, 9, 10), c(2, 2)), dom = ldom)
)

geols_mean_na <- meteogrid::as.geofield(
  array(
    c(mean(c(NA, NA, 5, 7)), mean(c(2, 4, 6, 8)), mean(c(3, 5, NA, 9)), mean(c(4, 6, 8, 10))),
    c(2, 2)
  ),
  domain = ldom
)

geols_sd_na <- meteogrid::as.geofield(
  array(
    c(sd(c(NA, NA, 5, 7)), sd(c(2, 4, 6, 8)), sd(c(3, 5, NA, 9)), sd(c(4, 6, 8, 10))),
    c(2, 2)
  ),
  domain = ldom
)

geols_var_na <- meteogrid::as.geofield(
  array(
    c(var(c(NA, NA, 5, 7)), var(c(2, 4, 6, 8)), var(c(3, 5, NA, 9)), var(c(4, 6, 8, 10))),
    c(2, 2)
  ),
  domain = ldom
)

geols_mean_na.rm <- meteogrid::as.geofield(
  array(
    c(
      mean(c(NA, NA, 5, 7), na.rm = TRUE),
      mean(c(2, 4, 6, 8), na.rm = TRUE),
      mean(c(3, 5, NA, 9), na.rm = TRUE),
      mean(c(4, 6, 8, 10), na.rm = TRUE)
    ),
    c(2, 2)
  ),
  domain = ldom
)

geols_sd_na.rm <- meteogrid::as.geofield(
  array(
    c(
      sd(c(NA, NA, 5, 7), na.rm = TRUE),
      sd(c(2, 4, 6, 8), na.rm = TRUE),
      sd(c(3, 5, NA, 9), na.rm = TRUE),
      sd(c(4, 6, 8, 10), na.rm = TRUE)
    ),
    c(2, 2)
  ),
  domain = ldom
)

geols_var_na.rm <- meteogrid::as.geofield(
  array(
    c(
      var(c(NA, NA, 5, 7), na.rm = TRUE),
      var(c(2, 4, 6, 8), na.rm = TRUE),
      var(c(3, 5, NA, 9), na.rm = TRUE),
      var(c(4, 6, 8, 10), na.rm = TRUE)
    ),
    c(2, 2)
  ),
  domain = ldom
)


test_that("basic stats summaries of geolists", {
  expect_equal(mean(as_geolist(geols)), geols_mean)
  expect_equal(std_dev(as_geolist(geols)), geols_sd)
  expect_equal(variance(as_geolist(geols)), geols_var)
  expect_equal(mean(as_geolist(geols_na)), geols_mean_na)
  expect_equal(std_dev(as_geolist(geols_na)), geols_sd_na)
  expect_equal(variance(as_geolist(geols_na)), geols_var_na)
  expect_equal(mean(as_geolist(geols_na), na.rm = TRUE), geols_mean_na.rm)
  expect_equal(std_dev(as_geolist(geols_na), na.rm = TRUE), geols_sd_na.rm)
  expect_equal(variance(as_geolist(geols_na), na.rm = TRUE), geols_var_na.rm)
})

# diff method for geolists
diff1 <- as_geolist(list(
  geols[[2]] - geols[[1]],
  geols[[3]] - geols[[2]],
  geols[[4]] - geols[[3]]
))

diff2 <- as_geolist(list(
  geols[[3]] - geols[[1]],
  geols[[4]] - geols[[2]]
))

diff3 <- as_geolist(list(
  geols[[4]] - geols[[1]]
))

diff_neg1 <- as_geolist(list(
  geols[[1]] - geols[[2]],
  geols[[2]] - geols[[3]],
  geols[[3]] - geols[[4]]
))

diff_neg2 <- as_geolist(list(
  geols[[1]] - geols[[3]],
  geols[[2]] - geols[[4]]
))

diff_neg3 <- as_geolist(list(
  geols[[1]] - geols[[4]]
))

diff_nas <- as_geolist(
  list(meteogrid::as.geofield(array(rep(NA_real_, 4), c(2, 2)), domain = ldom))
)

test_that("diff gives correct value for different lags", {
  expect_equal(diff(as_geolist(geols), 0), as_geolist(geols))
  expect_equal(diff(as_geolist(geols), 1), diff1)
  expect_equal(diff(as_geolist(geols), 2), diff2)
  expect_equal(diff(as_geolist(geols), 3), diff3)
  expect_equal(diff(as_geolist(geols), -1), diff_neg1)
  expect_equal(diff(as_geolist(geols), -2), diff_neg2)
  expect_equal(diff(as_geolist(geols), -3), diff_neg3)
  expect_equal(diff(as_geolist(geols), 1, trim = FALSE), c(diff_nas, diff1))
  expect_equal(diff(as_geolist(geols), 2, trim = FALSE), c(diff_nas, diff_nas, diff2))
  expect_equal(diff(as_geolist(geols), 3, trim = FALSE), c(diff_nas, diff_nas, diff_nas, diff3))
  expect_equal(diff(as_geolist(geols), -1, trim = FALSE), c(diff_neg1, diff_nas))
  expect_equal(diff(as_geolist(geols), -2, trim = FALSE), c(diff_neg2, diff_nas, diff_nas))
  expect_equal(diff(as_geolist(geols), -3, trim = FALSE), c(diff_neg3, diff_nas, diff_nas, diff_nas))
  expect_error(diff(as_geolist(geols), 4), "lag is too long")
})
