test_that("calc_spr returns one at zero fishing mortality", {
  sel <- c(0.05, 0.25, 0.7, 1.0, 1.0)
  m <- rep(0.2, length(sel))
  waa <- c(0.2, 0.5, 1.2, 2.0, 2.8)
  mat <- c(0.0, 0.1, 0.6, 0.9, 0.95)

  expect_equal(calc_spr(0, sel, m, waa, mat), 1)
})

test_that("calc_spr decreases with fishing mortality", {
  sel <- c(0.05, 0.25, 0.7, 1.0, 1.0)
  m <- rep(0.2, length(sel))
  waa <- c(0.2, 0.5, 1.2, 2.0, 2.8)
  mat <- c(0.0, 0.1, 0.6, 0.9, 0.95)

  expect_lt(calc_spr(0.4, sel, m, waa, mat), calc_spr(0.1, sel, m, waa, mat))
})

test_that("find_f_spr_target matches the requested ratio", {
  sel <- c(0.05, 0.25, 0.7, 1.0, 1.0)
  m <- rep(0.2, length(sel))
  waa <- c(0.2, 0.5, 1.2, 2.0, 2.8)
  mat <- c(0.0, 0.1, 0.6, 0.9, 0.95)

  f35 <- find_f_spr_target(0.35, sel, m, waa, mat)

  expect_equal(calc_spr(f35, sel, m, waa, mat), 0.35, tolerance = 1e-6)
})

