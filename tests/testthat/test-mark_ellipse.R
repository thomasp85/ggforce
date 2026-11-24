# How well are the input points contained in the returned ellipse? Values <= 1
# mean inside; the Khachiyan tolerance permits values slightly above 1.
ellipse_containment <- function(e, x, y) {
  dx <- x - e$x0
  dy <- y - e$y0
  ca <- cos(-e$angle)
  sa <- sin(-e$angle)
  u <- dx * ca - dy * sa
  v <- dx * sa + dy * ca
  (u / e$a)^2 + (v / e$b)^2
}

test_that("enclose_ellip_points contains all points for 3-point groups (#354)", {
  x <- c(-1.33, 92.6, 78.5)
  y <- c(46.5, -22.4, 0.9)
  e <- enclose_ellip_points(x, y, rep(1L, 3), 0.01)
  expect_lte(max(ellipse_containment(e, x, y)), 1 + 0.05)
})

test_that("enclose_ellip_points handles multi-group data (#214)", {
  x <- c(-235, -220,  187, -100, -162,   8, 336, 159, 443, -77, -412)
  y <- c( -32, -147, -123,    6,  102, -14,  40,   6,  15, 115,   30)
  g <- as.integer(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4))
  e <- enclose_ellip_points(x, y, g, 0.01)
  expect_equal(nrow(e), 4L)
  for (i in seq_len(nrow(e))) {
    expect_lte(max(ellipse_containment(e[i, ], x[g == i], y[g == i])), 1 + 0.05)
  }
})

test_that("enclose_ellip_points contains random point configurations", {
  set.seed(7341)
  for (i in 1:100) {
    n <- sample(3:12, 1)
    x <- rnorm(n, sd = runif(1, 1, 100))
    y <- rnorm(n, sd = runif(1, 1, 100))
    if (length(unique(x)) < 2 || length(unique(y)) < 2) next
    e <- enclose_ellip_points(x, y, rep(1L, n), 0.01)
    expect_lte(max(ellipse_containment(e, x, y)), 1 + 0.1)
  }
})

test_that("enclose_ellip_points handles degenerate inputs", {
  # Single point
  e <- enclose_ellip_points(1, 2, 1L, 0.01)
  expect_equal(e$x0, 1)
  expect_equal(e$y0, 2)
  # Collinear points
  e <- enclose_ellip_points(c(0, 1, 2), c(0, 1, 2), rep(1L, 3), 0.01)
  expect_lte(max(ellipse_containment(e, c(0, 1, 2), c(0, 1, 2))), 1 + 0.1)
  # Duplicate points
  e <- enclose_ellip_points(c(1, 1, 5), c(2, 2, 8), rep(1L, 3), 0.01)
  expect_lte(max(ellipse_containment(e, c(1, 1, 5), c(2, 2, 8))), 1 + 0.1)
})
