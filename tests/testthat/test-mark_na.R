# A coord that injects NAs into the transformed data, simulating coords whose
# transform can produce non-finite positions (e.g. points that fall outside a
# projection). handle_na has already run at this point, so these NAs would
# flow straight into the mark geometry and label placement (#347).
CoordNA <- ggproto("CoordNA", CoordCartesian,
  transform = function(self, data, panel_params) {
    data <- ggproto_parent(CoordCartesian, self)$transform(data, panel_params)
    data$x[1] <- NA
    data$y[2] <- NA
    data
  }
)
coord_na <- function() ggproto(NULL, CoordNA)

mark_df <- function() {
  set.seed(991)
  data.frame(
    x = c(rnorm(10, 0), rnorm(10, 5)),
    y = c(rnorm(10, 0), rnorm(10, 5)),
    g = rep(c("a", "b"), each = 10)
  )
}

expect_draws <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  # suppressWarnings: mark geoms trigger a pre-existing, unrelated
  # "Group definition failed" fallback warning from grid
  expect_no_error(suppressWarnings(grid::grid.draw(ggplotGrob(p))))
}

test_that("geom_mark_hull tolerates post-transform NAs (#347)", {
  df <- mark_df()
  p <- ggplot(df, aes(x, y)) +
    geom_mark_hull(aes(label = g, fill = g)) +
    coord_na()
  expect_draws(p)
})

test_that("geom_mark_hull tolerates post-transform NAs in ghost rows (#347)", {
  df <- mark_df()
  df$f <- rep(c(TRUE, FALSE), each = 10)
  p <- ggplot(df, aes(x, y)) +
    geom_mark_hull(aes(label = g, fill = g, filter = f)) +
    coord_na()
  expect_draws(p)
})

test_that("geom_mark_circle tolerates post-transform NAs (#347)", {
  df <- mark_df()
  p <- ggplot(df, aes(x, y)) +
    geom_mark_circle(aes(label = g, fill = g)) +
    coord_na()
  expect_draws(p)
})

test_that("geom_mark_ellipse tolerates post-transform NAs (#347)", {
  df <- mark_df()
  p <- ggplot(df, aes(x, y)) +
    geom_mark_ellipse(aes(label = g, fill = g)) +
    coord_na()
  expect_draws(p)
})

test_that("geom_mark_rect tolerates post-transform NAs (#347)", {
  df <- mark_df()
  p <- ggplot(df, aes(x, y)) +
    geom_mark_rect(aes(label = g, fill = g)) +
    coord_na()
  expect_draws(p)
})
