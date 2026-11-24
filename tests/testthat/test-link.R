test_that("geom_link2 handles single-observation groups (#328)", {
  lines <- data.frame(
    x = c(5, 12, 15, 9, 6),
    y = c(17, 20, 4, 15, 5),
    width = c(1, 10, 6, 2, 3),
    group = c("a", "a", "b", "b", "c")
  )
  p <- ggplot(lines) +
    geom_link2(aes(x = x, y = y, linewidth = width, group = group), n = 100)
  expect_no_error(ggplotGrob(p))

  dat <- ggplot_build(p)$data[[1]]
  # The singleton group is kept as a single uninterpolated row
  expect_equal(sum(dat$group == 3), 1)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(suppressWarnings(grid::grid.draw(ggplotGrob(p))))
})

test_that("geom_link2 handles unequal group sizes", {
  lines <- data.frame(
    x = c(1, 2, 3, 5, 6),
    y = c(1, 4, 2, 3, 1),
    group = c("a", "a", "a", "b", "b")
  )
  p <- ggplot(lines) + geom_link2(aes(x, y, group = group))
  expect_no_error(ggplotGrob(p))
})
