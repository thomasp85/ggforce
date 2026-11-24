fake_label_df <- function() {
  data.frame(
    fill = c("red", "blue"),
    colour = c("black", "white"),
    style = c(mark_style(), mark_style()),
    label = c("a", "b"),
    stringsAsFactors = FALSE
  )
}

test_that("apply_label_style resolves 'inherit' per group (#331)", {
  label <- fake_label_df()
  out <- apply_label_style(label, label.fill = "inherit", label.colour = "inherit")
  expect_equal(out$style[[1]]$body$background, "red")
  expect_equal(out$style[[2]]$body$background, "blue")
  expect_equal(out$style[[1]]$body$color, "black")
  expect_equal(out$style[[2]]$body$color, "white")
})

test_that("apply_label_style applies plain values and leaves NULL alone", {
  label <- fake_label_df()
  out <- apply_label_style(label, label.fill = "black", label.colour = NULL)
  expect_equal(out$style[[1]]$body$background, "black")
  expect_equal(out$style[[2]]$body$background, "black")
  untouched <- apply_label_style(label, NULL, NULL)
  expect_identical(untouched, label)
  expect_null(apply_label_style(NULL, "inherit", "inherit"))
})

draw_quiet <- function(p) {
  local_pdf_device()
  withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_no_error(suppressWarnings(grid::grid.draw(ggplotGrob(p))))
  )
}

test_that("label.fill = 'inherit' draws without error for all mark geoms (#331)", {
  base <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
    aes(fill = Species, label = Species) +
    geom_point()
  draw_quiet(base + geom_mark_circle(label.fill = "inherit"))
  draw_quiet(base + geom_mark_ellipse(label.fill = "inherit"))
  draw_quiet(base + geom_mark_hull(label.fill = "inherit"))
  draw_quiet(base + geom_mark_rect(label.fill = "inherit"))
  draw_quiet(base + geom_mark_hull(label.colour = "inherit",
                                   label.fill = "inherit_fill"))
})
