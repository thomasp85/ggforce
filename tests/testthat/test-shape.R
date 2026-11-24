test_that("shapeGrob draws unmodified polygons when the radius resolves to zero", {
  local_pdf_device()
  x <- c(0.1, 0.6, 0.4, 0.9, 0.3, 0.7)
  y <- c(0.1, 0.5, 0.3, 0.5, 0.8, 0.9)
  id <- c(2L, 1L, 2L, 1L, 3L, 1L)
  radius <- grid::stringWidth("")
  gp <- grid::gpar(fill = c("red", "blue", "grey"))
  grob <- shapeGrob(x, y, id = id, radius = radius, gp = gp)

  polygon <- grid::grid.force(grob, draw = TRUE)

  expect_s3_class(polygon, "polygon")
  expect_identical(as.numeric(polygon$x), x[order(id)])
  expect_identical(as.numeric(polygon$y), y[order(id)])
  expect_identical(polygon$id, sort(id))
  expect_identical(polygon$gp, gp)
})
