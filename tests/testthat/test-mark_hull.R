draw_plot <- function(p) {
  local_pdf_device()
  suppressWarnings(grid::grid.draw(ggplotGrob(p)))
}

hull_polygons <- function(p) {
  local_pdf_device()
  gt <- ggplotGrob(p)
  panel <- gt$grobs[[which(gt$layout$name == "panel")]]
  hull_grob <- panel$children[[grep("hull_enc", names(panel$children))[1]]]
  suppressWarnings(grid::grid.draw(gt))
  mc <- grid::makeContent(hull_grob)
  mark <- mc$children[[1]]
  list(x = as.numeric(mark$x), y = as.numeric(mark$y), id = mark$id)
}

test_that("geom_mark_hull draws groups with 1, 2, and 3 points (#320)", {
  df <- data.frame(
    x = c(1, 3, 4, 6, 6, 8),
    y = c(1, 2, 3, 5, 5, 6),
    g = c("one", "two", "two", "three", "three", "three")
  )
  expect_no_error(draw_plot(ggplot(df, aes(x, y)) +
    geom_mark_hull(aes(fill = g))))
  expect_no_error(draw_plot(ggplot(df, aes(x, y)) +
    geom_mark_hull(aes(fill = g, label = g))))
  expect_no_error(draw_plot(ggplot(df[1, ], aes(x, y)) +
    geom_mark_hull(aes(fill = g))))
  expect_no_error(draw_plot(ggplot(df[2:3, ], aes(x, y)) +
    geom_mark_hull(aes(fill = g))))
})

test_that("geom_mark_hull draws collinear groups with duplicates", {
  df <- data.frame(
    x = c(1, 1, 2, 2, 3, 5, 6),
    y = c(1, 1, 2, 2, 3, 5, 1),
    g = c("a", "a", "a", "a", "a", "b", "b")
  )
  expect_no_error(draw_plot(ggplot(df, aes(x, y)) +
    geom_mark_hull(aes(fill = g, label = g))))
})

test_that("near-collinear hulls with duplicates stay bounded (#345)", {
  mds <- data.frame(
    Dim1 = c(-0.0005302627, 0.0244285080, 0.0265060161, -0.0081200544,
             -0.0098483091, -0.0081200544, -0.0098483091, -0.0045749060,
             -0.0098926286),
    Dim2 = c(0.0245328673, -0.0026370658, -0.0034842113, -0.0028471845,
             -0.0041339975, -0.0028471845, -0.0041339975, -0.0003568991,
             -0.0040923272),
    Sp = c("B38", "B39", "B39", "B40", "B40", "B40", "B40", "B40", "B40")
  )
  p <- ggplot(mds, aes(Dim1, Dim2, fill = Sp)) + geom_mark_hull()
  polys <- hull_polygons(p)
  # Hull polygons (in mm) must remain within the vicinity of the panel;
  # the miter-spike bug sent group 3 out to ~400mm
  expect_true(all(abs(polys$x) < 300))
  expect_true(all(abs(polys$y) < 300))
})
