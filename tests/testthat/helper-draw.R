# Opens a real pdf device to a temp file for the duration of the calling
# test. pdf(NULL) is avoided: grid's clip-path resolution segfaults on it
# with old grid versions (R 4.2 on CI oldrel)
local_pdf_device <- function(env = parent.frame()) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  withr::defer(
    {
      grDevices::dev.off()
      unlink(path)
    },
    envir = env
  )
  invisible(path)
}
