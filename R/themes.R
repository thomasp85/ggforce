#' @importFrom ggplot2 theme_bw theme element_blank %+replace%
#' @export
theme_no_axes <- function (base_size = 12, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank())
}
