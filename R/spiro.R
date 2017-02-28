StatSpiro <- ggproto('StatSpiro', Stat,
    compute_layer = function(self, data, params, panels) {
        if (is.null(data)) return(data)
        #browser()
        n_spiro <- nrow(data)
        data$group <- seq_len(n_spiro)
        revo <- attr(MASS::fractions(data$r/data$R), 'fracs')
        revo <- as.numeric(sub('/.*$', '', revo))
        data <- data[rep(seq_len(n_spiro), params$n * revo), ]
        data$rho <- unlist(lapply(revo, function(r) {
            seq(0, 2*pi*r, length.out = params$n * r)
        }))
        data$index <- unlist(lapply(revo, function(r) {
            seq(0, 1, length.out = params$n * r)
        }))
        data$x <- data$x + ifelse(
            data$outer,
            (data$R + data$r) * cos(data$rho) - data$d * cos(data$rho * (data$R + data$r)/data$r),
            (data$R - data$r) * cos(data$rho) + data$d * cos(data$rho * (data$R - data$r)/data$r)
        )
        data$y <- data$y + ifelse(
            data$outer,
            (data$R + data$r) * sin(data$rho) - data$d * sin(data$rho * (data$R + data$r)/data$r),
            (data$R - data$r) * sin(data$rho) - data$d * sin(data$rho * (data$R - data$r)/data$r)
        )
        data
    },
    required_aes = c('R', 'r', 'd', 'outer', 'x', 'y'),
    default_aes = aes(outer = FALSE, x = 0, y = 0),
    extra_params = c('na.rm', 'n', 'outer')
)

stat_spiro <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, n = 100,
                       show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
        stat = StatSpiro, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n=n, ...)
    )
}

geom_spiro <- function(mapping = NULL, data = NULL, stat = "spiro",
                       position = "identity", arrow = NULL, n = 100,
                       lineend = "butt", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPath,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, n=n, ...))
}
