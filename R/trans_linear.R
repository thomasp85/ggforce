#' Create a custom linear transformation
#'
#' This function lets you compose transformations based on a sequence of linear
#' transformations. If the transformations are parameterised the parameters will
#' become arguments in the transformation function. The transformations are
#' one of \code{rotate}, \code{shear}, \code{stretch}, \code{translate}, and
#' \code{reflect}.
#'
#' @param ... A number of transformation functions.
#'
#' @return \code{linear_trans} creates a trans object. The other functions
#' return a 3x3 transformation matrix.
#'
#' @export
#' @importFrom scales trans_new
#'
#' @examples
#' trans <- linear_trans(rotate(a), shear(1, 0), translate(x1, y1))
#' square <- data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0))
#' square2 <- trans$transform(square$x, square$y, a = pi/3, x1 = 4, y1 = 8)
#' square3 <- trans$transform(square$x, square$y, a = pi/1.5, x1 = 2, y1 = -6)
#' square <- rbind(square, square2, square3)
#' square$group <- rep(1:3, each = 4)
#' ggplot(square, aes(x, y, group = group)) +
#'   geom_polygon(aes(fill = factor(group)), colour = 'black')
#'
linear_trans <- function(...) {
    calls <- as.list(substitute(list(...)))[-1]
    transformations <- sapply(calls, deparse)
    args <- unlist(lapply(calls, function(call) {
        args <- as.list(call)[-1]
        as.character(args[sapply(args, 'class') == 'name'])
    }))
    args <- unique(args)
    if (any(c('x', 'y') %in% args)) {
        stop('x and y are preserved argument names', call. = FALSE)
    }
    args <- c('x', 'y', args)
    trans_fun <- function() {
        env <- environment()
        trans_mat <- Reduce(function(l, r) r %*% l, lapply(calls, eval, envir = env))
        trans <- trans_mat %*% rbind(x, y, z = 1)
        data.frame(x = trans[1,], y = trans[2,])
    }
    formals(trans_fun) <- structure(rep(list(quote(expr = )), length(args)), names = args)
    inv_fun <- function() {
        env <- environment()
        trans_mat <- Reduce(function(l, r) r %*% l, lapply(calls, eval, envir = env))
        trans_mat <- solve(trans_mat)
        trans <- trans_mat %*% rbind(x, y, z = 1)
        data.frame(x = trans[1,], y = trans[2,])
    }
    formals(inv_fun) <- structure(rep(list(quote(expr = )), length(args)), names = args)
    trans_new(
        name = paste0('linear: ', paste(transformations, collapse = ', ')),
        transform = trans_fun,
        inverse = inv_fun,
        breaks = extended_breaks(),
        format = format_format()
    )
}

#' @rdname linear_trans
#' @param angle An angle in radians
rotate <- function(angle) {
    matrix(c(cos(angle), -sin(angle), 0, sin(angle), cos(angle), 0, 0, 0, 1), ncol = 3)
}
#' @rdname linear_trans
#' @param x the transformation magnitude in the x-direction
#' @param y the transformation magnitude in the x-direction
stretch <- function(x, y) {
    matrix(c(x, 0, 0, 0, y, 0, 0, 0, 1), ncol = 3)
}
#' @rdname linear_trans
shear <- function(x, y) {
    matrix(c(1, y, 0, x, 1, 0, 0, 0, 1), ncol = 3)
}
#' @rdname linear_trans
translate <- function(x, y) {
    matrix(c(1, 0, 0, 0, 1, 0, x, y, 1), ncol = 3)
}
#' @rdname linear_trans
reflect <- function(x, y) {
    l <- x^2 + y^2
    matrix(c((x^2-y^2)/l, 2*x*y/l, 0, 2*x*y/l, (y^2-x^2)/l, 0, 0, 0, 1), ncol = 3)
}
