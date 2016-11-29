globalVariables(
    c(
        '.'
    )
)

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}
