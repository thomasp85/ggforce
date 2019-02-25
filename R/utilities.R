new_data_frame <- function(x = list(), n = NULL) {
    if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
    lengths <- vapply(x, length, integer(1))
    if (is.null(n)) {
        n <- if (length(x) == 0) 0 else max(lengths)
    }
    for (i in seq_along(x)) {
        if (lengths[i] == n) next
        if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
        x[[i]] <- rep(x[[i]], n)
    }

    class(x) <- "data.frame"

    attr(x, "row.names") <- .set_row_names(n)
    x
}
df_rows <- function(x, i) {
    new_data_frame(lapply(x, `[`, i = i))
}
split_matrix <- function(x, col_names = colnames(x)) {
    force(col_names)
    x <- lapply(seq_len(ncol(x)), function(i) x[, i])
    if (!is.null(col_names)) names(x) <- col_names
    x
}
# More performant modifyList without recursion
modify_list <- function(old, new) {
    for (i in names(new)) old[[i]] <- new[[i]]
    old
}
empty <- function(df) {
    is.null(df) || nrow(df) == 0 || ncol(df) == 0
}
split_indices <- function(group) {
    split(seq_along(group), group)
}
# Adapted from plyr:::id_vars
# Create a unique id for elements in a single vector
id_var <- function(x, drop = FALSE) {
    if (length(x) == 0) {
        id <- integer()
        n = 0L
    } else if (!is.null(attr(x, "n")) && !drop) {
        return(x)
    } else if (is.factor(x) && !drop) {
        x <- addNA(x, ifany = TRUE)
        id <- as.integer(x)
        n <- length(levels(x))
    } else {
        levels <- sort(unique(x), na.last = TRUE)
        id <- match(x, levels)
        n <- max(id)
    }
    attr(id, "n") <- n
    id
}
#' Create an unique integer id for each unique row in a data.frame
#'
#' Properties:
#' - `order(id)` is equivalent to `do.call(order, df)`
#' - rows containing the same data have the same value
#' - if `drop = FALSE` then room for all possibilites
#'
#' @param .variables list of variables
#' @param drop Should unused factor levels be dropped?
#'
#' @return An integer vector with attribute `n` giving the total number of
#' possible unique rows
#'
#' @keywords internal
#' @noRd
#'
id <- function(.variables, drop = FALSE) {
    nrows <- NULL
    if (is.data.frame(.variables)) {
        nrows <- nrow(.variables)
        .variables <- unclass(.variables)
    }
    lengths <- vapply(.variables, length, integer(1))
    .variables <- .variables[lengths != 0]
    if (length(.variables) == 0) {
        n <- nrows %||% 0L
        id <- seq_len(n)
        attr(id, "n") <- n
        return(id)
    }
    if (length(.variables) == 1) {
        return(id_var(.variables[[1]], drop = drop))
    }
    ids <- rev(lapply(.variables, id_var, drop = drop))
    p <- length(ids)
    ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1), USE.NAMES = FALSE)
    n <- prod(ndistinct)
    if (n > 2^31) {
        char_id <- do.call("paste", c(ids, sep = "\r"))
        res <- match(char_id, unique(char_id))
    }
    else {
        combs <- c(1, cumprod(ndistinct[-p]))
        mat <- do.call("cbind", ids)
        res <- c((mat - 1L) %*% combs + 1L)
    }
    if (drop) {
        id_var(res, drop = TRUE)
    }
    else {
        res <- as.integer(res)
        attr(res, "n") <- n
        res
    }
}
#' Bind data frames together by common column names
#'
#' This function is akin to `plyr::rbind.fill`, `dplyr::bind_rows`, and
#' `data.table::rbindlist`. It takes data frames in a list and stacks them on
#' top of each other, filling out values with `NA` if the column is missing from
#' a data.frame
#'
#' @param dfs A list of data frames
#'
#' @return A data.frame with the union of all columns from the data frames given
#' in `dfs`
#'
#' @keywords internal
#' @noRd
#'
rbind_dfs <- function(dfs) {
    out <- list()
    columns <- unique(unlist(lapply(dfs, names)))
    nrows <- vapply(dfs, .row_names_info, integer(1), type = 2L)
    total <- sum(nrows)
    if (length(columns) == 0) return(new_data_frame(list(), total))
    allocated <- rep(FALSE, length(columns))
    names(allocated) <- columns
    col_levels <- list()
    for (df in dfs) {
        new_columns <- intersect(names(df), columns[!allocated])
        for (col in new_columns) {
            if (is.factor(df[[col]])) {
                all_factors <- all(vapply(dfs, function(df) {
                    val <- .subset2(df, col)
                    is.null(val) || is.factor(val)
                }, logical(1)))
                if (all_factors) {
                    col_levels[[col]] <- unique(unlist(lapply(dfs, function(df) levels(.subset2(df, col)))))
                }
                out[[col]] <- rep(NA_character_, total)
            } else {
                out[[col]] <- rep(.subset2(df, col)[1][NA], total)
            }
        }
        allocated[new_columns] <- TRUE
        if (all(allocated)) break
    }
    pos <- c(cumsum(nrows) - nrows + 1)
    for (i in seq_along(dfs)) {
        df <- dfs[[i]]
        rng <- seq(pos[i], length.out = nrows[i])
        for (col in names(df)) {
            if (inherits(df[[col]], 'factor')) {
                out[[col]][rng] <- as.character(df[[col]])
            } else {
                out[[col]][rng] <- df[[col]]
            }
        }
    }
    for (col in names(col_levels)) {
        out[[col]] <- factor(out[[col]], levels = col_levels[[col]])
    }
    attributes(out) <- list(class = "data.frame", names = names(out), row.names = .set_row_names(total))
    out
}
#' Apply function to unique subsets of a data.frame
#'
#' This function is akin to `plyr::ddply`. It takes a single data.frame,
#' splits it by the unique combinations of the columns given in `by`, apply a
#' function to each split, and then reassembles the results into a sigle
#' data.frame again.
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#' dropped.
#'
#' @return A data.frame if the result of `fun` does not include the columns
#' given in `by` these will be prepended to the result.
#'
#' @keywords internal
#' @importFrom stats setNames
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
    grouping_cols <- .subset(df, by)
    ids <- id(grouping_cols, drop = drop)
    group_rows <- split(seq_len(nrow(df)), ids)
    rbind_dfs(lapply(seq_along(group_rows), function(i) {
        cur_data <- df_rows(df, group_rows[[i]])
        res <- fun(cur_data, ...)
        if (is.null(res)) return(res)
        if (length(res) == 0) return(new_data_frame())
        vars <- lapply(setNames(by, by), function(col) .subset2(cur_data, col)[1])
        if (is.matrix(res)) res <- split_matrix(res)
        if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
        new_data_frame(modify_list(unclass(vars), unclass(res)))
    }))
}
