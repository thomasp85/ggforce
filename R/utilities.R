# Wrapping vctrs data_frame constructor with no name repair
data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

# Wrapping unique0() to accept NULL
unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

df_rows <- function(x, i) {
  cols <- lapply(x, `[`, i = i)
  data_frame0(!!!cols, .size = length(i))
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
    levels <- sort(unique0(x), na.last = TRUE)
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
  lengths <- lengths(.variables)
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
    char_id <- inject(paste(!!!ids, sep = "\r"))
    res <- match(char_id, unique0(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- inject(cbind(!!!ids))
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
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(data_frame0(!!!unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  cli::cli_abort("Please use {.fn to_lower_ascii}, which works fine in all locales.")
}

toupper <- function(x) {
  cli::cli_abort("Please use {.fn to_upper_ascii}, which works fine in all locales.")
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

single_value <- function(x, ...) {
  UseMethod("single_value")
}
#' @export
single_value.default <- function(x, ...) {
  # This is set by id() used in creating the grouping var
  identical(attr(x, "n"), 1L)
}
#' @export
single_value.factor <- function(x, ...) {
  # Panels are encoded as factor numbers and can never be missing (NA)
  identical(levels(x), "1")
}

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

vec_rbind0 <- function(..., .error_call = current_env(), .call = caller_env()) {
  with_ordered_restart(
    vec_rbind(..., .error_call = .error_call),
    .call
  )
}
with_ordered_restart <- function(expr, .call) {
  withCallingHandlers(
    expr,
    vctrs_error_incompatible_type = function(cnd) {
      x <- cnd[["x"]]
      y <- cnd[["y"]]

      class_x <- class(x)[1]
      class_y <- class(y)[1]

      restart <- FALSE

      if (is.ordered(x) || is.ordered(y)) {
        restart <- TRUE
        if (is.ordered(x)) {
          x <- factor(as.character(x), levels = levels(x))
        }
        if (is.ordered(y)) {
          y <- factor(as.character(y), levels = levels(y))
        }
      } else if (is.character(x) || is.character(y)) {
        restart <- TRUE
        if (is.character(x)) {
          y <- as.character(y)
        } else {
          x <- as.character(x)
        }
      } else if (is.factor(x) || is.factor(y)) {
        restart <- TRUE
        lev <- c()
        if (is.factor(x)) {
          lev <- c(lev, levels(x))
        }
        if (is.factor(y)) {
          lev <- c(lev, levels(y))
        }
        x <- factor(as.character(x), levels = unique(lev))
        y <- factor(as.character(y), levels = unique(lev))
      }

      # Don't recurse and let ptype2 error keep its course
      if (!restart) {
        return(zap())
      }

      msg <- paste0("Combining variables of class <", class_x, "> and <", class_y, ">")
      desc <- paste0(
        "Please ensure your variables are compatible before plotting (location: ",
        format_error_call(.call),
        ")"
      )

      deprecate_soft0(
        "3.4.0",
        I(msg),
        details = desc
      )

      x_arg <- cnd[["x_arg"]]
      y_arg <- cnd[["y_arg"]]
      call <- cnd[["call"]]

      # Recurse with factor methods and restart with the result
      if (inherits(cnd, "vctrs_error_ptype2")) {
        out <- vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
        restart <- "vctrs_restart_ptype2"
      } else if (inherits(cnd, "vctrs_error_cast")) {
        out <- vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
        restart <- "vctrs_restart_cast"
      } else {
        return(zap())
      }

      # Old-R compat for `tryInvokeRestart()`
      try_restart <- function(restart, ...) {
        if (!is_null(findRestart(restart))) {
          invokeRestart(restart, ...)
        }
      }
      try_restart(restart, out)
    }
  )
}
deprecate_soft0 <- function(..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}
