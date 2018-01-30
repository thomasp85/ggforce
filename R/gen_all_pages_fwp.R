#' Generate all pages with facet_wrap_paginate
#' 
#' This is a wrapper for [facet_wrap_paginate()] to generate all of the pages,
#' rather than just one at a time. 
#' 
#' @inheritParams facet_wrap_paginate
#' @inheritDotParams facet_wrap_paginate
#' @param plot A ggplot object.
#' @return A list, where each element is one page of plots.
#' @family ggforce facets
#' @examples 
#' g <- ggplot(diamonds, aes(x = cut, y = price)) + geom_boxplot()
#' gl <- gen_all_pages_fwp(g, "color", nrow = 2, ncol = 3)
#' gl[[1]]
#' @export
    
gen_all_pages_fwp <- function(plot, facets, ...){
    dots <- list(...)
    if (is.null(dots$nrow) || is.null(dots$ncol)){
        msg <- paste("nrow and/or ncol is NULL. Please supply values for both",
                     "arguments; otherwise, call facet_wrap directly.")
        stop(msg)
    }
    n <- n_pages(plot + facet_wrap_paginate(facets, ..., page = 1))
    plot_list <- lapply(1:n, function(i) {
        plot + facet_wrap_paginate(facets, ..., page = i)
    })
    return(plot_list)
}
