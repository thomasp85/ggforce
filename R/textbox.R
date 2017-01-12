#' @export
element_textbox <- function(family = NULL, face = NULL, colour = NULL,
                            size = NULL, hjust = NULL, vjust = NULL,
                            width = NULL, height = NULL, fill = NA,
                            linecolour = NA, linetype = NULL, linewidth = 0,
                            hjustbox = NULL, vjustbox = NULL, radius = NULL,
                            angle = NULL, lineheight = NULL, color = NULL,
                            linecolor = NULL, margin = NULL,
                            inherit.blank = FALSE) {

    if (!is.null(color))  colour <- color
    if (!is.null(linecolor))  linecolour <- linecolor
    structure(
        list(family = family, face = face, colour = colour, size = size,
             hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight,
             margin = margin, debug = FALSE, inherit.blank = inherit.blank),
        class = c("element_textbox", "element_text", "element"),
        box_par = list(
            width = width, height = height,
            fill = fill, linecolour = linecolour, linetype = linetype,
            linewidth = linewidth, hjustbox = hjustbox, vjustbox = vjustbox,
            lineheight = lineheight, radius = radius
        )
    )
}
#' @export
#' @importFrom grid valid.just gpar
#' @importFrom utils modifyList
#' @importFrom ggplot2 element_grob
element_grob.element_textbox <- function(element, label = "", x = NULL, y = NULL,
                                         family = NULL, face = NULL, colour = NULL,
                                         size = NULL, hjust = NULL, vjust = NULL,
                                         width = NULL, height = NULL, fill = NULL,
                                         linecolour = NULL, linetype = NULL,
                                         linewidth = NULL, hjustbox = NULL,
                                         vjustbox = NULL, radius = NULL,
                                         angle = NULL,  lineheight = NULL,
                                         margin = NULL, ...) {

    if (is.null(label)) return(zeroGrob())
    box_par <- attr(element, 'box_par')

    just <- c(
        hjust %||% element$hjust,
        vjust %||% element$vjust
    )
    just_box <- c(
        hjustbox %||% box_par$hjustbox,
        vjustbox %||% box_par$vjustbox
    )
    x <- x %||% element$x %||% valid.just(just_box)[1]
    y <- y %||% element$y %||% 1 - valid.just(just_box)[2]
    width <- width %||% box_par$width
    minwidth <- NULL
    maxwidth <- NULL
    if (length(width) == 3) {
        maxwidth <- width[[3]]
    }
    if (length(width) >= 2) {
        minwidth <- width[[1]]
        width <- width[[2]]
    }
    height <- height %||% box_par$height
    minheight <- NULL
    maxheight <- NULL
    if (length(height) == 3) {
        maxheight <- height[[3]]
    }
    if (length(height) >= 2) {
        minheight <- height[[1]]
        height <- height[[2]]
    }
    margin <- margin %||% element$margin

    angle <- angle %||% element$angle
    if (is.null(angle)) {
        stop("Text element requires non-NULL value for 'angle'.")
    }
    radius <- radius %||% box_par$radius

    # The gp settings can override element_gp
    gp_text <- gpar(fontsize = size, col = colour, fontfamily = family,
                    fontface = face, lineheight = lineheight)
    element_gp_text <- gpar(fontsize = element$size, col = element$colour,
                            fontfamily = element$family, fontface = element$face,
                            lineheight = element$lineheight)
    gp_box <- gpar(col = linecolour, fill = fill, lty = linetype,
                   lwd = linewidth)
    element_gp_box <- gpar(col = box_par$linecolour, fill = box_par$fill,
                           lty = box_par$linetype, lwd = box_par$linewidth)

    textboxGrob(label, x, y, just = just, rot = angle, width = width,
                height = height, minwidth = minwidth, maxwidth = maxwidth,
                minheight = minheight, maxheight = maxheight, boxjust = just_box,
                padding = margin, radius = radius,
                text.gp = modifyList(element_gp_text, gp_text),
                box.gp = modifyList(element_gp_box, gp_box))
}
#' @importFrom plyr defaults
#' @importFrom ggplot2 theme_get
#' @export
`&.gg` <- function(e1, e2) {
    if (!inherits(e1, 'ggplot')) {
        stop('Must be added to a ggplot', call. = FALSE)
    }
    if (!inherits(e2, 'theme')) {
        stop('Only objects of class "theme" can be added')
    }
    oldtheme <- defaults(e1$theme, theme_get())
    oldtheme <- oldtheme[names(e2)]
    oldtheme <- do.call(theme, c(oldtheme, list(complete = FALSE)))
    e2 <- add_theme(oldtheme, e2)
    e1$theme <- e2
    e1
}
add_theme <- function(t1, t2) {
    # Iterate over the elements that are to be updated
    for (item in names(t1)) {
        x <- t1[[item]]
        y <- t2[[item]]

        if (is.null(x) || inherits(x, "element_blank")) {
            # If x is NULL or element_blank, then just assign it y
            x <- y
        } else if (is.null(y) || is.character(y) || is.numeric(y) ||
                   is.logical(y) || inherits(y, "element_blank")) {
            # If y is NULL, or a string or numeric vector, or is element_blank, just replace x
            x <- y
        } else {
            # If x is not NULL, then copy over the non-NULL properties from y
            # Get logical vector of NULL properties in y
            idx <- vapply(y, is.null, logical(1))
            # Get the names of TRUE items
            idx <- names(idx[idx])

            # Update non-NULL items
            y[idx] <- x[idx]
        }

        # Assign it back to t1
        # This is like doing t1[[item]] <- x, except that it preserves NULLs.
        # The other form will simply drop NULL values
        t1[item] <- list(y)
    }

    # If either theme is complete, then the combined theme is complete
    attr(t1, "complete") <- attr(t1, "complete") || attr(t2, "complete")
    t1
}
#' @importFrom grid unit gpar gTree
#' @importFrom ggplot2 margin
#' @export
textboxGrob <- function(label, x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                        just = c('left', 'top'), rot = 0, width = NULL,
                        height = NULL, minwidth = NULL, maxwidth = NULL,
                        minheight = NULL, maxheight = NULL, boxjust = 'centre',
                        padding = margin(), radius = NULL,
                        clip = 'inherit', default.units = 'npc', name = NULL,
                        text.gp = gpar(), box.gp = gpar(), vp = NULL) {

    stopifnot(length(label) == 1)
    if (is.null(radius)) radius <- 0

    x <- withUnit(x, default.units)
    y <- withUnit(y, default.units)
    width <- withUnit(width, default.units)
    height <- withUnit(height, default.units)
    minwidth <- withUnit(minwidth, default.units)
    maxwidth <- withUnit(maxwidth, default.units)
    minheight <- withUnit(minheight, default.units)
    maxheight <- withUnit(maxheight, default.units)
    padding <- withUnit(padding, default.units)
    radius <- withUnit(radius, default.units)

    label <- gsub('-', '- ', label)
    label <- strsplit(label, split = ' ', fixed = TRUE)[[1]]
    label <- paste0(label, ' ')
    label <- sub('- ', '-', label)

    gTree(label = label, x = x, y = y, just = just, rot = rot, width = width,
          height = height, minwidth = minwidth, maxwidth = maxwidth,
          minheight = minheight, maxheight = maxheight, boxjust = boxjust,
          padding = padding, radius = radius, rounded = as.numeric(radius) != 0,
          clip = clip, name = name, text.gp = text.gp, box.gp = box.gp, vp = vp,
          cl = 'textboxgrob')
}
#' @importFrom grid convertWidth convertHeight unit textGrob grobWidth grobHeight descentDetails grid.layout viewport vpStack current.viewport
#' @export
makeContext.textboxgrob <- function(x) {
    label <- x$label
    margin_h <- convertWidth(x$padding, 'mm', TRUE)[c(2, 4)]
    margin_v <- convertHeight(x$padding, 'mm', TRUE)[c(1, 3)]
    boxwidth <- if (is.null(x$width)) {
        if (is.null(x$vp)) {
            convertWidth(unit(1, 'npc'), 'mm', TRUE)
        } else {
            if (is.null(x$vp$layout.pos.col)) {
                convertWidth(x$vp$width, 'mm', TRUE)
            } else {
                cur_vp <- current.viewport()
                span <- do.call(seq, as.list(range(x$vp$layout.pos.col)))
                convertWidth(unit(1, 'npc'), 'mm', TRUE) - sum(convertWidth(cur_vp$layout$widths[-span], 'mm', TRUE))
            }
        }
    } else {
        convertWidth(x$width, 'mm', TRUE)
    }
    if (!is.null(x$maxwidth)) {
        boxwidth <- min(boxwidth, convertWidth(x$maxwidth, 'mm', TRUE))
    }
    if (!is.null(x$minwidth)) {
        boxwidth <- max(boxwidth, convertWidth(x$minwidth, 'mm', TRUE))
    }
    boxwidth <- boxwidth - sum(margin_h)
    boxheight <- x$height
    if (!is.null(boxheight)) {
        boxheight <- convertHeight(boxheight, 'mm', TRUE)
        if (!is.null(x$maxheight)) {
            boxheight <- min(boxheight, convertHeight(x$maxheight, 'mm', TRUE))
        }
        if (!is.null(x$minheight)) {
            boxheight <- max(boxheight, convertHeight(x$minheight, 'mm', TRUE))
        }
        boxheight <- boxheight - sum(margin_v)
    }

    lab <- ''
    for (i in label) {
        oldlab <- lab
        lab <- paste0(lab, i)
        tmpGrob <- textGrob(lab, gp = x$text.gp)
        if (convertWidth(grobWidth(tmpGrob), 'mm', TRUE) > boxwidth) {
            lab <- paste(trimws(oldlab), i, sep = '\n')
            if (!is.null(boxheight)) {
                tmpGrob <- textGrob(lab, gp = x$text.gp)
                if (convertHeight(grobHeight(tmpGrob), 'mm', TRUE) > boxheight) {
                    lab <- oldlab
                    break
                }
            }
        }
    }
    lab <- trimws(lab)

    if (is.null(boxheight)) {
        tmpGrob <- textGrob(lab, gp = x$text.gp)
        boxheight <- convertHeight(grobHeight(tmpGrob), 'mm', TRUE) +
            convertHeight(descentDetails(tmpGrob), 'mm', TRUE)
    }
    fullwidth <- boxwidth + sum(margin_h)
    fullheight <- boxheight + sum(margin_v)
    layout <- grid.layout(3, 3,
                          widths = unit(append(margin_h, boxwidth, 1), 'mm'),
                          heights = unit(append(margin_v, boxheight, 1), 'mm'))
    vp <- viewport(x$x, x$y, width = unit(fullwidth, 'mm'),
                   height = unit(fullheight, 'mm'), just = x$boxjust,
                   angle = x$rot, layout = layout)
    if (is.null(x$vp)) {
        x$vp <- vp
    } else {
        x$vp <- vpStack(x$vp, vp)
    }
    x$label <- lab
    x
}
#' @importFrom grid roundrectGrob rectGrob valid.just textGrob viewport setChildren gList
#' @export
makeContent.textboxgrob <- function(x) {
    bg <- if (x$rounded) {
        roundrectGrob(r = x$radius, gp = x$box.gp)
    } else {
        rectGrob(gp = x$box.gp)
    }
    just <- valid.just(x$just)
    text <- textGrob(x$label, x = just[1], y = just[2], just = just,
                     gp = x$text.gp, vp = viewport(layout.pos.row = 2,
                                                   layout.pos.col = 2,
                                                   clip = x$clip))
    setChildren(x, gList(bg, text))
}
#' @export
heightDetails.textboxgrob <- function(x) {
    x$vp$height
}
#' @export
widthDetails.textboxgrob <- function(x) {
    x$vp$width
}
#' @importFrom grid is.unit unit
withUnit <- function(x, default) {
    if (!is.null(x) && !is.unit(x)) {
        x <- unit(x, default)
    }
    x
}
