#'
#' @title Create a grob representing an arrow
#'
#' @description Function to create a grob representing an arrow.
#'
#' @param x : x-axis location of base (passed on to [grid::grob()])
#' @param y : y-axis location of base (passed on to [grid::grob()])
#' @param angle : angle of shaft  (passed on to [grid::grob()])
#' @param length : length of shaft  (passed on to [grid::grob()])
#' @param pivot :  passed on to [grid::grob()]
#' @param default.units : (passed on to [grid::grob()])
#' @param ... : passed on to [grid::grob()]
#'
#' @return a \pkg{grid} grob.
#'
#' @details Code stolen from from Elio Campitelli (https://github.com/eliocamp/metR) and adapted
#' by WTS. Uses [grid::grob()] with \code{cl="arrow2"}.
#'
#' @importFrom grid grob
#'
arrowGrob <- function(x, y, angle, length, pivot, default.units = "npc", ...) {
    x <- .unit_ifnot(x, default.units);
    y <- .unit_ifnot(y, default.units);

    grid::grob(x = x, y = y, angle = angle, length = length, pivot = pivot, ..., cl = "arrow2");
}

#'
#' @title Make content for an arrow
#'
#' @description Function to make content for an arrow.
#'
#' @param x : a grid grob
#'
#' @return a grid grob or gTree
#'
#' @details This function will be used by grid.draw when called on an arrowGrob. It
#' provides a specific implementation for the generic [grid::makeContent()].
#'
#' @importFrom grid convertUnit
#' @importFrom grid convertX
#' @importFrom grid convertY
#' @importFrom grid makeContent
#' @importFrom grid unit.c
#' @importFrom memoise memoise
#'
#' @export
#'
makeContent.arrow2 <- function(x) {
    #--A memoised copy is basically a lazier version of the same function:
    #--it saves the answers of new invocations, and re-uses the answers of old ones.
    #--Under the right circumstances, this can provide a very nice speedup indeed.
    fast.unit.c <- memoise::memoise(grid::unit.c);

    x$id <- rep(seq(length(x$x)), 2)
    x$x  <- grid::convertX(x$x, 'mm')
    x$y  <- grid::convertY(x$y, 'mm')
    dx   <- grid::convertUnit(x$length, "mm")*cos(x$angle*pi/180)
    dy   <- grid::convertUnit(x$length, "mm")*sin(x$angle*pi/180)

    x$x <- fast.unit.c(x$x - dx*x$pivot,
                       x$x + dx*(1-x$pivot))

    x$y <- fast.unit.c(x$y - dy*x$pivot,
                       x$y + dy*(1-x$pivot))


    x$cl <- "polyline"
    class(x)[1] <- "polyline"
    x
}

#'
#' @title Create a grob defining a geometric vector
#'
#' @description
#' Function to create a grob defining a geometric vector.
#'
#' @param x : x-axis location of base (passed on to [grid::grob()])
#' @param dx : x-axis offset to tip of vector
#' @param y : y-axis location of base (passed on to [grid::grob()])
#' @param dy : y-axis offset to tip of vector
#' @param length : passed on to [grid::grob()]
#' @param preserve.dir : passed on to [grid::grob()]
#' @param default.units : units to attach to x, dx, y, or dy (if not already attached )
#' @param pivot : passed on to [grid::grob()]
#' @param ... : passed on to [grid::grob()]
#'
#' @return grob describing a vector
#'
#' @details Basically a pass-through for [grid::grob()]
#'
#' @importFrom grid grob
#'
vectorGrob <- function(x, y, dx, dy, length, preserve.dir,
                       default.units = "npc", pivot, ...) {
    # angle <- atan2(yend - y, xend - x)
    x  <- .unit_ifnot(x, default.units)
    y  <- .unit_ifnot(y, default.units)
    dx <- .unit_ifnot(dx, default.units)
    dy <- .unit_ifnot(dy, default.units)

    grid::grob(x = x, y = y, dx = dx, dy = dy, length = length, pivot = pivot,
               preserve.dir = preserve.dir, ..., cl = "vector")
}

#'
#' @title Make content for an arrow
#'
#' @description Function to make content for a vectorGrob object.
#'
#' @param x : a vectorGrob
#'
#' @return a grid grob or gTree with class \code{polyline}.
#'
#' @details This function will be used by grid.draw when called on a vectorGrob. It
#' provides a specific implementation for the generic [grid::makeContent()].
#'
#' @importFrom grid convertHeight
#' @importFrom grid convertWidth
#' @importFrom grid convertX
#' @importFrom grid convertY
#' @importFrom grid makeContent
#' @importFrom grid unit.c
#'
#' @export
#'
makeContent.vector <- function(x) {
    x$id <- rep(seq(length(x$x)), 2)
    x$x  <- grid::convertX(x$x, 'mm')
    x$y  <- grid::convertY(x$y, 'mm')
    x$dx <- grid::convertWidth(x$dx, 'mm')
    x$dy <- grid::convertHeight(x$dy, 'mm')

    x$angle <- atan2(as.numeric(x$dy), as.numeric(x$dx))

    x$dx <- x$length*cos(x$angle)
    x$dy <- x$length*sin(x$angle)

    x$x <- grid::unit.c(x$x - x$dx*x$pivot,
                        x$x + x$dx*(1 - x$pivot))

    x$y <- grid::unit.c(x$y - x$dy*x$pivot,
                        x$y + x$dy*(1 - x$pivot))

    x$cl <- "polyline"
    class(x)[1] <- "polyline"
    x
}

#'
#' @title Convert quantity to one with units (if unitless)
#'
#' @description Function to convert quantity to one with units (if unitless).
#'
#' @param x : value to attach units
#' @param unit : units to attach
#'
#' @return \code{x} with units attached
#'
#' @details Uses [grid::unit()] to attach units.
#'
#'
#' @importFrom grid is.unit
#' @importFrom grid unit
#'
.unit_ifnot <- function(x, unit) {
    if (!grid::is.unit(x)) {
        x <- grid::unit(x, unit)
    }
    x
}
