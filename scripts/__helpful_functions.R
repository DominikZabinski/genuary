#' Title
#'
#' @param center 
#' @param radius 
#' @param dens 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
create_circle <- function(center = c(0, 0), radius = 3, dens = 100, id = "1")
{
    thisCircle <- data.table(t = seq(from = 0, to = 2 * pi, length.out = dens))
    thisCircle[, x := radius * cos(t) + center[1]]
    thisCircle[, y := radius * sin(t) + center[2]]
    return(thisCircle[,.(x, y, id = id)])
}

#' Function to create data for ellipse
#'
#' @param x0 x coordinate of a center
#' @param y0 y coordinate of a center
#' @param rx half of width
#' @param ry half of height
#' @param dens how many point will ellipse have
#'
#' @return
#' @export
#'
#' @examples
create_elipse <- function(x0 = 0, y0 = 0, rx = 1, ry = 1, dens = 10)
{
    d <- rbindlist(
        l = lapply(
            X = seq(from = 0, to = 2 * pi, length.out = dens),
            FUN = function(t){
                data.table(x = rx * cos(t) + x0, y = ry * sin(t) + y0, t = t)
            }
        )
    )
}

#' Calculate distance between two points
#'
#' @param p1 (x,y) coordinates of the first point
#' @param p2 (x,y) coordinates of the second point
#'
#' @return
#' @export
#'
#' @examples
dist_between_points <- function(p1, p2)
{
    sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)
}

#' Function to generate curve
#'
#' @param stepLength length of a single step
#' @param numberOfSteps total number of steps to take
#' @param startingAngle starting direction
#' @param angleChange with each step what will angle change?
#' @param startingPoint starting point
#'
#' @return
#' @export
#'
#' @examples
create_curve <- function(stepLength = 1, numberOfSteps = 24, startingAngle = pi/2, angleChange = pi/32, startingPoint = c(0, 0))
{
    curveData <- data.table(x = startingPoint[1], y = startingPoint[2])
    lastPoint <- startingPoint
    lastAngle <- startingAngle
    for (i in 1:numberOfSteps)
    {
        nextPoint <- c(lastPoint[1] + stepLength * cos(lastAngle), lastPoint[2] + stepLength * sin(lastAngle))
        curveData <- rbind(curveData, data.table(x = nextPoint[1], y = nextPoint[2]))
        lastPoint <- nextPoint
        lastAngle <- lastAngle + angleChange
    }
    return(curveData)
}
