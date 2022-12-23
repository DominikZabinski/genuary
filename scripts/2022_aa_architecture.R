# libraries ----
library(data.table)
library(ggplot2)

# functions ----
calc_dist_pt_pt <- function(p1, p2)
{
    sqrt((p1[1] - p2[1]) ^ 2 + (p1[2] - p2[2]) ^ 2)
}

create_rectangle <- function(x0, y0, width, height, id)
{
    data.table(
        x = c(x0, x0 + width, x0 + width, x0),
        y = c(y0, y0, y0 + height, y0 + height),
        id = id
    )
}

log_pt_lies_rect <- function(point, dataRect)
{
    if (between(point[1], lower = min(dataRect$x), upper = max(dataRect$x)) & between(point[2], lower = min(dataRect$y), upper = max(dataRect$y))) return(TRUE)
    return(FALSE)
}

log_nr_lies_range <- function(nr, range)
{
    if (between(nr, lower = min(range), upper = max(range))) return(TRUE)
    return(FALSE)
}

create_line_eq <- function(p1, p2)
{
    pars <- list(a = 0, b = 0, c = p1[1])
    pars$a <- (p2[2] - p1[2]) / (p2[1] - p1[1])
    pars$b <- p1[2] - pars$a * p1[1]
    return(pars)
}

calc_line_intersection <- function(line1, line2)
{
    if (line1$a == line2$a) return('oo')
    if (is.infinite(line1$a))
    {
        x <- line1$c
        y <- line2$a * x + line2$b
    } else if (is.infinite(line2$a))
    {
        x <- line2$c
        y <- line1$a * x + line1$b
    } else
    {
        x <- (line2$b - line1$b) / (line1$a - line2$a)
        y <- line1$a * x + line1$b
    }
    
    return(c(x, y))
}

persp_front <- function(x, y, z, width = 1, height = 1, id, focusPoint, suf = "_front")
{
    zRef <- calc_zlevel_y(z = 0)
    # create a reference for a front
    # create a front on a given x but on a horizon lvl
    thisFrontRef <- data.table(
        x = c(x * cubeSide + focusPoint[1], x * cubeSide + focusPoint[1] + width, x * cubeSide + focusPoint[1] + width, x * cubeSide + focusPoint[1]),
        y = c(zRef, zRef, zRef + height, zRef + height),
        id = id
    )
    
    # create lines from focus point to a base of front on a horizon lvl
    linesFrontFocus <- lapply(
        X = 1:2, 
        FUN = function(i)
        {
            create_line_eq(p1 = focusPoint, p2 = c(thisFrontRef$x[i], thisFrontRef$y[i]))
        }
    )
    
    # create a front on a given x and y but on a horizon lvl
    thisFrontProp <- data.table(
        x = c(x * cubeSide + focusPoint[1], x * cubeSide + focusPoint[1] + width, x * cubeSide + focusPoint[1] + width, x * cubeSide + focusPoint[1]),
        y = c(y * cubeSide + zRef, y * cubeSide + zRef, y * cubeSide + zRef + height, y * cubeSide + zRef + height),
        id = id
    )
    
    # create lines from focus point to a front on a given x and y but on a horizon lvl
    linesFrontFocusProp <- lapply(
        X = 1:4, 
        FUN = function(i)
        {
            create_line_eq(p1 = focusPoint, p2 = c(thisFrontProp$x[i], thisFrontProp$y[i]))
        }
    )
    
    # create intersection between lines from a bottom side of a front on horzion lvl and the given z line
    trZY <- calc_zlevel_y(z = z)
    imageFrontOnZ <- rbindlist(
        l = list(
            as.data.table(t(calc_line_intersection(line1 = linesFrontFocus[[1]], line2 = list(a = 0, b = trZY)))),
            as.data.table(t(calc_line_intersection(line1 = linesFrontFocus[[2]], line2 = list(a = 0, b = trZY))))
        )
    )
    setnames(imageFrontOnZ, names(imageFrontOnZ), c("x", "y")) 
    
    # calculate intersecionts between lines from above intersections and lines from image on x and y but on a horizon lvl
    lastFront <- rbindlist(
        l = lapply(
            X = 1:4,
            FUN = function(ii)
            {
                pp <- 2
                if (ii %in% c(1, 4)) pp <- 1
                as.data.table(t(calc_line_intersection(line1 = linesFrontFocusProp[[ii]], line2 = list(a = Inf, b = 0, c = imageFrontOnZ$x[pp]))))    
            }
        )
    )
    setnames(lastFront, names(lastFront), c("x", "y")) 
    lastFront[, id := paste0(id, suf)]
    return(lastFront[])
}

lies_in_range <- function(x, values)
{
    if (between(x, min(values), max(values))) return(T)
    return(F)
}

persp_cube <- function(x, y, z, id, focusPoint, width, height)
{
    front <- persp_front(x = x, y = y, z = z, id = id, focusPoint = focusPoint, suf = "_front", width = width, height = height)
    back <- persp_front(x = x, y = y, z = z - 1, id = id, focusPoint = focusPoint, suf = "_back", width = width, height = height)
    listOfThings <- list(front)
    # this shodul be dependent on whether x/y lies where in reference to focusPoint
    # top/bottom
    if (!lies_in_range(focusPoint[2], front$y[c(1, 4)]))
    {
        pp <- c(3, 4)
        if (focusPoint[2] < front$y[1])
            pp <- c(1, 2)
        topBottom <- data.table(x = c(front$x[pp], back$x[rev(pp)]), y = c(front$y[pp], back$y[rev(pp)]), id = paste0(id, "_tb"))
        listOfThings <- c(listOfThings, list(topBottom))
    }
    
    if (!lies_in_range(focusPoint[1], front$x[c(1, 2)]))
    {
        pp <- c(2, 3)
        if (focusPoint[1] < front$x[1])
            pp <- c(1, 4)
        side <- data.table(x = c(front$x[pp], back$x[rev(pp)]), y = c(front$y[pp], back$y[rev(pp)]), id = paste0(id, "_side"))  
        listOfThings <- c(listOfThings, list(side))
    }
    
    rbindlist(
        l = listOfThings
    )
}

make_steps <- function(n, startX = 0, startY = 0, startZ = 0)
{
    steps = c(2)
    n2 <- n - 2
    while(sum(steps) < n)
    {
        m <- max(steps)
        k <- which(steps == m)
        if (length(k) == 2)
        {
            m <- m + 1
        } 
        add <- min(m, n2)
        steps[length(steps) + 1] <- add
        n2 <- n2 - add
    }
    da <- data.table(x = startX, y = startY, z = startZ)
    for (ii in 2:length(steps))
    {
        ll <- da[nrow(da)]
        if (ii %% 4 == 1)
        {
            ff <- data.table(x = ll$x, y = ll$y + 1, z = ll$z:(ll$z+steps[ii]))
        } else if (ii %% 4 == 3)
        {
            ff <- data.table(x = ll$x, y = ll$y + 1, z = ll$z:(ll$z-steps[ii]))
        } else if (ii %% 4 == 2)
        {
            ff <- data.table(x = ll$x:(ll$x+steps[ii]), y = ll$y + 1, z = ll$z)
        } else
        {
            ff <- data.table(x = ll$x:(ll$x-steps[ii]), y = ll$y + 1, z = ll$z)
        }
        da <- rbind(da, ff)
    }
    return(da[2:nrow(da)])
}
# data ----
focusPoint <- c(5, 5)
horizonAt <- 3

calc_zlevel_y <- function(z, yh = horizonAt, yp = focusPoint[2], a = .15)
{
    (yh - yp) * exp(a * z) + yp
}

calc_zlevel_y(z = 0)

zis <- data.table(zlev = seq(-4, 4, length.out = 9))
zis[, y := calc_zlevel_y(z = zlev)]

cubeSide <- 2 * (calc_zlevel_y(z = -1) - calc_zlevel_y(z = 0))
# width and heigth as relative unit as cubeside
# x and y in units
# x is relative to focusPoint[1]
cubesAll <- make_steps(n = 120, startX = -5.5, startY = -5, startZ = 3)
cubesAll[, id := as.character(1:nrow(cubesAll))]
cubesAll[, c("width", "height") := 1]

dataCubesAll <- rbindlist(
    l = lapply(
        X = cubesAll$id,
        FUN = function(i)
        {
            thisCube <- cubesAll[id == i]
            # multiple by cubeside unit
            persp_cube(
                x = thisCube$x, y = thisCube$y, z = thisCube$z, id = thisCube$id, focusPoint = focusPoint, 
                width = thisCube$width * cubeSide, height = thisCube$height * cubeSide
            ) 
        }
    )
)

as1 <- list(
    x = range(dataCubesAll$x),
    y = range(dataCubesAll$y)
)

as <- c(
    as1$x[2] - as1$x[1],
    as1$y[2] - as1$y[1]
)

pp <- 0.05
adder <- list(
    x = c(as1$x[1] - pp * as[1], as1$x[2] + pp * as[1]),
    y = c(as1$y[1] - pp * as[2], as1$y[2] + pp * as[2])
)

# establish order
# from most back, left, bottomm
cubesOrder <- dataCubesAll[,.(xmin = min(x), ymin = min(y)), by = .(idSide = id)]
cubesOrder[, idCube := substr(idSide, 1, regexpr(pattern = "_", text = idSide) - 1)]
cubesOrder[, sideType := substr(idSide, regexpr(pattern = "_", text = idSide) + 1, nchar(idSide))]
cubesOrder[, sideType := factor(sideType, levels = c("side", "tb", "front"), ordered = T)]

cubesOrder <- merge(cubesOrder, cubesAll[,.(idCube = id, z)], by = "idCube")
cubesOrder <- cubesOrder[order(z, sideType, -abs(focusPoint[1] - xmin), -abs(focusPoint[2] - ymin))]

# order should be based on the position relative to cfocus point

ggplot() + 
    geom_point(data = data.table(x = focusPoint[1], y = focusPoint[2]), aes(x = x, y = y)) +
    lapply(
        X = cubesOrder$idSide,
        FUN = function(ii)
        {
            dt <- dataCubesAll[id == ii]
            geom_polygon(data = dt, aes(x = x, y = y, group = id), fill = "yellow", color = "black", size = .3)
        }
    ) +
    theme_void() +
    theme(
        aspect.ratio = as[2] / as[1],
        panel.background = element_rect(fill = "black")
    ) + 
    coord_fixed(xlim = adder$x, ylim = adder$y)
he <- 10
ggsave(filename = "results/2022_25_meets_9.png", height = he, width = he * 9 / 16, dpi = 720)
file.show("results/2022_25_meets_9.png")
