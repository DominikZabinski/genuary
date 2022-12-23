library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(magick)
library(png)
# functions ----
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

#' Function to trim ellipse data
#'
#' @param data data.table - product of create_elipse()
#' @param perRem percenatage of points to remove
#'
#' @return
#' @export
#'
#' @examples
trim_it <- function(data, perRem = runif(n = 1, min = 0, max = .2))
{
    # remove first point (overlaps with last)
    d <- data[-1]
    
    # remove some random points
    howManyToRemove <- round(perRem * nrow(d))
    startRemoving <- sample(x = 1:nrow(d), size = 1)
    toRemove <- c(startRemoving:(startRemoving + howManyToRemove)) %% nrow(d) + 1
    d[, rowNumber := 1:nrow(d)]
    d <- d[-toRemove]
    
    # reorder, if neeed
    if (startRemoving + howManyToRemove <= max(d$rowNumber))
    {
        d <- rbindlist(list(d[rowNumber >= startRemoving + howManyToRemove], d[rowNumber < startRemoving + howManyToRemove]))
    }
    d[, rowNumber := NULL]
    return(d)
}

#' Function to creat whole dataset for a single body part
#'
#' @param partParams named list: y0, x0, rx, ry
#'
#' @return
#' @export
#'
#' @examples
make_a_part <- function(partParams, partName)
{
    partData <- rbindlist(
        l = lapply(
            X = 1:tries, 
            FUN = function(try){
                d <- create_elipse(
                    x0 = partParams$x0 + runif(n = 1, min = -shakePar, max = shakePar), 
                    y0 = partParams$y0 + runif(n = 1, min = -shakePar, max = shakePar), 
                    rx = partParams$rx, ry = partParams$ry, dens = densGlobal
                )
                d <- trim_it(d)
                d[, id := try]
                return(d)
            }
        )
    )
    partData[, part := partName]
    return(partData)
}

# settings ----
# number of tries 'human' will make in attempt to draw a part
tries <- 15
# shake - margin of error for initial point of origin
shakePar <- .15
# density - number of initial points that makes an ellipse
densGlobal <- 150

# define parameters for parts
headHeight <- 1
partsParameters <- list(
    head = list(x0 = 0, y0 = 0, rx = .5, ry = headHeight / 2),
    upperBody = list(x0 = 0, y0 = - headHeight, rx = 1.5, ry = 3 * headHeight / 2),
    torso = list(x0 = 0, y0 = -2 * headHeight, rx = .5, ry = 2 * headHeight / 2),
    leftLeg = list(x0 = -1, y0 = - 5 * headHeight, rx = .5, ry = 4 * headHeight / 2),
    rightLeg = list(x0 = 1, y0 = - 5 * headHeight, rx = .5, ry = 4 * headHeight / 2) 
)
# create data ----
# create data for each part
head <- make_a_part(partsParameters$head, partName = "head")
upperBody <- make_a_part(partsParameters$upperBody, partName = "upperBody")
torso <- make_a_part(partsParameters$torso, partName = "torso")
leftLeg <- make_a_part(partsParameters$leftLeg, partName = "leftLeg")
rightLeg <- make_a_part(partsParameters$rightLeg, partName = "rightLeg")

humanData <- rbindlist(
    l = list(
        head, upperBody, torso, leftLeg, rightLeg
    )
)
humanData[, drawId := paste0(part, "_", id)]
humanData[, id2 := id / max(id)]

backgroundWarhol <- c("#e7422d", "#e43d2d", "#708d69", "#4f7c67", "#3f8abd", "#6592a5", "#dd2d37", "#e93e37")
colorsWarhol <- c("#e8a9bc", "#d580a6", "#f19796", "#dd7379", "#eb6083", "#ee6887", "#f6c5bf", "#e29b9a")
pp <- ggplot() + 
    geom_path(data = humanData, aes(x = x, y = y, group = drawId, alpha = id2), color = colorsWarhol[1], size = .5) +
    theme_void() +
    theme(
        aspect.ratio = 2/1, legend.position = "none",
        panel.background = element_rect(fill = backgroundWarhol[1], color = NA), 
        )
pp
ggsave(filename = "results/2022_02_base.png", width = 1.5, height = 3, units = "in", dpi = 720)
file.show("results/2022_02_base.png")


# wczytaj, podziel na prostokaty, wybrane prostokaty poddaj ditherowaniu, pozostale utrzymaj w kolorze

img <- image_read(path = "results/2022_02_base.png")
imgGreyScale <- image_convert(image = img, type='grayscale')
matrixBase <- as_EBImage(image = img)@.Data
matrixGrey <- as_EBImage(image = imgGreyScale)@.Data


dither_area <- function(mat, x0, y0, xend, yend)
{
    ditherWeights <- data.table(
        w = c(7, 3, 5, 1) / 16, 
        c = c(1, -1, 0, 1), 
        r = c(0, 1, 1, 1)
    )
    
    for (i in (x0-1):(xend + 1))
    {
        if (i < 1 | i > ncol(mat)) next()
        for (j in (y0-1):(yend + 1))
        {
            if ((j > nrow(mat)) | (j < 1)) next()
            gg <- tryCatch(expr = {mat[j, i]}, error = function(e) message(sprintf("%s-%s", i, j)))
            val <- ifelse(gg > runif(1), 1, 0)
            err <- gg - val
            mat[j, i] <- val
            for (ii in 1:nrow(ditherWeights))
            {
                resCol <- i + ditherWeights$r[ii]
                resRow <- j + ditherWeights$c[ii]
                if ((resRow <= nrow(mat)) & (resRow > 0) & (resCol <= ncol(mat)) & (resCol > 0))
                {
                    mat[resRow, resCol] <- err * ditherWeights$w[ii] + mat[resRow, resCol]
                }
            }
        }
    }
    return(mat)    
}
spotsX <- floor(c(runif(1, .25, .45), runif(1, .55, .85)) * ncol(matrixGrey))
spotsY <- floor(c(runif(1, .25, .45), runif(1, .55, .85)) * nrow(matrixGrey))
# dither from left to right, from top to bottom
areas <- list(
    a = list(x0 = 1, y0 = 1, xend = spotsX[1], yend = spotsY[1]),
    b = list(x0 = spotsX[1], y0 = spotsY[1], xend = spotsX[2], yend = spotsY[2]),
    c = list(x0 = spotsX[2], y0 = spotsY[2], xend = ncol(matrixGrey), yend = nrow(matrixGrey)),
    aa = list(x0 = 1, xend = spotsX[1], y0 = spotsY[2], yend = nrow(matrixGrey)),
    sd = list(x0 = spotsX[2], xend = ncol(matrixGrey), y0 = 1,  yend = spotsY[1])
)

for (i in 1:length(areas))
{
    matrixGrey <- dither_area(mat = matrixGrey, x0 = areas[[i]]$x0, y0 = areas[[i]]$y0, xend = areas[[i]]$xend, yend = areas[[i]]$yend)
}

# combine matrixes
combine_matrixes <- function(matBase, matOver, areas = list())
{
    gg <- function(x)
    {
        codsa <- c(0, 0, 0, 1)
        if (x == 1) codsa <- c(255, 255, 255, 0)
        return(codsa)
    }
    for (nn in 1:length(areas))
    {
        for (i in areas[[nn]]$x0:areas[[nn]]$xend)
        {
            for (j in areas[[nn]]$y0:areas[[nn]]$yend)
            {
                matBase[j, i, ] <- gg(matOver[j, i])
            }
        }
    }
    return(matBase)
}
combinedMatrix <- combine_matrixes(matBase = matrixBase, matOver = matrixGrey, areas = areas)
writePNG(image = combinedMatrix, target = "results/2022_02.png")
file.show("results/2022_02.png")
