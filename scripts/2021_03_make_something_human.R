# make something human
# libraries ----
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

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

# single creation
ggplot(data = humanData, aes(x = x, y = y, group = drawId, alpha = id2)) + 
    geom_path() +
    theme_void() +
    theme(aspect.ratio = 2/1, legend.position = "none")

# actual plotting ----
# lets get it warhol-ed
humanDataWarhol <- rbindlist(
    l = lapply(
        X = 1:8, 
        FUN = function(w){
            f <- copy(humanData)
            f[, idWarhol := as.character(w)]
            return(f)
        }
    )
)

backgroundWarhol <- c("#e7422d", "#e43d2d", "#708d69", "#4f7c67", "#3f8abd", "#6592a5", "#dd2d37", "#e93e37")
names(backgroundWarhol) <- c(1:8)

colorsWarhol <- c("#e8a9bc", "#d580a6", "#f19796", "#dd7379", "#eb6083", "#ee6887", "#f6c5bf", "#e29b9a")
names(colorsWarhol) <- c(1:8)

# i'd prefer using lapply() in grid.arrange, but for some reason it throws errors, so I trun to good, old-fashioned for loop
for (i in c(1:8))
{
    humanPlotTmp <- ggplot(data = humanDataWarhol[idWarhol == i], aes(x = x, y = y, group = drawId, alpha = id2, color = idWarhol)) + 
        geom_path() +
        theme_void() +
        facet_wrap(~idWarhol, nrow = 2) +
        scale_color_manual(values = colorsWarhol) +
        theme(
            aspect.ratio = 2/1, legend.position = "none", 
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            panel.background = element_rect(fill = backgroundWarhol[i], color = NA), 
            panel.spacing = unit(x = 0, units = "npc")
        )
    assign(x = paste0("humanPlot", i), value = humanPlotTmp)
}


png(filename = "results/03_make_something_human.png", width = 6, height = 6, units = "in", res = 720)
grid.draw(
    arrangeGrob(
        humanPlot1, 
        humanPlot2, 
        humanPlot3, 
        humanPlot4, 
        humanPlot5, 
        humanPlot6,
        humanPlot7, 
        humanPlot8, 
        nrow = 2)
)
dev.off()
