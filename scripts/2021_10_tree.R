# libraries ----
library(data.table)
library(ggplot2)
# functions ----
source("scripts/__helpful_functions.R")
switch_col <- function(present, background)
{
    if (length(present) > 1) return(unlist(lapply(X = 1:length(present), FUN = function(i) switch_col(present[i], background = background))))
    
    if (background == "l")
    {
        switch(EXPR = present, "o" = "l", "l" = "m", "m" = "r", "r" = "o")
    } else if (background == "m")
    {
        switch(EXPR = present, "o" = "m", "l" = "r", "m" = "o", "r" = "l")
    } else if (background == "r")
    {
        switch(EXPR = present, "o" = "r", "l" = "o", "m" = "l", "r" = "m")
    } else
    {
        switch(EXPR = present, "o" = "o", "l" = "l", "m" = "m", "r" = "r")
    }
    
}
# create data ----
# creating first layer
# not-so-random 3 points
seed <- sample(1:45000, 1)#33737
set.seed(seed = seed)
leftCenter <- c(runif(1), runif(1))
angleT <- runif(1, 0, 2 * pi / 5)
midCenter <- c(
    leftCenter[1] + (1 + runif(1, -.1, .1)) * cos(angleT), 
    leftCenter[2] + (1 + runif(1, -.1, .1)) * sin(angleT)
)

angleT2 <- runif(1, -2 * pi / 5, 0)
rightCenter <- c(
    midCenter[1] + dist_between_points(leftCenter, midCenter) * (1 + runif(1, -.1, .1)) * cos(angleT2), 
    midCenter[2] + dist_between_points(leftCenter, midCenter) * (1 + runif(1, -.1, .1)) * sin(angleT2)
)

# establish radiuses
leftRadius <- dist_between_points(leftCenter, midCenter) / runif(1, 2, 3)
midRadius <- dist_between_points(leftCenter, midCenter) - leftRadius
rightRadius <- dist_between_points(rightCenter, midCenter) - midRadius

firstLayer <- rbindlist(l = list(
    create_circle(center = leftCenter, radius = leftRadius, id = "0_l", dens = 1e3),
    create_circle(center = midCenter, radius = midRadius, id = "0_m", dens = 1e3),
    create_circle(center = rightCenter, radius = rightRadius, id = "0_r", dens = 1e3)
))
firstLayer[, col := substr(id, 3, 3)]
firstLayer[, depthOfLayer := 0]

allTheLayers <- copy(firstLayer)

# depth
maxDepth <- 2
thisDepth <- 0

while (thisDepth < maxDepth) 
{
    aboveLayer <- allTheLayers[depthOfLayer == thisDepth]
    # determine square that can contain whole above layer
    aboveSquare <- aboveLayer[,.(x1 = min(x), x2 = max(x), y1 = min(y), y2 = max(y))]
    # it can be rectangle, so lets make an adjustment here
    aboveSquare[, xSide := x2 - x1]
    aboveSquare[, ySide := y2 - y1]
    if (aboveSquare$xSide != aboveSquare$ySide)
    {
        aboveSquareSide <- max(aboveSquare$x2 - aboveSquare$x1, aboveSquare$y2 - aboveSquare$y1)
        if (aboveSquare$xSide > aboveSquare$ySide)
        {
            aboveSquare[, y1 := y1 - (aboveSquare$xSide - aboveSquare$ySide)]
        } else 
        {
            aboveSquare[, x1 := x1 - (aboveSquare$ySide - aboveSquare$xSide)]
        }
    }
    
    aboveSquareCenter <- c(mean(aboveSquare$x1, aboveSquare$x2), mean(aboveSquare$y1, aboveSquare$y2))
    
    # for each circle in the layer
    for (lay in sort(unique(aboveLayer$id)))
    {
        # determine square that fits in this circle
        thisLayer <- aboveLayer[id == lay]
        thisSquare <- thisLayer[,.(x1 = min(x), x2 = max(x), y1 = min(y), y2 = max(y))]
        
        # it can be rectangle, so lets make an adjustment here
        thisSquare[, xSide := x2 - x1]
        thisSquare[, ySide := y2 - y1]
        if (thisSquare$xSide != thisSquare$ySide)
        {
            thisSquareSide <- max(thisSquare$x2 - thisSquare$x1, thisSquare$y2 - thisSquare$y1)
            if (thisSquare$xSide > thisSquare$ySide)
            {
                thisSquare[, y1 := y1 - (thisSquare$xSide - thisSquare$ySide)]
            } else 
            {
                thisSquare[, x1 := x1 - (thisSquare$ySide - thisSquare$xSide)]
            }
        }
        
        thisSquareCenter <- c(mean(thisSquare$x1, thisSquare$x2), mean(thisSquare$y1, thisSquare$y2))
        scaleIt <- thisSquareSide / aboveSquareSide
        # rescale layer so it fits into that square
        secondLayer <- aboveLayer[,
                                  .(
                                      x = (x - aboveSquareCenter[1]) * scaleIt + thisSquareCenter[1], 
                                      y = (y - aboveSquareCenter[2]) * scaleIt + thisSquareCenter[2], 
                                      col,
                                      id,
                                      depthOfLayer = thisDepth + 1
                                  )
                                  ]
        secondLayer[, col := switch_col(present = col, background = thisLayer$col[1])]
        secondLayer[, id := paste0(lay, "_", id, "_", depthOfLayer, "_", col)]
        allTheLayers <- rbindlist(l = list(allTheLayers, secondLayer), use.names = T)
    }
    thisDepth <- thisDepth + 1
}

allTheLayers[,.(.N), by = .(id)][N > 1000]

# coloring pallette
fillPallette <- c("#D46969", "#FECFA8", "#659999", "#86CC86")
names(fillPallette) <- c("o", "l", "m", "r")

ggplot(data = allTheLayers, aes(x = x, y = y, group = id, fill = depthOfLayer, color = depthOfLayer)) + 
    geom_polygon() +
    theme_void() +
    # scale_fill_manual(values = fillPallette) +
    # scale_color_manual(values = fillPallette) +
    scale_fill_gradient(low = "#FFAAAA", high = "#550000") +
    scale_color_gradient(low = "#FFAAAA", high = "#550000") +
    theme(
        # aspect.ratio = 1,
        panel.background = element_rect(fill = fillPallette[1])
    )
message(seed)
