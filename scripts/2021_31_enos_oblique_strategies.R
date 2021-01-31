# SEARCH FOR "ENO'S OBLIQUE STRATEGIES"
# Breathe more deeply
# libraries ----
library(ggplot2)
library(data.table)

# functions ----
create_polygon <- function(number = 3, id = 4)
{
    # calculate inner angle
    angle <- pi * (number - 2) / number
    # every step change the angle
    additionalAngle <- pi - angle
    xAxis <- c(0)
    yAxis <- c(0)
    thisPoint <- c(0, 0)
    for (i in 1:(number - 1))
    {
        thisPoint <- thisPoint + 1 * c(cos(angle - (i-1) * additionalAngle), sin(angle - (i-1) * additionalAngle))
        xAxis <- c(xAxis, thisPoint[1])
        yAxis <- c(yAxis, thisPoint[2])
    }
    return(data.table(x = xAxis, y = yAxis, id = id))
}

fR <- function(t, t2)
{
    b <- (range(t)[2] - range(t)[1]) / 2
    c(mean(range(t2)) - b, mean(range(t2)) + b)
}

# create data ----
maxP <- 13
polygons <- rbindlist(l = lapply(X = maxP:3, FUN = function(x) create_polygon(number = x, id = maxP - x)))
polygonsMirror <- polygons[,.(x = x, y = 2 * max(polygons$y) - y, id = id + max(polygons$id) + 1)]

polygonData <- rbind(polygons, polygonsMirror)
polygonData[, toFill := .N, by = .(id)]

# plot ----
ggplot(data = polygonData, aes(x = x, y = y, group = id, fill = toFill)) +
    geom_polygon() +
    theme_void() +
    scale_fill_gradient2(low = "#395865", high = "#305A53") +
    theme(
        aspect.ratio = 1,
        legend.position = "none",
        panel.background = element_rect(fill = "#2B514A")
    ) + 
    coord_fixed(ratio = 1, xlim = fR(range(polygonData$y), range(polygonData$x)), ylim = range(polygonData$y), expand = F)
ggsave(filename = "results/31_enos.png", height = 3, width = 3, dpi = 1440)
