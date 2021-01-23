##264653 #2a9d8f #e9c46a #f4a261 #e76f51, no gradients.

# Optionally, you can use a black or white background.

# libraries ----
library(data.table)
library(ggplot2)

# functions ----
create_rectangle <- function(corner = c(0, 0), xLength = 2, yLength = xLength)
{
    data.table(
        x = c(corner[1], corner[1] + xLength, corner[1] + xLength, corner[1]),
        y = c(corner[2], corner[2], corner[2] - yLength, corner[2] - yLength)
    ) 
}

# other settings ----
colorPallette <- c('#2a9d8f', '#e9c46a', '#f4a261', '#e76f51')
names(colorPallette) <- letters[1:length(colorPallette)]

set.seed(seed = 126)

# create data to plot ----
# number of rectangles to draw
nRects <- 5000

# minimum and maximum length of side
minSize <- .1
maxSize <- .5

dataPLot <- rbindlist(
    l = lapply(
        X = 1:nRects,
        FUN = function(n)
        {
            thisRect <- create_rectangle(
                corner = c(log(log(n))*cos(n), log(log(n))*sin(n)), # this gives me a spinning effect
                xLength = runif(1, minSize, maxSize)/log(n), 
                yLength = runif(1, minSize, maxSize)/log(n)
                )
            thisRect[, filling := sample(names(colorPallette), size = 1)]
            thisRect[, id := n]
            return(thisRect)
        }
    )
)

# plot itself
ggplot(data = dataPLot[order(-id)], aes(x = x, y = y, fill = filling, group = id)) + 
    geom_polygon() + 
    scale_fill_manual(values = colorPallette) + 
    theme_void() + 
    theme(
        legend.position = "none", aspect.ratio = 1, panel.background = element_rect(fill = '#264653')
    )
ggsave(filename = "results/23_no_gradients.png", height = 3, width = 3, dpi = 1440)
