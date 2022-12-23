# // One process grows, another process prunes.
# libraries ----
library(data.table)
library(ggplot2)
source("scripts/__helpful_functions.R")
# create data ----
create_branch <- function(n = 250)
{
    seed <- sample(1:5000, 1)
    set.seed(seed)
    # n <- 250
    startingPoint <- c(runif(1, 0, 500), 0)
    i <- 1
    wholeData <- data.table()
    while (i < n)
    {
        # growing part
        # create curve and mirror image of it
        thisGrowth <- create_curve(
            stepLength = runif(1, 1, 2), 
            startingPoint = startingPoint, 
            numberOfSteps = sample(15:25, 1), 
            angleChange = runif(1, pi/128, pi/20), 
            startingAngle = runif(1, 0, 2 * pi)
        )
        thisGrowth[, id := 2 * i - 1]
        thisGrowthRows <- nrow(thisGrowth)
        mirrorGrowth <- thisGrowth[,.(x = 2 * startingPoint[1] - x, y = 2 * startingPoint[2] - y, id = 2 * i)]
        
        # pruning part
        # decide which (growth or its image) to prune
        if (runif(1) < .5)
        {
            thisGrowth <- thisGrowth[1:floor(thisGrowthRows * runif(1, .25, .75))]
            startingPoint <- c(mirrorGrowth[nrow(mirrorGrowth)]$x, mirrorGrowth[nrow(mirrorGrowth)]$y)
        } else 
        {
            mirrorGrowth <- mirrorGrowth[1:floor(thisGrowthRows * runif(1, .25, .75))]
            startingPoint <- c(thisGrowth[nrow(thisGrowth)]$x, thisGrowth[nrow(thisGrowth)]$y)
        }
        
        wholeData <- rbindlist(l = list(wholeData, thisGrowth, mirrorGrowth))
        
        i <- i + 1
    }
    wholeData[, s := seed]
    wholeData[, ss := id / (2*n)]
    return(wholeData)
}


# plot ----
wholeData <- rbindlist(lapply(X = 1:5, FUN = function(x) create_branch()))
wholeData[, ff := paste0(s, "_", id)]

mm <- ggplot(
    data = wholeData, 
    aes(
        x = x, y = y, group = ff, 
        # alpha = ss, 
        color = ss
        # ,
                          # size = log(id + 1)
        )
    ) + 
    geom_path(lineend = "round", size = 1) + 
    # scale_size_continuous(range = c(1, 2)) +
    scale_color_gradient(low = "#C9EA9C", high = "#2D4E00")+
    # scale_color_viridis_c()+
    theme_void() + 
    theme(
        legend.position = "none", panel.background = element_rect(fill = "#805D15")
    ) 
mm
mm + facet_wrap(~s, scales = "free")

