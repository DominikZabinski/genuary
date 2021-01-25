# libraries ----
library(data.table)
library(ggplot2)

# functions ----
totalMoves <- function(movesSide)
{
    factorial(2 * movesSide) / (factorial(movesSide) * factorial(movesSide))
}

create_path_polygons <- function(path = "RDR", startingPoint = c(0, 0), stepPath = 1)
{
    steps <- unlist(strsplit(x = path, split = ""))
    xAxis <- c(startingPoint[1])
    yAxis <- c(startingPoint[2])
    lastPoint <- startingPoint
    for (step in steps)
    {
        if (step == "D")
        {
            lastPoint[2] <- lastPoint[2] - stepPath
        } else
        {
            lastPoint[1] <- lastPoint[1] + stepPath
        }
        xAxis <- c(xAxis, lastPoint[1])
        yAxis <- c(yAxis, lastPoint[2])
    }
    data.table(x = xAxis, y = yAxis)
}

# main idea ----
# assume we have n x n grid and we want to set a path from cell (1,1) to cell (n,n) while we can onyl move to the right (R) or down (D). how many possible paths are there? this is a problem when we need to think about permutations. permutations of steps D and R
# create data ----
# how many moves (maximum) we can make in one direction?
movesSide <- 5

# how many paths are there?
totalMoves(movesSide)

# for the nice grid we well use a square setup
pathsNeeded <- 7 ^ 2
pathsChosen <- c()
pathsCount <- 0

# we could make all the possible permutations and randomize that many paths that we need. all we can randomize path, check if we already have it: if yes, randomize again; if not randomize again, but increase a counter and store that path
# that in mind, we have to pick reasonalbe number for total number of moves and number of paths that we will need

# create paths
set.seed(seed = 1234)

while (pathsCount < pathsNeeded)
{
    thisPath <- sample(x = c(rep("D", movesSide), rep("R", movesSide)), size = 2 * movesSide, replace = F)
    if (!paste0(thisPath, collapse = "") %in% pathsChosen)
    {
        pathsCount <- pathsCount + 1
        pathsChosen <- c(pathsChosen, paste0(thisPath, collapse = ""))
    }
}

# length of step
stepPath <- 1

# point representations of paths
dataPaths <- rbindlist(
    l = lapply(
        X = 1:pathsNeeded,
        FUN = function(pathNumber){
            whichRow <- ceiling(pathNumber / sqrt(pathsNeeded))
            whichColumn <- pathNumber - (whichRow  - 1) * sqrt(pathsNeeded)
            create_path_polygons(
                path = pathsChosen[pathNumber], 
                startingPoint = c((whichColumn - 1) * (movesSide + 3) , (whichRow - 1) * (movesSide + 3)), 
                stepPath = stepPath
            )
        }
    )
)

# plot ----
ggplot() + 
    geom_point(data = dataPaths, aes(x = x, y = y), color = "yellow", size = .5) + 
    theme_void() + 
    theme(
        panel.background = element_rect(fill = "black"),
        aspect.ratio = 1
    )
ggsave(filename = "results/25_grid_of_permutations.png", height = 3, width = 3, dpi = 1440)
