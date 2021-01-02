# rule 30: https://en.wikipedia.org/wiki/Rule_30
# libraries ----
library(data.table)
library(ggplot2)

# functions ----
#' Function to calculate rule 30
#'
#' @param a left neighbour of the cell
#' @param b actual cell
#' @param c right neighbour of the cell
#'
#' @return
#' @export
#'
#' @examples
rule30 <- function(a, b, c)
{
    if (length(a) == 0) a <- 0
    if (is.na(c)) c <- 0
    bc <-  b | c
    (a | bc) & !(a & bc) 
}

# check if rule30 yield correct results
for(i in c(T, F))
{
    for(j in c(T,F))
    {
        for(k in c(T, F))
        {
            message(sprintf("%s %s %s %s", i*1, j*1, k*1, rule30(i, j, k)*1))
        }
    }
}

# genuary code ----
# number of cells (make sure its odd, for better results)
n <- 211

# determine halfway and starting status
halfway <- (n - 1) / 2
cellRow <- c(rep(0, halfway), 1, rep(0, halfway))

# create data
ruleData <- data.table(xAxis = 1:n, yAxis = 0, cell = cellRow)
for (gen in 1:halfway)
{
    thisRow <- c()
    for (cell in 1:n)
    {
        thisRow <- c(thisRow, rule30(a = cellRow[cell - 1], b = cellRow[cell], c = cellRow[cell + 1]))
    }
    ruleData <- rbind(ruleData, data.table(xAxis = 1:n, yAxis = gen, cell = thisRow))
    cellRow <- thisRow
}

# calculate distance from given point - its gonna be helpful for shading
givenPoint <- c(0, 0)
ruleData[, dist := sqrt((xAxis - givenPoint[1]) ^ 2 + (yAxis - givenPoint[2]) ^ 2)]
ruleData[, dist := sqrt(dist / max(dist))]

# establish colors
fill0 <- "#000000"
fill1 <- "#F8F8FF"

# plot the plot ...
ggplot() + 
    geom_tile(data = ruleData[cell == 0], aes(x = xAxis, y = yAxis, alpha = dist), fill = fill0) +
    geom_tile(data = ruleData[cell == 1], aes(x = xAxis, y = yAxis, alpha = 1 - dist), fill = fill1) +
    theme_void() +
    theme(legend.position = "none") + 
    scale_y_reverse() 

# ... and save it
dir.create(path = "results/", showWarnings = F)
ggsave(filename = "results/02_rule30.png", height = 3, width = 6, dpi = 720)
