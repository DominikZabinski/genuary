# curves only
# libraries ----
library(data.table)
library(ggplot2)

# functions ----
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
# main body ----
# generate parameters for the curve
set.seed(seed = 95661)
startingPoint <- c(0, 0)
startingAngle <- 2*pi/5
numberBreaks <- 50

wholeCurveParam <- list()
for (i in 1:numberBreaks)
{
    wholeCurveParam[[i]] <- list(angleChange = runif(1, pi/128, pi/20), numberOfSteps = sample(20:40, 1), stepLength = runif(1, 1, 2))
    if (i %%2 != 0)
    {
        wholeCurveParam[[i]]$angleChange <- -wholeCurveParam[[i]]$angleChange
    }
}

# create data for each on point on the curve
wholeCurve <- data.table()

for (i in 1:length(wholeCurveParam))
{
    thisParams <- wholeCurveParam[[i]]
    thisCurve <- create_curve(startingAngle = startingAngle, startingPoint = startingPoint, angleChange = thisParams$angleChange, numberOfSteps = thisParams$numberOfSteps, stepLength = thisParams$stepLength)
    startingPoint <- c(thisCurve$x[thisParams$numberOfSteps + 1], thisCurve$y[thisParams$numberOfSteps + 1])
    startingAngle <- startingAngle + thisParams$numberOfSteps * thisParams$angleChange
    wholeCurve <- rbind(wholeCurve, thisCurve)
}

# lets see what we got (actual process involves selecting random seed until we are happy with final result)
ggplot() + 
    geom_path(data = wholeCurve, aes(x = x, y = y), color = "black", size = 1) + 
    theme_void() + 
    theme(
        legend.position = "none", aspect.ratio = 1, 
        plot.margin = unit(rep(-10, 4), "pt"))

# replicate curve - this will be some kind of shadow
n <- 25
replicCurve <- rbindlist(
    l = lapply(
        X = 1:n, 
        FUN = function(rep){
            thisRep <- rbind(wholeCurve[,.(x = x + .2*rep, y = y, id = rep)])
            return(thisRep)
        }
    )
)
replicCurve[, sat := 1 - id / (n + 1)]

# final plot
ggplot() + 
    geom_path(data = wholeCurve, aes(x = x, y = y), color = "#00203FFF", size = 1.5) + 
    geom_path(data = replicCurve, aes(x = x, y = y, group = id, alpha = sat), color = "#ED2B33FF", size = .1) + 
    theme_void() + 
    theme(
        panel.background = element_rect(fill = "#ADEFD1FF", color = "#ADEFD1FF"),
        legend.position = "none", aspect.ratio = 1, 
        plot.margin = unit(rep(-10, 4), "pt")
    ) 
ggsave(filename = "results/08_curves_only.png", height = 3, width = 3, dpi = 1440)

