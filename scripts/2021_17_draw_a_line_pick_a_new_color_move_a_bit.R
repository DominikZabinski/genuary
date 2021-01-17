# Draw a line, pick a new color, move a bit.
# I'll use curve_gen from 'Curves only' prompt
# libraries ----
library(data.table)
library(ggplot2)
# functions ----
source("scripts/__helpful_functions.R")
# generate parameters for the curve
set.seed(seed = 38285)
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
    # this is 'move a bit' part
    startingPoint <- startingPoint + c(runif(1, -5, 5), runif(1, -5, 5))
    startingAngle <- startingAngle + thisParams$numberOfSteps * thisParams$angleChange
    thisCurve[, id := i]
    wholeCurve <- rbind(wholeCurve, thisCurve)
}

# that is 'pick new color' part
# color for each line will be based on 3 parameters. value of each parameter will determin red, green or blue part of color
parametersFrame <- rbindlist(
    l = lapply(
        X = 1:numberBreaks, 
        FUN = function(x) data.table(t(as.data.table(unlist(wholeCurveParam[[x]]))))
    )
)
setnames(parametersFrame, names(parametersFrame), c("angleChange", "numberOfSteps", "stepLength"))
parametersFrame[, id := 1:numberBreaks]
parametersFrame[id %%2 != 0, angleChange := -angleChange]

# normalize values
parametersFrame[, angNorm := (angleChange - pi/128) / (pi/20 - pi/128)]
parametersFrame[, numberNorm := (numberOfSteps - 20) / (40 - 20)]
parametersFrame[, stepNorm := (stepLength -  1) / (2 - 1)]
parametersFrame[, res := rgb(angNorm, numberNorm, stepNorm)]

lineColors <- parametersFrame$res
names(lineColors) <- parametersFrame$id

# some extra effort
# create background rectangles based on color of points. filling of each rectangle will be based on color of the points that will be in that rectangle (opposite color to weighted mean of colors)
rr <- seq(from = min(wholeCurve$x) - 1, to = max(wholeCurve$x) + 1, length.out = 15)
dataRect <- rbindlist(
    l = lapply(
        X = 1:(length(rr) - 1),
        FUN = function(i){
            x1 <- rr[i]
            x2 <- rr[i + 1]
            tt <- merge(wholeCurve[between(x, x1, x2),.(.N), by = . (id)], parametersFrame[,.(id, angNorm, numberNorm, stepNorm)], by = 'id')
            panelB <- rgb(1 - weighted.mean(x = tt$angNorm, w = tt$N), 1 - weighted.mean(x = tt$numberNorm, w = tt$N), 1- weighted.mean(x = tt$stepNorm, w = tt$N))
            data.table(xmin = x1, xmax = x2, id2 = i, b = panelB)
        }
    )
)

dataRect[, ymin := min(wholeCurve$y) - 1]
dataRect[, ymax := max(wholeCurve$y) + 1]

rectBackColors <- dataRect$b
names(rectBackColors) <- dataRect$id

# actual plot
ggplot() + 
    geom_rect(data = dataRect, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = as.factor(id2)), alpha = .6) + 
    scale_fill_manual(values = rectBackColors) + 
    geom_path(data = wholeCurve, aes(x = x, y = y, color = as.factor(id), group = id), size = 0.5, lineend = "round") + 
    theme_void() + 
    scale_color_manual(values = lineColors) +
    theme(
        legend.position = "none", aspect.ratio = 1, 
        plot.margin = unit(rep(-10, 4), "pt")
    )

ggsave(filename = "results/17_draw_pick_move.png", height = 3, width = 3, dpi = 1440)
