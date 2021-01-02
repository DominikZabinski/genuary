# // TRIPLE NESTED LOOP
# libraries ----
library(data.table)
library(ggplot2)

# generate data ----
# the main idea is to use random walk, which easily gives double nested loop:
# first loop: simulation and determine number of steps
# second loop: loop through steps
# for the third loop (which is gonna be actually a root loop) I'm gonna use different ranges of angles and add an angle/direction to the step-taking-making phase

# to replicate results
set.seed(20210101)

# number of simulations for each starting conditions (first loop)
n <- 50
plotDataFinal <- rbindlist(
    l = lapply(
        X = seq(from = pi/2, to = -pi/2, by = -pi/24),  # root loop - range of angles
        FUN = function(st){
            plotData <- rbindlist(
                l = lapply(
                    X = 1:n,
                    FUN = function(n, startingAngle = st){
                        # for each simulation
                        steps <- floor(runif(n = 1, min = 100, max = 1000))
                        xAxis <- c(0)
                        yAxis <- c(0)
                        lastPoint <- c(0, 0)
                        for (j in 1:steps) # second loop - steps
                        {
                            # for each step
                            # how long is the step
                            stepsLength <- runif(n = 1, min = 2, max = 5)
                            # what is the angle
                            angle <- runif(n = 1, min = startingAngle, max = startingAngle + pi)
                            # update last point coordinates
                            lastPoint <- lastPoint + c(cos(angle) * stepsLength, sin(angle) * stepsLength)
                            xAxis <- c(xAxis, lastPoint[1])
                            yAxis <- c(yAxis, lastPoint[2])
                        }
                        data.table(id = n, step = 0:steps, x = xAxis, y = yAxis)
                    }
                )
            )
            plotData[, angle := st]
            return(plotData)
        }
    )
)

# alpha based on proximity to 0,0
plotDataFinal[, lightFactor := sqrt(x ^ 2 + y ^ 2)]
plotDataFinal[, lightFactor := lightFactor / max(lightFactor), by = .(angle)]

# plot the plot ...
ggplot(data = plotDataFinal, mapping = aes(x = x, y = y, group = id, color = lightFactor)) + 
    geom_line(size = .01) +
    facet_wrap(~angle, scales = "free") +
    scale_color_gradient(low = "#191919", high = "#ffffff") +
    theme_void() + 
    theme(
        panel.background = element_rect(fill = "black"), 
        legend.position = "none", 
        strip.text = element_blank(), panel.spacing = margin(0, 0, 0, 0)
    )

# ... and save it
dir.create(path = "results/", showWarnings = F)
ggsave(filename = "results/01_triple_nested_loop.png", width = 4, height = 4, dpi = 1440)
