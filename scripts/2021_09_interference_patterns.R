# Interference patterns
# libraries ----
library(data.table)
library(ggplot2)

# data and plot ----
# create data.table
nRows <- 600
nCols <- 600

dataInt <- data.table(expand.grid(x = 1:nRows, y = 1:nCols))

set.seed(seed = 717)

# randomize starting position of each wave source [startingPoint], wave length [waveNumber] and its amplitude [A]
# the angular frequency could be ommitted

dropPoints <- list(
    list(
        startingPoint = c(sample((nCols/2):nCols, 1), sample((nRows/2):nRows, 1)), 
        waveNumber = 2 * pi * runif(1), angularFreq = 2 * pi, A = runif(1, 0, 1)
    ),
    list(
        startingPoint = c(sample(1:(nCols/2), 1), sample(1:(nRows/2), 1)), 
        waveNumber = 2 * pi * runif(1, 1, 2), angularFreq = 2 * pi, A = runif(1, 0, 1)
    ),
    list(
        startingPoint = c(sample(1:(nCols/2), 1), sample((nRows/2):nRows, 1)), 
        waveNumber = 2 * pi * runif(1, 2, 4), angularFreq = 2 * pi, A = runif(1, 0, 1)
    ),
    list(
        startingPoint = c(sample((nCols/2):nCols, 1), sample(1:(nRows/2), 1)), 
        waveNumber = 2 * pi * runif(1, 4, 8), angularFreq = 2 * pi, A = runif(1, 0, 1)
    )
)

# for each wavesource
for (droplet in 1:length(dropPoints))
{
    thisDrop <- dropPoints[[droplet]]
    # calcuate distance from center of the wave for each point in data
    dataInt[, dist := sqrt((x - thisDrop$startingPoint[1])^2 + (y - thisDrop$startingPoint[2])^2)]    
    # calculate wave height in ths point
    dataInt[, amp := thisDrop$A * cos(thisDrop$waveNumber * dist - thisDrop$angularFreq)]
    setnames(dataInt, "amp", paste0("amp", droplet))
}

# final height of the point will be dependen on the sum of heights of each wave
dataInt$totAmp <- rowSums(dataInt[, which(names(dataInt) %like% "amp"), with = F])

# plot ...
ggplot(data = dataInt, aes(x = x, y = y, fill = totAmp)) + 
    geom_tile() + 
    theme_void() + 
    scale_fill_gradient(high = "#000000", low = "#FFFFFF") +
    theme(
        legend.position = "none", 
        aspect.ratio = 1,
        plot.margin = unit(rep(-10, 4), "pt")
    )
# ... and save
ggsave(filename = "results/09_interference_patterns.png", height = 3, width = 3, dpi = 1440)
