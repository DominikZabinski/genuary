# Replicate a natural concept (e.g. gravity, flocking, path following).
# libraries ----
library(data.table)
library(ggplot2)
library(ggvoronoi)

# functions ----
source("scripts/__helpful_functions.R")

create_sunflower <- function(scale = 1, center = c(0,0))
{
    goldenAngle <- pi * (3 - sqrt(5))
    ff <- data.table(n = 1:500)
    ff[, t := 2 * pi * n / (goldenAngle ^ 2)]
    ff[, x := scale * sqrt(n) * cos(t) + center[1]]
    ff[, y := scale * sqrt(n) * sin(t) + center[2]]
    ff[, ww := sqrt((x - center[1])^2 + (y-center[2])^2)]
    return(ff[])
}

sunflowerData <- create_sunflower()

ggplot() +
    geom_voronoi(data = sunflowerData, aes(x = x, y = y, fill = ww))+
    scale_fill_gradient(low = "#E3BB1C", high = "#5E4D0B") +
    theme_void() +
    theme(
        aspect.ratio = 1,
        legend.position = "none",
        plot.margin = unit(rep(-10, 4), "pt")
    )
ggsave(filename = "results/30_concept.png", height = 3, width = 3, dpi = 1440)
