# inspirations: https://twitter.com/conjuntovacio__/status/1364423232984477699/photo/1 and 'The One Where the Stripper Cries'
# libraries ----
library(data.table)
library(ggplot2)

# creating dataset ----
seed <- 6991
set.seed(seed = seed)
modSd <- .1
dataAll <- rbindlist(
    l = lapply(
        X = seq(0.1, 100, by = .1),
        FUN = function(t)
        {
            leftEnd <- rnorm(n = 1, mean = 2 + sin(t), sd = modSd * sqrt(t^3))
            rightEnd <- rnorm(n = 1, mean = 50 + sin(t), sd = modSd * t)
            data.table(
                i = t, x = sort(c(leftEnd, rightEnd)), 
                y = t, whichEnd = c("start", "end"))
        }
    )
)

# color variable
dataAll[, toColor := cos(i/4)]

# spline the points
splineDens <- 50
pointsToSplineIndex <- seq(from = 1, by = splineDens, to = nrow(dataAll) / 2)
splinedStart <- spline(x = dataAll[whichEnd == "start"]$y[pointsToSplineIndex], y = dataAll[whichEnd == "start"]$x[pointsToSplineIndex], n = splineDens * length(pointsToSplineIndex))
splinedEnd <- spline(x = dataAll[whichEnd == "end"]$y[pointsToSplineIndex], y = dataAll[whichEnd == "end"]$x[pointsToSplineIndex], n = splineDens * length(pointsToSplineIndex))
dataAll[, xSplined := x]
dataAll[whichEnd == "start", xSplined := splinedStart$y]
dataAll[whichEnd == "end", xSplined := splinedEnd$y]

# actual plot
ggplot() + 
    geom_path(data = dataAll, aes(x = xSplined, y = y, group = i, color = toColor), size = 1.5, lineend = "round") +
    coord_polar() +
    scale_color_gradientn(colours = rev(c("#9F8469", "#B39B84", "#CAB69D", "#EAD9BD", "#F9EED9", "#8D7252"))) +
    theme_void() + 
    theme(
        aspect.ratio = 1,
        legend.position = "none",
        panel.background = element_rect(color = NA, fill = "#e8ddc6")
    )

ggsave(filename = "results/mocca.png", height = 3, width = 3, dpi = 1440)