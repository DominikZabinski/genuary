# Any shape, none can touch
# libraries ----
library(data.table)
library(ggplot2)

# helpful functions ----
source("scripts/__helpful_functions.R")
# create data ----
# main idea - fill a circle of radius = 1 and center in 0,0 with smaller circles so that none of that circles overlaps. positions of smaller circles are random and chosen in not-so-inteligent way

set.seed(seed = 1567)
howMany <- 750
count <- 0
dataCircles <- data.table()

# it takes about a minute to do it by brute force
while(count < howMany)
{
    # center of the circle
    ss <- c(runif(1, -1, 1), runif(1, -1, 1))
    if (nrow(dataCircles) == 0)
    {
        r <- runif(1, 0.01, .1)
        dataCircles <- rbind(dataCircles, data.table(x = ss[1], y= ss[2], id = count, radius = r))
        count <- count + 1
    } else {
        # check if this point is inside existing circle
        dataCircles$d <- unlist(lapply(X = 1:nrow(dataCircles), FUN = function(i) dist_between_points(p1 = c(dataCircles$x[i], dataCircles$y[i]), p2 = ss)))
        
        # if not 
        if (all(dataCircles$d > dataCircles$radius))
        {
            # check if im in a imaginary circle 0,0, radius 1
            if (ss[1] ^ 2 + ss[2] ^ 2 < 1)
            {
                # establish upper boundary for the radius of this circle
                maxrr <- min(.1, min(dataCircles$d - dataCircles$radius)) -1e-4
                
                # correction for outer circle boundary
                thisR <- sqrt(ss[1]^2+ss[2]^2)
                maxrr <- min(maxrr, 1 - thisR)
                
                if (maxrr > .01)
                {
                    r <- runif(1, 0.01, maxrr)
                    dataCircles[, d := NULL]
                    dataCircles <- rbind(dataCircles, data.table(x = ss[1], y = ss[2], id = count, radius = r))
                    count <- count + 1
                }
                
            }
        }
    }
}

# create points for each circle
dataPlot <- rbindlist(
    lapply(
        X = dataCircles$id, 
        FUN = function(w){
            create_circle(center = c(dataCircles[id==w]$x, dataCircles[id==w]$y), radius = dataCircles[id==w]$radius, id = w)
        }
    )
)

# filling is based on the position on the x-axis
dataPlot[, toFill := max(x), by = .(id)]

# plot itself ----
ggplot() +
    geom_polygon(data = dataPlot, aes(x = x, y=y, group = id, color = toFill), fill = "#000000", size = .15) +
    theme_void() + 
    theme(
        aspect.ratio = 1,
        panel.background = element_rect(fill = "#000000", color = "#000000"),
        legend.position = "none"
    )
ggsave(filename = "results/29_any_shape_none_can_touch.png", height = 3, width = 3, dpi = 1440)
