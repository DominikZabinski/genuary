# 500 lines.
# idea - create number of circles, each consisting number of lines. number of lines in each part will be an element of arithmetic sequence
# libraries ----
library(data.table)
library(ggplot2)

# functions ----
create_kinda_circle <- function(pivot = 0, x0 = 0, y0 = 0, radius = 2, nLines = 25)
{
    lines <- data.table(t = seq(from = .05 * pi, to = .95 * pi, length.out = nLines))
    lines[, x := radius * cos(t + pivot) + x0]
    lines[, y := radius * sin(t + pivot) + y0]
    lines[, xend := radius * cos(2 * pi + pivot - t) + x0]
    lines[, yend := radius * sin(2 * pi + pivot - t) + y0]
    
    return(lines)
}

# create data ----
totalNumberOfLines <- 500
nCircles <- 5
q <- -40
a1 <- (totalNumberOfLines - q * nCircles * (nCircles - 1) / 2) / nCircles


dataLine <- rbindlist(
    l = lapply(
        X = 1:nCircles,
        FUN = function(idC){
            thisKindaCircle <- create_kinda_circle(
                pivot = idC * (pi / (2 * nCircles)), 
                x0 = cos(idC) * idC, 
                y0 = sin(idC) * idC, 
                radius = (nCircles - idC + 1), 
                nLines = a1 + (idC - 1) * q
            )
            thisKindaCircle[, id := idC]
            thisKindaCircle[, radius := (nCircles - idC + 1)]
            return(thisKindaCircle)
        }
    )
)

# check number of lines per each id
dataLine[,.(.N), by = .(id)]

# plot and save it
ggplot() + 
    geom_segment(
        data = dataLine,
        aes(
            x = x, xend = xend, y = y, yend = yend,
            color = radius
        ),
        size = .35
        ) +
    theme_void() +
    scale_color_gradient(high = "#FFAAAA",  low= "#550000") +
    theme(
        aspect.ratio = 1,
        legend.position = "none",
        panel.background = element_rect(fill = "#669999", color = "#669999")
    )

ggsave(filename = "results/24_500_lines.png", height = 3, width = 3, dpi = 1440)


