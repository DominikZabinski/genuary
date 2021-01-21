# function f(x) { 
#     DRAW(x); 
#     f(1 * x / 4); 
#     f(2 * x / 4); 
#     f(3 * x / 4); 
# }

# My idea - lets assume that everytime we get set of specific coordinates (x, y) we would draw it with some color (eg. #FFFFF0). So if we get same point another time and draw it it gets darker and darker and darker ;)

# libraries ----
library(ggplot2)
library(data.table)

# functions ----
#' Title
#'
#' @param data 
#' @param portion 
#' @param lvl 
#' @param chaos 
#'
#' @return
#' @export
#'
#' @examples
get_points <- function(data, portion = 1, lvl = 0, chaos = TRUE)
{
    if (chaos == TRUE)
    {
        data$idNew <- sample(1:nrow(data))
    } else
    {
        data$idNew <- 1:nrow(data)
    }
    
    newData <- data[idNew <= nrow(data) * portion]
    if (lvl == 4)
    {
        return(newData)
    } else
    {
        rbindlist(
            l = list(
                newData, 
                get_points(newData, portion = 1/4, lvl = lvl + 1, chaos = chaos),
                get_points(newData, portion = 1/2, lvl = lvl + 1, chaos = chaos),
                get_points(newData, portion = 3/4, lvl = lvl + 1, chaos = chaos)
            )
        )
    }
}

# create data ----
nPoints <- 1:1000
simplePoints <- data.table(expand.grid(x = nPoints, y = nPoints))

orderPoints <- get_points(data = simplePoints, portion = 1, lvl = 0, chaos = FALSE)
orderPoints <- orderPoints[,.(v = .N), by = .(x, y)]

chaoticPoints <- get_points(data = simplePoints, portion = 1, lvl = 0, chaos = TRUE)
chaoticPoints <- chaoticPoints[,.(v = .N), by = .(x, y)]

ggplot(data = orderPoints, aes(x = x, y = y, fill = v)) + 
    geom_tile() + 
    scale_fill_gradient(low = "#FFFFF0", high = "#000000") +
    theme_void() +
    theme(
        legend.position = "none",
        aspect.ratio = 1, 
        plot.margin = unit(rep(0, 4), "pt")
    )
ggsave(filename = "results/21_order.png", height = 3, width = 3, dpi = 1440)


ggplot(data = chaoticPoints, aes(x = x, y = y, fill = v)) + 
    geom_tile() + 
    scale_fill_gradient(low = "#FFFFF0", high = "#000000") +
    theme_void() +
    theme(
        legend.position = "none",
        aspect.ratio = 1, 
        plot.margin = unit(rep(0, 4), "pt")
    )
ggsave(filename = "results/21_chaos.png", height = 3, width = 3, dpi = 1440)
