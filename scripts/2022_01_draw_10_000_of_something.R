# libraries ----
library(data.table)
library(ggplot2)

# functions ----
#' Title
#'
#' @param pivot 
#' @param x0 
#' @param y0 
#' @param radius 
#' @param nLines 
#'
#' @return
#' @export
#'
#' @examples
create_kinda_circle <- function(pivot = 0, x0 = 0, y0 = 0, radius = 2, nLines = 25)
{
    lines <- data.table(t = seq(from = .05 * pi, to = .95 * pi, length.out = nLines))
    lines[, x := radius * cos(t + pivot) + x0]
    lines[, y := radius * sin(t + pivot) + y0]
    lines[, xend := radius * cos(2 * pi + pivot - t) + x0]
    lines[, yend := radius * sin(2 * pi + pivot - t) + y0]
    
    return(lines)
}

determine_no_circle <- function(p, n)
{
    cc <- c()
    while (sum(cc) != n)
    {
        jj <- sample(p, 1)
        cc <- c(cc, jj)
        if (n - sum(cc) < min(p))
        {
            b <- n - sum(cc)
            cc[which(max(p) - cc > 0)[1:b]] <- cc[which(max(p) - cc > 0)[1:b]] + 1  
        }
    }
    return(cc)
}

set.seed(seed = 1234)

totalNumberOfLines <- 10000
possibleNoLines <- c(30:110)
circles <- data.table(n = determine_no_circle(p = possibleNoLines, totalNumberOfLines))
circles[, size := n / 50]

nCircles <- nrow(circles)
hist(circles$n)

positions <- gg(n = nCircles)
ggplot(data = positions) + 
    geom_point(aes(x = x, y= y)) + 
    theme_void()

dataLine <- rbindlist(
    l = lapply(
        X = 1:nCircles,
        FUN = function(idC){
            thisKindaCircle <- create_kinda_circle(
                pivot = idC * (pi / (2 * nCircles)), 
                x0 = cos(idC) * idC,
                # x0 = positions$x[idC],
                y0 = sin(idC) * idC,
                # y0 = positions$y[idC],
                radius = (nCircles - idC + 1) * runif(1, .1, .3),
                # radius = sqrt((nCircles - idC + 1)) * runif(1, .1, .3),
                # nLines = a1 + (idC - 1) * q
                nLines = circles$n[idC]
            )
            thisKindaCircle[, id := idC]
            # thisKindaCircle[, radius := (nCircles - idC + 1)]
            thisKindaCircle[, radius := .1]
            return(thisKindaCircle)
        }
    )
)

circles[, color := sample(x = c("#E1BC4F", "#343564", "#39648D"), size = nrow(circles), replace = T)]
circles[, op := sample(x = c("FF", "E6", "CC", "B3"), size = nrow(circles), replace = T)]
circles[, full := paste0(color, op)]
# plot and save it
ggplot() +
    lapply(
        X = 1:nrow(circles),
        FUN = function(i)
        {
            geom_segment(
                data = dataLine[id == i],
                aes(
                    x = x, xend = xend, y = y, yend = yend
                ), 
                # size = circles$size[i], 
                size = .35,
                color = circles$full[i]
            )      
        }
    ) +
    theme_void() +
    theme(
        aspect.ratio = 1,
        legend.position = "none",
        panel.background = element_rect(fill = "#ADC1D1", color = "#ADC1D1")
    )

ggsave(filename = "results/2022_01.png", height = 4, width = 4, dpi = 1440)


gg <- function(n)
{
    f1=jitter(sample(c(2,3),1));f2=jitter(sample(c(2,3),1));f3=jitter(sample(c(2,3),1));f4=jitter(sample(c(2,3),1))
    d1=runif(1,0,1e-02);d2=runif(1,0,1e-02);d3=runif(1,0,1e-02);d4=runif(1,0,1e-02)
    p1=runif(1,0,pi);p2=runif(1,0,pi);p3=runif(1,0,pi);p4=runif(1,0,pi)
    xt = function(t) exp(-d1*t)*sin(t*f1+p1)+exp(-d2*t)*sin(t*f2+p2)
    yt = function(t) exp(-d3*t)*sin(t*f3+p3)+exp(-d4*t)*sin(t*f4+p4)
    t=seq(1, 100, length.out = n)
    data.table(x = xt(t), y = yt(t))
}

