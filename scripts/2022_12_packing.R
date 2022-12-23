library(data.table)
library(ggplot2)

create_circle <- function(x0, y0, r, id)
{
    t <- seq(from = 0, to = 2 * pi, length.out = 100)
    data.table(
        x = r * cos(t) + x0,
        y = r * sin(t) + y0,
        id = id
    )
}

odl_punkt <- function(x1, y1, x2, y2)
{
    sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}

percProg <- .45
existingCircles <- data.table(x = numeric(), y = numeric(), r = numeric(), id = character())
baseCircle <- c(0, 0, 1)
currentCoverage <- sum(existingCircles$r ^ 2) / (baseCircle[3] ^ 2)
minRadius <- .01
maxRadius <- .025
padding <- .005
progrr <- 1
while (currentCoverage < percProg & progrr > .0001)
{
    # losuj punkt z kola
    parLos <- c(sqrt(runif(1)), runif(1, 0, 2 * pi))
    punktLos <- parLos[1] * c(cos(parLos[2]), sin(parLos[2]))
    
    if (nrow(existingCircles) > 0)
    {
        # czy jest wewnatrz istniejacego kola
        existingCircles[, odl := odl_punkt(x1 = punktLos[1], y1 = punktLos[2], x2 = x, y2 = y)]
        existingCircles[, rozn := odl - r - padding]
        if (nrow(existingCircles[rozn <= 0]) == 0)
        {
            # odleglosc od krawedzi
            odKrawedzi <- 1 - odl_punkt(x1 = 0, y1 = 0, x2 = punktLos[1], y2 = punktLos[2])
            rLosMax <- min(existingCircles[rozn > 0]$rozn, odKrawedzi)
            if (rLosMax > minRadius)
            {
                rLos <- runif(1, minRadius, pmin(maxRadius, rLosMax))
                existingCircles[, c("rozn", "odl") := NULL]
                existingCircles <- rbind(existingCircles, data.table(x = punktLos[1], y = punktLos[2], r = rLos, id = nrow(existingCircles) + 1))
                ccf <- sum(existingCircles$r ^ 2) / (baseCircle[3] ^ 2)
                progrr <- (ccf - currentCoverage) / currentCoverage
                currentCoverage <- ccf
                message(sprintf("%s %s", Sys.time(), currentCoverage))
            }
        }
    } else
    {
        # losuj promien tak zeby nie wyjsc poza kolo
        rLosMax <- 1 - odl_punkt(x1 = 0, y1 = 0, x2 = punktLos[1], y2 = punktLos[2])
        if (rLosMax > minRadius)
        {
            rLos <- runif(1, minRadius, pmin(maxRadius, rLosMax))
            existingCircles <- rbind(existingCircles, data.table(x = punktLos[1], y = punktLos[2], r = rLos, id = 1))
            currentCoverage <- sum(existingCircles$r ^ 2) / (baseCircle[3] ^ 2)
            message(sprintf("%s %s", Sys.time(), currentCoverage))
        }
    }
}

colorr <- c("#284253", "#F4A720")
names(colorr) <- c("none", "be")

dataCircles <- rbindlist(
    l = lapply(
        X = 1:nrow(existingCircles),
        FUN = function(i)
        {
            tmpD <- existingCircles[i]
            create_circle(x0 = tmpD$x, y0 = tmpD$y, r = tmpD$r, id = tmpD$id)
        }
    )
)
dataCircles <- merge(dataCircles, existingCircles[,.(id, x0 = x, y0 = y, r)], by = "id")
dataCircles[, group := "none"]
dataCircles[odl_punkt(x1 = x0, y1 = y0, x2 = .25, y2 = .4) < .1, group := "be"]
dataCircles[odl_punkt(x1 = x0, y1 = y0, x2 = -.15, y2 = -.4) < .1, group := "be"]
dataCircles[odl_punkt(x1 = x0, y1 = y0, x2 = .3, y2 = 0) < .1, group := "be"]
dataCircles[odl_punkt(x1 = x0, y1 = y0, x2 = -.2, y2 = .3) < .1, group := "be"]

pp <- ggplot() + 
    geom_polygon(mapping = aes(x = x, y = y, group = id), data = create_circle(x0 = 0, y0 = 0, r = 1, id = 0), color = "#E0542E", fill = "#E0542E") + 
    geom_polygon(mapping = aes(x = x, y = y, group = id, color = group, fill = group), data = dataCircles) +
    scale_fill_manual(values = colorr) +
    scale_color_manual(values = colorr) +
    theme_void() + 
    theme(
        aspect.ratio = 1,
        legend.position = "none",
        plot.background = element_rect(fill = "#4D7186")
    )
ggsave(plot = pp, filename = "D:/aa.png", width = 5, height = 5, dpi = 720)
file.show("D:/aa.png")
