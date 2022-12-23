# libraries ----
library(data.table)
library(ggplot2)

# functions ----
count_as <- function(data)
{
    (max(data$y) - min(data$y)) / (max(data$x) - min(data$x))
}

iso_me <- function(x, y, z, rp = c(0, 0))
{
    c(
        rp[1] + x + y/2,
        rp[2] + z + y/2
    )
}

iso_front <- function(lbc, side = 1, id)
{
    front <- rbindlist(
        l = list(
            as.data.table(t(iso_me(x = lbc[1], y = lbc[2], z = lbc[3]))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2], z = lbc[3]))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2], z = lbc[3] + side))),
            as.data.table(t(iso_me(x = lbc[1], y = lbc[2], z = lbc[3] + side)))
        )
    )
    setnames(front, names(front), c("x", "y"))
    front[, id := paste0(id, "_f")]
    return(front[])
}

iso_side <- function(lbc, side = 1, id)
{
    side <- rbindlist(
        l = list(
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2], z = lbc[3]))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2] + side, z = lbc[3]))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2] + side, z = lbc[3] + side))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2], z = lbc[3] + side)))
        )
    )
    setnames(side, names(side), c("x", "y"))
    side[, id := paste0(id, "_s")]
    return(side[])
}

iso_top <- function(lbc, side = 1, id)
{
    top <- rbindlist(
        l = list(
            as.data.table(t(iso_me(x = lbc[1], y = lbc[2], z = lbc[3] + side))),
            as.data.table(t(iso_me(x = lbc[1], y = lbc[2] + side, z = lbc[3] + side))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2] + side, z = lbc[3] + side))),
            as.data.table(t(iso_me(x = lbc[1] + side, y = lbc[2], z = lbc[3] + side)))
        )
    )
    setnames(top, names(top), c("x", "y"))
    top[, id := paste0(id, "_t")]
    return(top[])
}

iso_cube <- function(lbc, side = 1, id)
{
    rbindlist(
        l = list(
            iso_front(lbc = lbc, id = id, side = side),
            iso_side(lbc = lbc, id = id, side = side),
            iso_top(lbc = lbc, id = id, side = side)
        )
    )
}

# data ----
n <- 20
pp <- c(0, 0)
wx <- .5
allCubes <- data.table(expand.grid(x = seq(from = 1, to = n, by = .5), y = seq(from = 1, to = n, by = .5)))
allCubes[, t := sqrt((pp[1]-x*wx) ^ 2 + (pp[2] - y) ^ 2)]
allCubes[, z := ceiling(10 * cos(t))]
allCubes[z == 0, z := sample(x = c(1, -1), size = nrow(allCubes[z == 0]), replace = T)]
range(allCubes$z)
allCubes[, id := 1:nrow(allCubes)]
dataT <- rbindlist(
    l = lapply(
        X = allCubes$id,
        FUN = function(ii)
        {
            aa <- allCubes[id == ii]
            iso_cube(lbc = c(aa$x, aa$y, aa$z), id = ii, side = .5)
        }
    )
)
dataT[, idC := substr(id, 1, regexpr(pattern = "_", text = id) - 1)]

# order cubes
allCubes <- allCubes[order(-y, z, x)]

# how to remove cubes/sides, that are not at all visible?
pp <- ggplot() +
    lapply(
        X = allCubes$id,
        FUN = function(ii)
        {
            dd <- dataT[idC == ii]
            dd[, z := allCubes[id == ii]$z]
            geom_polygon(data = dd, mapping = aes(x = x, y = y, group = id, fill = z), color = "white", size = .1)
        }
    ) +
    scale_fill_gradient(high = "#D3ECF9", low = "#232A5C") +
    theme_void() +
    theme(
        aspect.ratio = count_as(data = dataT),
        # aspect.ratio = 1 / count_as(data = dataT),
        plot.background = element_rect(fill = "#F1E9DA", color = "#F1E9DA"), 
        legend.position = "none"
    ) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

wi = 5
ggsave(filename = "results/2022_29.png", plot = pp, height = wi * (16 / 9) ^ (-1), width = wi, dpi = 300)
file.show("results/2022_29.png")
