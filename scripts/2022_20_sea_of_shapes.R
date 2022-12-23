# cs of shapes
# libraries ----
library(data.table)
library(ggplot2)
library(bezier)
# pallette - https://sarahrenaeclark.com/wp-content/uploads/2019/01/Color-Palette-278-living-coral.jpg
create_poly <- function(x0, y0, r, no, id)
{
    t <- seq(from = 0, to = 2 * pi, length.out = no + 1)[1:no]
    data.table(
        x = r * cos(t) + x0,
        y = r * sin(t) + y0,
        id = id
    )
}

mm <- function(x)
{
    max(x) - min(x)
}

p <- matrix(data = c(0,0, -1,-1, -2,0, -3,1, -2,2, -1,3, 0,2), nrow=7, ncol=2, byrow=TRUE)
p2 <- matrix(data = c(-.25,.5, -.75,0, -1.25,.5, -2,1, -1.25,1.5, -.75,2, -.25,1.5), nrow=7, ncol=2, byrow=TRUE)
possSh <- c(100, 3:7)


make_c <- function(bC, laps = 10, perLap = 10, pref = "")
{
    rbindlist(
        l = lapply(
            X = 1:laps, 
            FUN = function(i)
            {
                g <- perLap * i
                gg <- sample(x = 1:nrow(bC), size = g, replace = F)
                err <- 1/i
                oo <- rbindlist(
                    l = lapply(
                        X = 1:g, 
                        FUN = function(j)
                        {
                            create_poly(
                                x0 = bC[gg[j]]$V1+runif(1, -err, err), 
                                y0 = bC[gg[j]]$V2+runif(1, -err, err), 
                                r = 1 / (1 + sqrt(i^3)), 
                                no = sample(x = possSh, size = 1), id = paste0(pref, g, "_", j)
                            )
                        }
                    )
                )
                oo[, batch := i]
                return(oo)
            }
        )
    )
}

s <- sample(x = 1:123456, size = 1)
set.seed(seed = s)
cShape <- data.table(bezier(t = seq(0, 1, length = 100), p = p))
dataC <- make_c(bC = cShape)
dataC2 <- make_c(bC = data.table(bezier(t = seq(0, 1, length = 100), p = p2)), perLap = 8, pref = "o_")

aV <- 1/(log(1:10) + 1)
names(aV) <- 10:1
sV <- log(1:10 + 1) / 5
names(sV) <- 1:10
ppa <- mm(cShape$V2) / mm(cShape$V1)

allCs <- rbind(dataC, dataC2)[order(batch)]

gg2 <- list(
    x = range(cShape$V1) * .9,
    y = range(cShape$V2) * .9
)

backgroundGrid <- data.table(
    expand.grid(
        x = seq(from = gg2$x[1]/.9, to = gg2$x[2]/.9, by = .025), 
        y = seq(from = gg2$y[1]/.9, to = gg2$y[2]/.9, by = .025)
    )
)
kkB <- colorRampPalette(colors = c('#35503e', '#3c7899', '#324b35', '#338fb8'))(8)
backgroundGrid[, kk := sample(x = kkB, replace = T, size = nrow(backgroundGrid))]


pp <- ggplot() + 
    lapply(
        X = 1:nrow(backgroundGrid),
        FUN = function(ii)
        {
            geom_tile(mapping = aes(x = x, y = y), color = backgroundGrid[ii]$kk, fill = backgroundGrid[ii]$kk, data = backgroundGrid[ii])
        }
    ) +
    lapply(
        X = unique(allCs[,.(batch, id)])[order(batch)]$id,
        FUN = function(ii)
        {
            geom_polygon(
                data = allCs[id == ii],
                mapping = aes(x = x, y = y, group = id, alpha = factor(batch), fill = batch, size = factor(batch)),
                color = 'white'
            )
        }
    ) +
    scale_alpha_manual(values = aV) +
    scale_size_manual(values = sV) +
    scale_fill_gradient(low = "#ff7062", high = "#fc4f00") +
    theme_void() + 
    theme(
        aspect.ratio = ppa,
        legend.position = "none",
        panel.background = element_rect(fill = NA, color = NA)
    ) + 
    coord_fixed(xlim = gg2$x, ylim = gg2$y)
w <- 4
ggsave(plot = pp, filename = "D:/dsas.png", width = w, height = w * ppa, dpi = 300)
file.show("D:/dsas.png")
message(s)
