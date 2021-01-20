# No loops
# libraries ----
library(data.table)
library(ggplot2)

# create data ----
tVec <- c(1:440)
plotData <- data.table(
    t = tVec, 
    x = tVec * cos(.1 * tVec) / 1e3, 
    y = tVec * sin(.07 * tVec), 
    alpha = sqrt((max(tVec) - tVec + 1) / max(tVec)),
    size = tVec ^ 2
)

plotData[, color := sqrt(x^2+y^2)]

# plot it and save it ----
ggplot(
    data = plotData, 
    mapping = aes(
        x = x, 
        y = y, 
        col = color, fill = color,
        alpha = alpha,
        size = size
    )
) + 
    geom_point(shape = 21) +
    scale_color_viridis_c(option = "B") +
    scale_fill_viridis_c(option = "B") +
    scale_size_continuous(range = c(.1, 3)) +
    theme_void() +
    theme(
        panel.background = element_rect(fill = "#AA3939", color = "#AA3939"),
        legend.position = "none",
        aspect.ratio = 1, 
        plot.margin = unit(rep(0, 4), "pt")
    )
ggsave(filename = "results/20_no_loops.png", height = 3, width = 3, dpi = 1440)
