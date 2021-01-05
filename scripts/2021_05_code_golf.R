library(data.table)
library(ggplot2)

n <- 100
ggplot(data = data.table(x=(5*n):1,y=1:n), aes(x = x, y = y, alpha = x/(5*n)))+ 
    geom_path() + theme_void() + coord_polar(theta = "y") + theme(legend.position = "none")
ggsave(filename = "results/04_code_golf.png", height = 3, width = 3, dpi = 720)
