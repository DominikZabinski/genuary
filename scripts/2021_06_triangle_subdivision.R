# triangle subdivision
# libraries ----
library(data.table)
library(ggplot2)

# functions ----
#' Find random point between two points
#'
#' @param p1 (x,y) coordinates of the first point
#' @param p2 (x,y) coordinates of the second point
#' @param lower lower limit
#' @param upper upper limit
#'
#' @return
#' @export
#'
#' @examples
random_point_between_points <- function(p1 = c(0, 0), p2 = c(1, 0), lower = .8, upper = .9)
{
    # i wanna hit point that is between lower% and upper% of total distance between given points
    boundsX <- c(min(p1[1], p2[1]), max(p1[1], p2[1]))
    newBoundsX <- c(boundsX[1] + lower * (boundsX[2]-boundsX[1]), boundsX[1] + upper * (boundsX[2]-boundsX[1]))
    boundsY <- c(min(p1[2], p2[2]), max(p1[2], p2[2]))
    newBoundsY <- c(boundsY[1] + lower * (boundsY[2]-boundsY[1]), boundsY[1] + upper * (boundsY[2]-boundsY[1]))
    
    if (p1[1] == p2[1])
    {
        randomX <- p1[1]
        notSoRandomY <- runif(n = 1, min = newBoundsY[1], max = newBoundsY[2])
    } else 
    {
        a <- (p2[2] - p1[2]) / (p2[1] - p1[1])
        b <- p1[2] - a * p1[1]    
        
        randomX <- runif(n = 1, min = newBoundsX[1], max = newBoundsX[2])
        notSoRandomY <- a * randomX + b
    }
    c(randomX, notSoRandomY)
}

#' Calculate distance between two points
#'
#' @param p1 (x,y) coordinates of the first point
#' @param p2 (x,y) coordinates of the second point
#'
#' @return
#' @export
#'
#' @examples
dist_between_points <- function(p1, p2)
{
    sqrt((p1[1]-p2[1])^2 + (p1[2]-p1[2])^2)
}

#' Simple function that basically gets specific columns from given row
#'
#' @param triangleData data.table with x and y columns
#' @param whichPoint which row 
#'
#' @return
#' @export
#'
#' @examples
get_point_from_triangle <- function(triangleData, whichPoint = 3)
{
    c(triangleData$x[whichPoint], triangleData$y[whichPoint])
}

# create data ----
# base triangle
triangFinData <- data.table(x = c(0, 1, 0), y = c(0, 0, 1), id = 0)

# copy base triangle as last triangle and establish number of iterations
lastTriangle <- copy(triangFinData)
n <- 32
for (i in 1:n)
{
    # new triangle would be: 
    # random point between second and third point from last triangle ... 
    
    # get second point of the last triangle
    p12 <- get_point_from_triangle(lastTriangle, 2)
    # get last point of previous triangle
    p13 <- get_point_from_triangle(lastTriangle, 3)
    # get random point from line connecting second point and last point ... 
    np11 <- random_point_between_points(p1 = p12, p2 = p13)
    
    # ... and its mirror image
    np11mirror <- p13 - (np11 - p12)
    
    # the one i want its closer to p12
    dist1112 <- dist_between_points(p12, np11)
    dist11mirror12 <- dist_between_points(p12, np11mirror)
    
    if (dist11mirror12 < dist1112) np11 <- np11mirror
    
    # ... and third and first point from the last triangle
    # get first point of the last triangle
    p11 <- get_point_from_triangle(lastTriangle, 1)
    
    # create new triangle
    thisTriangle <- data.table(x = c(np11[1], p13[1], p11[1]), y = c(np11[2], p13[2], p11[2]), id = i)
    
    # add data to existing set
    triangFinData <- rbind(triangFinData, thisTriangle)
    lastTriangle <- copy(thisTriangle)
}

# lets see what we've accomplished
ggplot(data = triangFinData, aes(x = x, y = y, group = id, fill = id)) + 
    geom_polygon() +
    scale_fill_viridis_c(option = "B", direction = -1) + theme_void()

# the same way create opposite triangle
oppositeTriangles <- data.table(x = c(1, 1, 0), y = c(0, 1, 1), id = 0)
lastOppTriangle <- copy(oppositeTriangles)
for (i in 1:n)
{
    # new triangle would be: 
    # random point between second and third point from last triangle ... 
    
    # get second point of the last triangle
    p12 <- get_point_from_triangle(lastOppTriangle, 2)
    # get last point of previous triangle
    p13 <- get_point_from_triangle(lastOppTriangle, 3)
    # get random point from line connecting second point and last point ... 
    np11 <- random_point_between_points(p1 = p12, p2 = p13)
    
    # ... and its mirror image
    np11mirror <- p13 - (np11 - p12)
    
    # the one i want its closer to p12
    dist1112 <- dist_between_points(p12, np11)
    dist11mirror12 <- dist_between_points(p12, np11mirror)
    
    if (dist11mirror12 < dist1112) np11 <- np11mirror
    
    # ... and third and first point from the last triangle
    # get first point of the last triangle
    p11 <- get_point_from_triangle(lastOppTriangle, 1)
    
    # create new triangle
    thisTriangle <- data.table(x = c(np11[1], p13[1], p11[1]), y = c(np11[2], p13[2], p11[2]), id = i)
    
    # add data to existing set
    oppositeTriangles <- rbind(oppositeTriangles, thisTriangle)
    lastOppTriangle <- copy(thisTriangle)
}

# check out the results
ggplot(data = oppositeTriangles, aes(x = x, y = y, group = id, fill = id)) + 
    geom_polygon() +
    scale_fill_viridis_c(option = "B", direction = -1) + theme_void()

# combine two set in one object and check out the result
twoRealms <- rbind(
    triangFinData[,.(x, y, id)],
    oppositeTriangles[,.(x, y, id = id + n + 1)]
)

ggplot(data = twoRealms, aes(x = x, y = y, group = id, fill = id)) + 
    geom_polygon() +
    scale_fill_viridis_c(option = "B", direction = -1) + theme_void()

# draw up a pallette 
myColors <- colorRampPalette(colors = c("#b8cdc8", "#9f6262"))(length(unique(twoRealms$id)))
names(myColors) <- c(c(n:0), c((n+1):(2*n+1)))

ggplot(data = twoRealms, aes(x = x, y = y, group = id, fill = as.factor(id))) + 
    geom_polygon() +
    scale_fill_manual(values = myColors) +
    theme_void() + 
    theme(legend.position = "none", aspect.ratio = 1)

# save result ----
ggsave(filename = "results/06_triangle_subdivision.png", height = 3, width = 3, dpi = 720)
