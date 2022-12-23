library(data.table)
library(ggplot2)

add_alpha <- function(a)
{
    if (length(a) > 1) return(unlist(lapply(X = a, FUN = function(x) add_alpha(x))))
    a <- paste0(a, "%")
    switch(
        a,
        "100%" = 'FF', "99%" = 'FC', "98%" = 'FA', "97%" = 'F7', "96%" = 'F5', "95%" = 'F2',
        "94%" = 'F0', "93%" = 'ED', "92%" = 'EB', "91%" = 'E8', "90%" = 'E6', "89%" = 'E3', 
        "88%" = 'E0', "87%" = 'DE', "86%" = 'DB', "85%" = 'D9', "84%" = 'D6', "83%" = 'D4', 
        "82%" = 'D1', "81%" = 'CF', "80%" = 'CC', "79%" = 'C9', "78%" = 'C7', "77%" = 'C4', 
        "76%" = 'C2', "75%" = 'BF', "74%" = 'BD', "73%" = 'BA', "72%" = 'B8', "71%" = 'B5', 
        "70%" = 'B3', "69%" = 'B0', "68%" = 'AD', "67%" = 'AB', "66%" = 'A8', "65%" = 'A6', 
        "64%" = 'A3', "63%" = 'A1', "62%" = '9E', "61%" = '9C', "60%" = '99', "59%" = '96', 
        "58%" = '94', "57%" = '91', "56%" = '8F', "55%" = '8C', "54%" = '8A', "53%" = '87', 
        "52%" = '85', "51%" = '82', "50%" = '80', "49%" = '7D', "48%" = '7A', "47%" = '78', 
        "46%" = '75', "45%" = '73', "44%" = '70', "43%" = '6E', "42%" = '6B', "41%" = '69', 
        "40%" = '66', "39%" = '63', "38%" = '61', "37%" = '5E', "36%" = '5C', "35%" = '59', 
        "34%" = '57', "33%" = '54', "32%" = '52', "31%" = '4F', "30%" = '4D', "29%" = '4A', 
        "28%" = '47', "27%" = '45', "26%" = '42', "25%" = '40', "24%" = '3D', "23%" = '3B', 
        "22%" = '38', "21%" = '36', "20%" = '33', "19%" = '30', "18%" = '2E', "17%" = '2B', 
        "16%" = '29', "15%" = '26', "14%" = '24', "13%" = '21', "12%" = '1F', "11%" = '1C', 
        "10%" = '1A', "9%" = '17', "8%" = '14', "7%" = '12', "6%" = '0F', "5%" = '0D', "4%" = '0A', "3%" = '08', "2%" = '05', "1%" = '03', "0%" = '00'
    )
}


create_square <- function(x0, y0, a, id)
{
    data.table(
        x = c(x0, x0 + a, x0 + a, x0),
        y = c(y0, y0, y0 + a, y0 + a),
        id = id
    )
}

calc_dist <- function(xa, ya, xb, yb)
{
    sqrt((xa-xb)^2 + (ya-yb)^2)
}

create_destroyed_square <- function(xy, side, idS)
{
    tmpSquare <- create_square(x0 = xy[1], y0 = xy[2], a = side, id = idS)
    # pick a point in the middle
    point <- c(
        runif(n = 1, min = xy[1] + .15 * side, max =  xy[1] + .85 * side),
        runif(n = 1, min = xy[2] + .15 * side, max =  xy[2] + .85 * side)
    )
    
    xis <- c(tmpSquare$x, tmpSquare$x[1])
    yis <- c(tmpSquare$y, tmpSquare$y[1])
    # pick from 1 to 2 points from each side
    points <- rbindlist(
        l = lapply(
            X = 1:4,
            FUN = function(j)
            {
                i <- sample(1:2, size = 1)
                if (i == 0) return(NULL)
                xS <- xis[j];yS <- yis[j];xE <- xis[j+1];yE <- yis[j + 1]
                xS <- min(c(xS, xE));xE <- max(c(xS, xE));yS <- min(c(yS, yE));yE <- max(c(yS, yE))
                if (xS != xE)
                {
                    xE = xS + .95 * side; xS <- xS + .05 * side; 
                } 
                
                if (yS != yE)
                {
                    yE = yS + .95 * side; yS <- yS + .05 * side; 
                } 
                
                points <- data.table(
                    x = runif(n = i, min = xS, max = xE),
                    y = runif(n = i, min = yS, max = yE),
                    side = j
                )[order(x, y)]
                points$i <- 4 ^ (j - 1) + 1:i
                return(points)
            }
        )
    )
    
    points <- points[,.(i = mean(i), side = min(side)), by = .(x, y)]
    pointsForRect <- rbind(points, points[1][,.(x, y, i = max(points$i) + 1, side)])
    
    # create polygons (and move them a bit)
    polygonsFrom <- rbindlist(
        l = lapply(
            X = 1:nrow(points),
            FUN = function(j)
            {
                thisPoints <- pointsForRect[c(j, j + 1)][,.(x, y)]
                if (thisPoints$x[1] != thisPoints$x[2] & thisPoints$y[1] != thisPoints$y[2])
                {
                    midPoint <- tmpSquare[pointsForRect[j]$side %% 4 + 1]
                    thisPoints <- rbindlist(
                        l = list(thisPoints[1], data.table(x = midPoint$x, y = midPoint$y), thisPoints[2])
                    )
                }
                
                
                thisPolygon <- rbind(
                    thisPoints,
                    data.table(x = point[1], y = point[2])
                )
                thisPolygon[, id := paste0(idS, "_", j)]
                
                # move them away from the center point
                moveMe <- c(
                    mean(thisPoints$x) - point[1],
                    mean(thisPoints$y) - point[2]
                )
                
                rMb <- c(.15, .15)
                
                thisPolygon[, x := sign(moveMe[1]) * runif(1, min(rMb) * abs(moveMe[1]), max(rMb) * abs(moveMe[1])) + x]
                thisPolygon[, y := sign(moveMe[2]) * runif(1, min(rMb) * abs(moveMe[2]), max(rMb) * abs(moveMe[2])) + y]
                return(thisPolygon)
            }
        )
    )
    
    # rotate whole thing by an angle
    # angleRot <- runif(1, min = 0, max = 2 * pi / 3)
    angleRot <- 0
    newBottomLeft <- polygonsFrom[x == min(x)][y == min(y)]
    polygonsFrom[, fromO := calc_dist(x, y, newBottomLeft$x[1], newBottomLeft$y[1])]
    polygonsFrom[, fromFloor := calc_dist(x, y, x, newBottomLeft$y[1])]
    polygonsFrom[, alpha0 := asin(fromFloor / fromO)]
    polygonsFrom[, alphaNew := alpha0 + angleRot]
    polygonsFrom[, c("xNew", "yNew") := list(fromO * cos(alphaNew) + tmpSquare$x[1], fromO * sin(alphaNew) + tmpSquare$y[1])]
    polygonsFrom[is.nan(alpha0), c("xNew", "yNew") := list(x, y)]
    
    return(polygonsFrom)
}

# create a square
usedSeed <- sample(x = c(1:1e5), size = 1)
# usedSeed <- 87141
set.seed(seed = usedSeed)

post <- list(
    c(0, 0, 1), c(0, 1, 1), c(0, 2, 1), c(1, 2, 1), c(2, 2, 1),
    c(1, 0, 2)
)
positions <- rbindlist(
    l = lapply(
        X = 1:length(post), 
        FUN = function(i)
        {
            m <- runif(1, .9, .95)
            mm <- post[[i]][3] * (1 - m) / 2
            data.table(
                x = post[[i]][1] + mm, 
                y = post[[i]][2] + mm, 
                s = post[[i]][3] * m
                )
        }
    )
)


dataRes <- rbindlist(
    l = lapply(
        X = 1:nrow(positions), 
        FUN = function(z)
        {
            create_destroyed_square(
                xy = c(positions$x[z], positions$y[z]),
                side = positions$s[z],
                idS = z
            )
        }
    )
)

# each square get its own opacity
# each piece get its own color
piecesColors <- c("#E1BC4F", "#343564", "#39648D")
toColors <- unique(dataRes[,.(id)])
toColors[, pieceNo := as.numeric(substr(id, regexpr(pattern = "_", text = id) + 1, nchar(id)))]
toColors[, colorPiece := piecesColors[pieceNo %% length(piecesColors) + 1]]
toColors[, mainId := as.numeric(substr(id, 1, regexpr(pattern = "_", text = id) - 1))]
toColors$colorPiece2 <- paste0(toColors$colorPiece, add_alpha(100 - 7 * (toColors$mainId - 1)))

# nie calkowicie random - wieksze kawalkiw powinny byc pozniej
orderPieces <- sample(x = 1:nrow(toColors), size = nrow(toColors), prob = toColors$mainId / sum(toColors$mainId))
ggplot() + 
    lapply(
        X = orderPieces,
        FUN = function(i)
        {
            lId <- toColors$id[i]
            geom_polygon(data = dataRes[id == lId], aes(x = xNew, y = yNew, group = id), 
                         # color = toColors[i]$colorPiece2, 
                         color = "white",
                         fill = toColors[i]$colorPiece2, size = 1)     
        }
    ) +
    theme_void() +
    theme(
        panel.background = element_rect(fill = "#ADC1D1", color = "#ADC1D1"), aspect.ratio = 1
    )
message(usedSeed)

ggsave(filename = "results/2022_05.png", dpi = 720)
file.show("results/2022_05.png")
