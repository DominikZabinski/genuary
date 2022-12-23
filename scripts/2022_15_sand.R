# libraries ----
library(data.table)
library(ggplot2)
library(magick)
library(png)

# functions ----
point_dist <- function(x1, y1, x2, y2)
{
    sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}

dej_mnie <- function(p, pal)
{
    a <- unlist(lapply(X = pal, FUN = function(x) sqrt(sum((p - x) ^ 2))))
    which(a == min(a))[1]
}

pal_to_cielab <- function(pal)
{
    lapply(
        X = pal,
        FUN = function(x)
        {
            a <- rgb_to_xyz(x[1], x[2], x[3])
            a <- xyz_to_cielab(a[1], a[2], a[3])
            return(a)
        }
    )
}

dither_area2 <- function(mat, x0, y0, xend, yend, pal)
{
    ditherWeights <- data.table(
        w = c(7, 3, 5, 1) / 16, 
        c = c(1, -1, 0, 1), 
        r = c(0, 1, 1, 1)
    )
    
    for (i in (x0-1):(xend + 1))
    {
        if (i < 1 | i > ncol(mat)) next()
        for (j in (y0-1):(yend + 1))
        {
            if ((j > nrow(mat)) | (j < 1)) next()
            gg <- tryCatch(expr = {mat[j, i, ]}, error = function(e) message(sprintf("%s-%s", i, j)))
            val <- pal[dej_mnie(gg, pal)][[1]]
            err <- gg - val
            mat[j, i, ] <- val
            for (ii in 1:nrow(ditherWeights))
            {
                resCol <- i + ditherWeights$r[ii]
                resRow <- j + ditherWeights$c[ii]
                if ((resRow <= nrow(mat)) & (resRow > 0) & (resCol <= ncol(mat)) & (resCol > 0))
                {
                    mat[resRow, resCol, ] <- err * ditherWeights$w[ii] + mat[resRow, resCol, ]
                }
            }
        }
    }
    return(mat)    
}

# http://www.easyrgb.com/en/math.php
# https://shihn.ca/posts/2020/dithering/
rgb_to_xyz <- function(r, g, b)
{
    var_R <- r; var_G <- g; var_B <- b
    if ( var_R > 0.04045 )
    {
        var_R = ( ( var_R + 0.055 ) / 1.055 ) ^ 2.4
    } else
    {
        var_R = var_R / 12.92
    }
    if ( var_G > 0.04045 ) 
    {
        var_G = ( ( var_G + 0.055 ) / 1.055 ) ^ 2.4
    } else
    {
        var_G = var_G / 12.92
    }
    if ( var_B > 0.04045 )
    {
        var_B = ( ( var_B + 0.055 ) / 1.055 ) ^ 2.4
    } else
    {
        var_B = var_B / 12.92
    }
    
    var_R = var_R * 100
    var_G = var_G * 100
    var_B = var_B * 100
    
    X = var_R * 0.4124 + var_G * 0.3576 + var_B * 0.1805
    Y = var_R * 0.2126 + var_G * 0.7152 + var_B * 0.0722
    Z = var_R * 0.0193 + var_G * 0.1192 + var_B * 0.9505
    return(c(X, Y, Z))
}

xyz_to_cielab <- function(X, Y, Z)
{
    ReferenceX <- 95.0489
    ReferenceY <- 100
    ReferenceZ <- 108.8840
    var_X = X / ReferenceX
    var_Y = Y / ReferenceY
    var_Z = Z / ReferenceZ
    
    if ( var_X > 0.008856 ) 
    {
        var_X = var_X ^ ( 1/3 )
    } else
    {
        var_X = ( 7.787 * var_X ) + ( 16 / 116 )
    }
    if ( var_Y > 0.008856 )
    {
        var_Y = var_Y ^ ( 1/3 )
    }  else
    {
        var_Y = ( 7.787 * var_Y ) + ( 16 / 116 )
    }
    if ( var_Z > 0.008856 )
    {
        var_Z = var_Z ^ ( 1/3 )
    } else
    {
        var_Z = ( 7.787 * var_Z ) + ( 16 / 116 )
    }
    
    L = ( 116 * var_Y ) - 16
    a = 500 * ( var_X - var_Y )
    b = 200 * ( var_Y - var_Z )
    return(c(L, a, b))
}

cielab_to_xyz <- function(L, a, b)
{
    ReferenceX <- 95.0489
    ReferenceY <- 100
    ReferenceZ <- 108.8840
    
    var_Y = ( L + 16 ) / 116
    var_X = a / 500 + var_Y
    var_Z = var_Y - b / 200
    
    if ( var_Y^3  > 0.008856 ) 
    {
        var_Y = var_Y^3
    } else
    {
        var_Y = ( var_Y - 16 / 116 ) / 7.787
    }
    
    if ( var_X^3  > 0.008856 ) 
    {
        var_X = var_X^3
    } else
    {
        var_X = ( var_X - 16 / 116 ) / 7.787
    }
    if ( var_Z^3  > 0.008856 ) 
    {
        var_Z = var_Z^3
    } else
    {
        var_Z = ( var_Z - 16 / 116 ) / 7.787
    }
    
    X = var_X * ReferenceX
    Y = var_Y * ReferenceY
    Z = var_Z * ReferenceZ
    return(c(X, Y, Z))
}

xyz_to_rgb <- function(X, Y, Z)
{
    var_X = X / 100
    var_Y = Y / 100
    var_Z = Z / 100
    
    var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986
    var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415
    var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570
    
    if ( var_R > 0.0031308 ) 
    {
        var_R = 1.055 * ( var_R ^ ( 1 / 2.4 ) ) - 0.055
    } else
    {
        var_R = 12.92 * var_R
    }
    if ( var_G > 0.0031308 ) 
    {
        var_G = 1.055 * ( var_G ^ ( 1 / 2.4 ) ) - 0.055
    } else
    {
        var_G = 12.92 * var_G
    }
    if ( var_B > 0.0031308 ) 
    {
        var_B = 1.055 * ( var_B ^ ( 1 / 2.4 ) ) - 0.055
    } else
    {
        var_B = 12.92 * var_B
    }
    
    sR = var_R
    sG = var_G
    sB = var_B
    return(c(sR, sG, sB))
}
# variables ----
n <- 1e3
colorsUsed <- c("#F0E6DC", "#F9DDCF", "#E1B275", "#CC907B")
# base plot ----
# waves
ggf <- function(j, kk = 10)
{
    sin(j/kk) + sin(3 * j / kk) / 3 + sin(5 * j / kk) / 5
}
dataM <- data.table(expand.grid(x = 1:n, y = 1:n))

# add lines
set.seed(1234)
div <- runif(n = 1, min = 1/3, max = 2/3)
dataM[y < n * div, h := ggf(x, kk = 10)]
dataM[y >= n * div, h := ggf(y - n/2, kk = 12)]

# add circles
# distance from center is new x
offs <- runif(n = 1, min = .005, max = .01)
r1 <- n/4
dataM[, dist1 := point_dist(x, y, n - r1 - offs * n, n - r1 - offs * n)]
dataM[dist1 < r1, h := ggf(dist1, kk = 12)]

r2 <- (n * sqrt(2) - r1 * (1 + sqrt(2) + offs)) / (1 + sqrt(2)) - offs
dataM[, dist2 := point_dist(x, y, r2 + offs * n, r2 + offs * n)]
dataM[dist2 < r2, h := ggf(dist2, kk = 10)]

pp <- ggplot() + 
    geom_tile(data = dataM, aes(x = x, y = y, color = h, fill = h)) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void() + 
    scale_color_gradientn(colours = colorsUsed) +
    scale_fill_gradientn(colours = colorsUsed) +
    theme(
        aspect.ratio = 1, legend.position = "none",
        plot.background = element_blank() # removes margins (with scale_x/y expand = 0)
    )

ggsave(filename = "D:/github/genuary/results/2022_15.png", width = 2, height = 2, dpi = 720, plot = pp, bg = "red")
file.show("D:/github/genuary/results/2022_15.png")
# dithering ----
# read and conver base image
img <- image_read(path = "D:/github/genuary/results/2022_15.png")
imgGreyScale <- image_convert(image = img, type='colorseparation')
matrixGrey <- as_EBImage(image = imgGreyScale)@.Data

# create matrices - to xyz ...
matrixXYZ <- matrixGrey
for (i in 1:nrow(matrixXYZ))
{
    for (j in 1:nrow(matrixXYZ))
    {
        matrixXYZ[i, j, ] <- rgb_to_xyz(matrixXYZ[i, j, 1], matrixXYZ[i, j, 2], matrixXYZ[i, j, 3]) 
    }
}

# ... and to cielab
matrixCielab <- matrixXYZ
for (i in 1:nrow(matrixCielab))
{
    for (j in 1:nrow(matrixCielab))
    {
        matrixCielab[i, j, ] <- xyz_to_cielab(matrixCielab[i, j, 1], matrixCielab[i, j, 2], matrixCielab[i, j, 3]) 
    }
}

# provide final color pallette
palll <- colorRampPalette(colorsUsed)(4)
palll <- lapply(palll, FUN = function(x) t(col2rgb(x)) / 255)
palll <- pal_to_cielab(palll)

# provide dithering areas
areas <- list(
    a = list(x0 = 1, y0 = 1, xend = 1440, yend = 1440)
)

# dither on cielab matrix ...
for (i in 1:length(areas))
{
    matrixCielab <- dither_area2(mat = matrixCielab, x0 = areas[[i]]$x0, y0 = areas[[i]]$y0, xend = areas[[i]]$xend, yend = areas[[i]]$yend, pal = palll)
}

# ... and convert it to rgb
matrixOut <- matrixCielab
for (i in 1:nrow(matrixOut))
{
    for (j in 1:nrow(matrixOut))
    {
        matrixOut[i, j, ] <- cielab_to_xyz(matrixOut[i, j, 1], matrixOut[i, j, 2], matrixOut[i, j, 3]) 
        matrixOut[i, j, ] <- xyz_to_rgb(matrixOut[i, j, 1], matrixOut[i, j, 2], matrixOut[i, j, 3]) 
    }
}

# save final plot
writePNG(image = matrixOut, target = "D:/github/genuary/results/2022_15_02.png")
file.show("D:/github/genuary/results/2022_15_02.png")
