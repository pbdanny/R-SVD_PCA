library(jpeg)
img <- readJPEG("tiger.jpg")

# Split array of RGB to R, G, B array
img.r <- img[, , 1]
img.g <- img[, , 2]
img.b <- img[, , 3]

# Use luminosity method to convert to gray scales
# L = 0.21R + 0.72G + 0.07B
img.l <- (img.r * 0.21) + (img.g * 0.72) + (img.b * 0.07)

# Write out the gray scale .jpg
writeJPEG(img.l, target = "tiger_gray.jpg")

rm(list = c("img.b", "img.g", "img.r", "img"))

# Start apply SVD
i <- svd(img.l)

# Luckily the d (zigma) already sorted
# Calculate % information reserved

newSingularPercent <- function(m, p){
  cumsum <- cumsum(m$d)/sum(m$d)
  idx <- cumsum <= p
  n <- length(idx[idx == TRUE])
  percent = cumsum[n]
  cat(sprintf("%% Information reserved : %0.2f \n", percent*100))
  cat(sprintf("No. of singular value reserved 1 .. %d", n))
  return(newd <- m$d * idx)
}

newD <- newSingularPercent(i, 0.3)

new.img <- i$u %*% diag(newD) %*% t(i$v)

writeJPEG(new.img, target = "tiger_gray_compress.jpg")
