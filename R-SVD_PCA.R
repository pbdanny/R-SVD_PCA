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

# Start apply SVD ----
i <- svd(img.l)

# Luckily the d (zigma) already sorted
# Calculate % information reserved

newSingularPercent <- function(m, p){
  # Find % cumulative information reserved
  cumInfo <- cumsum(m$d)/sum(m$d)
  # Cut-off for desired % information
  idx <- cumInfo <= p
  # #of rank to be preserved
  rank <- length(idx[idx == TRUE])
  percent = cumInfo[rank]
  cat(sprintf("%% Information reserved : %0.2f \n", percent*100))
  cat(sprintf("No. of singular value reserved 1 .. %d", rank))
  newd <- m$d * idx  # Bit mask vector D with boolean vector idx
  return(list("rank" = rank, "D" = newd))
}

plotSingular <- function(m) {
  par(mfcol = c(2,1))
  plot(x = 1:length(m$d), y = m$d, log = "y", type = "l")
  title(main = "Singular value (log10) : Rank")
  plot(x = 1:length(m$d), y = cumsum(m$d)/sum(m$d), type = "l")
  title(main = "% Cumulative information : Rank" )
}

plotSingular(i)

newD <- newSingularPercent(i, 0.5)

# faster matrix multiplication (trimmed matrix with rank)
new.img <- i$u[, 1:newD$rank] %*% diag(newD$D[1:newD$rank]) %*% t(i$v[, 1:newD$rank])

# This matrix multiplication slower, output same as trimmed matrix with rank
# new.img <- i$u %*% diag(newD$D) %*% t(i$v)

writeJPEG(new.img, target = "tiger_gray_compress.jpg")

# PCA ----
pca.img <- prcomp(img.l, scale = TRUE) 
screeplot(pca.img)
var <- (pca.img$sdev^2) # sdev = std deviation of principal components
plot(x = pca.img$x[, 1], y = pca.img$x[, 2])
