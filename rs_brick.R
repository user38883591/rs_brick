
library(raster)

setwd("C:/Users/colloh/Desktop/aa/lesson3/data")


image <- raster("C:/Users/colloh/Desktop/aa/lesson3/data/sample_image.tif")

#Creating a raster brick from raster layer
rs_brick <- brick("C:/Users/colloh/Desktop/aa/lesson3/data/sample_image.tif")

#You can now pick individual bands
band1 <- rs_brick$sample_image_1
band2 <- rs_brick$sample_image_2
band3 <- rs_brick$sample_image_3
band4 <- rs_brick$sample_image_4

#Plot brick
plot(rs_brick)

#create true color composite
trueColor <- stack(band3, band2, band1)

#creating a false color composite
falseColor <- stack(band4,band3, band2)

par(mfrow = c(1,2))

#Plotting composite images
plotRGB(trueColor, stretch = "lin", axes = TRUE, main = "True Color Composite")

plotRGB(falseColor, stretch = "lin", axes=TRUE, main= "False Color Composite")

#Calculate NDVI
ndvi <- (band4-band3)/(band4+band3)

#plotting NDVI
plot(ndvi, main = "NDVI1")

# Writing a function for computing normalized difference spectral indices

vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}


#implement vi function

ndvi2 <- vi(rs_brick, 4, 3)

plot(ndvi2, main= "NDVI2")

# Calculate Normalized Difference Green Red Index, where Green is Band 2 and Red is Band 3

ndgr <- vi(rs_brick, 2, 3)


plot(ndvi2, main= "NDVI2")
plot(ndgr, main= "NDGR")

#Reading values from multiple band raster
rsValues <- getValues


summary(rsValues)


#Plotting box plot of band values
par(mfrow= c(1,1))
boxplot(rsValues, main="Band Distribution", col= "lightgrey") 



