forestfires <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Neu/Sem 2/Data Mining/Dataset/forestfires.csv")


#Question 1
plot(x = forestfires$temp, y = forestfires$area,
             main = "Scatterplot Area vs. Temp",
             xlab = "Temp (c)",
             ylab = "Area (ha)")

plot(x = forestfires$month, y = forestfires$area,
     main = "Scatterplot Area vs. Month",
     xlab = "Month",
     ylab = "Area(ha)")

plot(x = forestfires$DC, y = forestfires$area,
     main = "Scatterplot Area vs. DC",
     xlab = "DC",
     ylab = "Area (ha)")

par(mfrow = c(1, 4))
plot(forestfires$area ~ forestfires$temp, xlab = "Temp(c)", ylab = "Area")
plot(forestfires$area ~ forestfires$month, xlab = "Month", ylab = "Area")
plot(forestfires$area ~ forestfires$DC, xlab = "DC", ylab = "Area")
plot(forestfires$area ~ forestfires$RH, xlab = "RH", ylab = "Area")


#Question 1.b
par(mfrow = c(1, 1))
hist(forestfires$wind, xlab = "wind")

#1.c

install.packages('Hmisc')
library(Hmisc)
summary(forestfires$wind)


#1.d

par(mfrow = c(1, 1))
hist(forestfires$wind, xlab = "wind")
d <- density(forestfires$wind)

hist(forestfires$wind, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "WIND",
     main = "WIND")
lines(density(forestfires$wind), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#1.e

