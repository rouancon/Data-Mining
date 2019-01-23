#Homework 1
#Group 3
#Connor Rouan and Yadukrishnan Sethumadhavan

#-----Problem 1-----
#--Part a--
par(mfrow = c(1, 4))
plot(forestfires$area ~ forestfires$temp, xlab = "Temperature (C)", ylab = "Area (ha)")
plot(forestfires$area ~ forestfires$month, xlab = "Month", ylab = "Area (ha)")
plot(forestfires$area ~ forestfires$DC, xlab = "DC", ylab = "Area (ha)")
plot(forestfires$area ~ forestfires$RH, xlab = "Relative Humidity (%)", ylab = "Area (ha)")

#--Part b-- DONE
par(mfrow = c(1, 1))
hist(forestfires$wind, xlab = "Wind Speed (km/h)")

#--Part c-- DONE
library(Hmisc)
summary(forestfires$wind)

#--Part d-- DONE
hist(forestfires$wind,
     border="black",
     prob = TRUE,
     xlab = "Wind Speed (km/h)",
     main = "Wind Speed Density Distribution")
lines(density(forestfires$wind),
      lwd = 2,
      col = "blue")

#--Part e--
library(sm)
attach(mtcars)
lbl <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)

#--Part f--
plot(forestfires[, c(9, 3, 7, 6)])

#--Part g-- DONE
par(mfrow = c(1, 3))
boxplot(forestfires$wind, xlab = "Wind Speed (km/h)")
boxplot(forestfires$ISI, xlab = "ISI")
boxplot(forestfires$DC, xlab = "DC")

#The Wind Speed boxplot has several upper limit outliers and has a median near the center.
#The ISI boxplot has a small distribution with a median near the center. It has several outliers 
#-near the upper limit as well as a single outlier significantly greater than the upper extreme.
#The DC boxplot has a wide range of extremes with a few lower outliers tightly contained. The median is
#-nearer to the upper quartile rather than in the center as in the other two instances.

#--Part h-- DONE
par(mfrow = c(1, 2))
hist(forestfires$DMC,
     border="black",
     prob = TRUE,
     xlab = "DMC",
     main = "DMC Density Distribution")

#log of DMC
hist(log(forestfires$DMC),
     border="black",
     prob = TRUE,
     xlab = "log(DMC)",
     main = "Logarithmic DMC Density Distribution")

#The logarithmic distribution of the DMC data fits more of an even distribution rather than the raw DMC data.
#There is a stronger central distribution of the data that clearly shows a center point and a much smaller
#-range of total data points along the distribution.

