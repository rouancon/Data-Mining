#Homework 1
#Group 3
#Connor Rouan and Yadukrishnan Sethumadhavan

#-----Problem 1-----
#--Part a-- DONE
par(mfrow = c(2, 2))
plot(forestfires$area ~ forestfires$temp, xlab = "Temperature (C)", ylab = "Area (ha)")
plot(forestfires$area ~ forestfires$month, xlab = "Month", ylab = "Area (ha)")
plot(forestfires$area ~ forestfires$DC, xlab = "DC", ylab = "Area (ha)")
plot(forestfires$area ~ forestfires$RH, xlab = "Relative Humidity (%)", ylab = "Area (ha)")

#--Part b-- DONE
par(mfrow = c(1, 1))
hist(forestfires$wind, xlab = "Wind Speed (km/h)", main="Histogram of Wind Speeds")

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

#--Part e-- help
library(ggplot2)
#df <- forestfires$wind['month'] = forestfires$wind.forestfires$month.round(-1)
ggplot(forestfires$wind,aes(x='wind speed',colour='Month')) + geom_density()

#--Part f-- DONE
plot(forestfires[, c(9, 3, 7, 6)])

#The following datasets appear to be correlated:
#DC and temp
#temp and DMC
#DC and DMC

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


#-----Problem 2-----
#--Part a-- Done
par(mfrow = c(2, 1))
hist(Twitter$friends_count, xlab="Friend Count")
hist(log(Twitter$friends_count), xlab="log(Friend Count)")

#Looking at the histrogram of the data that shows the distribution of friends_count, there is an extremely
#-wide range of values which prevents clear visibility into the distribution of the data.
#For better visibility we looked at the histogram of the logarthmic transformation of the dataset, and
#-it is clear that the data is approx normally distrubted.
#Frequency of friend count is higher around the central mean.

#--Part b-- DONE
library(Hmisc)
summary(Twitter$friends_count)

#--Part c-- DONE
library(Hmisc)
describe(Twitter$friends_count)

#Data quality is good for the friends_count variable, with no missing values however there is an account
#-that has -84 friends which is not possible. Otherwise the data is good considering the quantity of entries.

#--Part d-- DONE
par(mfrow = c(1, 1))
library(scatterplot3d)
scatterplot3d(Twitter$created_at_year, 
              Twitter$education, 
              Twitter$age,
              main="3D Scatter Plot",
              xlab = "Created At (year)",
              ylab = "Education (years)",
              zlab = "Age (years)",
              highlight.3d=TRUE)

#--Part e-- DONE
par(mfrow = c(1, 2))

#Pie chart
slices <- c(650, 1000, 900, 300, 14900) 
lbls <- c("UK", "Canada", "India", "Australia", "US")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Twitter Accounts by Country")

#3D Pie Chart
library(plotrix)
slices <- c(650, 1000, 900, 300, 14900) 
lbls <- c("UK", "Canada", "India", "Australia", "US")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Twitter Accounts by Country")

#--Part f-- DONE
par(mfrow = c(1, 1))
plot(density(Twitter$created_at_year), 
     main="Kernal Density Plot of Twitter Accts Created by Year", 
     xlab = "Year")

#Based on the plot Twitter account created started off more slowly in 2006 and peaked in account creation in 2009.
#Between 2009 and 2014, account creation followed a cyclical pattern of peaking every other year. There was another
#-local peak in 2014 before dropping off significantly after 2014.

#-----Problem 3-----
#--Part a-- DONE
meanA <- mean(raw_data$A)
stdA <- sd(raw_data$A)
meanB <- mean(raw_data$B)
stdB <- sd(raw_data$B)
meanC <- mean(raw_data$C)
stdC <- sd(raw_data$C)
meanD <- mean(raw_data$D)
stdD <- sd(raw_data$D)

scaledA <- scale(raw_data$A, center = meanA, scale = stdA)
scaledB <- scale(raw_data$B, center = meanB, scale = stdB)
scaledC <- scale(raw_data$C, center = meanC, scale = stdC)
scaledD <- scale(raw_data$D, center = meanD, scale = stdD)

Ndata <- NA
Ndata$A <- scaledA
Ndata$B <- scaledB
Ndata$C <- scaledC
Ndata$D <- scaledD

#--Part b-- DONE
par(mfrow = c(1, 4))
boxplot(raw_data$A, xlab = "A")
boxplot(raw_data$B, xlab = "B")
boxplot(raw_data$C, xlab = "C")
boxplot(raw_data$D, xlab = "D")

#--Part c-- DONE
par(mfrow = c(1, 4))
boxplot(Ndata$A, xlab = "Transformed A")
boxplot(Ndata$B, xlab = "Transformed B")
boxplot(Ndata$C, xlab = "Transformed C")
boxplot(Ndata$D, xlab = "Transformed D")

#--Part d-- DONE

#Although the boxplots didn't change themselves (eg range, median, outliers, etc), the range of the axes changed.
#The standardized sets of data are centered around a mean of 0 and their respective standard deviations,
#-meaning they are now directly comparable.

#--Part e-- DONE
par(mfrow = c(1, 1))
plot(raw_data$A ~ raw_data$B, xlab = "A", ylab = "B", main="Plot of B vs A")

#The data do not appear to be correlated.