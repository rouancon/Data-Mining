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

library(sm)
sm.density.compare(forestfires$month, forestfires$wind, xlab="Wind")
title(main="Wind Distribution by Month")

#1.f

plot(forestfires[, c(9,3,7,6)])


#1.g

par(mfrow = c(1, 3))
boxplot(forestfires$wind)
boxplot(forestfires$ISI)
boxplot(forestfires$DC)

#the wind speed box plot has some upper linit outliers and median near centre quartile.
# The ISI boxplot has severl uppe rlinit outliers and signle outlier below lower limit
# and has  a smaller range of quartiles.
#DC boxplot has a median close to 3rd quartile and has a wide range> it ha s few lower outliers.

#1.f
par(mfrow = c(1, 2))
hist(forestfires$DMC, xlab = "DMC")
hist(log(forestfires$DMC), xlab = "DMC")

# log of DMC is an even dist.


#2

#2.a 
par(mfrow = c(1, 1))
hist(Twitter$friends_count,breaks=100)
hist(log(Twitter$friends_count))

#it looks like the data is normally distributed over a wide range with most data around the mean.

#2.b
library(Hmisc)
summary(Twitter$friends_count)

#2.c
library(Hmisc)
describe(Twitter$friends_count)

# There is no missing data out of the total 21916 entries but one person had -84 freinds whcih is incorrect.

#2.d

library(scatterplot3d)
scatterplot3d(Twitter$created_at_year,Twitter$education,Twitter$age,
              main="3D scatter plot",
              xlab="Created at (years)",
              ylab="Education(years)",
              zlab="Age(years)",
              highlight.3d=TRUE
              )
#2.e

#Pie chart
par(mfrow = c(1, 2))
slices <- c(650, 1000, 900, 300, 14900)
lbls <- c("UK", "Canada", "India" ,"Australia","US")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Twitter account by Countries") 

# 3D Pie Chart
library(plotrix)
slices <- c(650, 1000, 900, 300, 14900)
lbls <- c("UK", "Canada", "India" ,"Australia","US")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels=lbls,
      main="Twitter account by Countries ")

#2.f

par(mfrow = c(1, 1))
plot(density(Twitter$created_at_year), xlab="Created Year", count=1, ylab="Density", main="Kernel density plot of 
created_at_year
     ")

# most twitter accounts were created in 2009

#3
#3.1
library(standardize)
Ndata <- standardize(raw_data)

#3.b
par(mfrow = c(1, 4))
boxplot(raw_data$A, xlab="A")
boxplot(raw_data$B, xlab="B")
boxplot(raw_data$C, xlab="C")
boxplot(raw_data$D, xlab="D")

#3.c
par(mfrow = c(1, 4))
boxplot(Ndata$A, xlab="A")
boxplot(Ndata$B, xlab="B")
boxplot(Ndata$C, xlab="C")
boxplot(Ndata$D, xlab="D")

#3.d

#3.f
par(mfrow = c(1, 1))
plot(raw_data$A~raw_data$B,xlab="A",ylab="B")
